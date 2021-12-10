{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (maximum)
import Test.QuickCheck
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit
import GHC.IO
import GHC.IO.Exception

import qualified Requests as R
import qualified UI
import qualified USA as U
import qualified Types as T
import qualified Arrival as A
import qualified Departure as D
import qualified Data.Aeson as DA
import Data.Vector (fromList)
import qualified Data.Aeson.Types as DA
import Debug.Trace (traceShowId)
import Types (Waypoint(Waypoint))

-- Helper functions from CSE 230 homework code
type Score = IORef (Int, Int)

runTests :: [Score -> TestTree] -> IO ()
runTests groups = do
  sc <- initScore
  -- defaultMain (tests sc groups) `catch` (\(e :: ExitCode) -> do
  defaultMain (localOption (mkTimeout 1000000) (tests sc groups)) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- readIORef sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> [Score -> TestTree] -> TestTree
tests x gs = testGroup "Tests" [ g x | g <- gs ]
--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> IO b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    actR <- f x
    if actR == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)
--------------------------------------------------------------------------------
scoreProp :: (Testable prop) => Score -> (String, prop, Int) -> TestTree
--------------------------------------------------------------------------------
scoreProp sc (name, prop, n) = scoreTest' sc (act, (), True, n, name)
  where
    act _                    = isSuccess <$> labelledExamplesWithResult args prop
    args                     = stdArgs { chatty = False, maxSuccess = 100 }

--------------------------------------------------------------------------------
-- Project tests
--------------------------------------------------------------------------------

main :: IO ()
main = runTests
  [ pixelMapLocationBounds,
    showAirportCodeTests,
    unixTimeToLocalTests,
    replaceCharAtIndexTests,
    waypointParsing
  ]

--------------------------------------------------------------------------------
-- Map functions
--------------------------------------------------------------------------------

isWithinBounds :: (Float, Float) -> Bool
isWithinBounds (x, y) = x >= 0 && x < U.mercatorMapPixelTotalWidth && y >= 0 && y < U.mercatorMapPixelTotalHeight

genCoordinates :: Gen (Float, Float)
genCoordinates = do
  lat <- choose (-85, 85)
  lon <- choose (-175, 175)
  return (lat, lon)

genCoordsToPixelMap :: Gen (Float, Float)
genCoordsToPixelMap = do
  UI.convertCoordinateToPixelMapLocation <$> genCoordinates

propTotalMapWithinBounds :: Property
propTotalMapWithinBounds = forAll genCoordsToPixelMap isWithinBounds

pixelMapLocationBounds :: Score -> TestTree
pixelMapLocationBounds sc = testGroup "Result from convertCoordinateToPixelMapLocation is within map bounds"
    [ scoreProp sc ("propTotalMapWithinBounds", propTotalMapWithinBounds, 1)
    ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

showAirportCodeTests :: Score -> TestTree
showAirportCodeTests sc = testGroup "showAirportCode"
  [ scoreTest (\_ -> UI.showAirportCode (Just "KSAN"), (), "KSAN", 1, "showAirportCode-1")
  , scoreTest (\_ -> UI.showAirportCode Nothing, (), "????", 1, "showAirportCode-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

unixTimeToLocalTests :: Score -> TestTree
unixTimeToLocalTests sc = testGroup "unixTimeToLocal"
  [ scoreTest (\_ -> UI.unixTimeToLocal 0, (), "05:00 PM", 1, "test-1")
  -- Note: all times are in PDT (not PST) so below is 10:47 and not 9:47
  , scoreTest (\_ -> UI.unixTimeToLocal 1639158447, (), "10:47 AM", 1, "test-2")
  , scoreTest (\_ -> UI.unixTimeToLocal 1023746521, (), "03:02 PM", 1, "test-3")
  , scoreTest (\_ -> UI.unixTimeToLocal 1653836400, (), "08:00 AM", 1, "test-4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

replaceCharAtIndexTests :: Score -> TestTree
replaceCharAtIndexTests sc = testGroup "replaceCharAtIndex"
  [ scoreTest (\_ -> UI.replaceCharAtIndex 3 '!' "hello, world", (), "hel!o, world", 1, "replaceCharAtIndex-1")
  , scoreTest (\_ -> UI.replaceCharAtIndex 3 'a' "", (), "", 1, "replaceCharAtIndex-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

--------------------------------------------------------------------------------
-- Parsing functions
--------------------------------------------------------------------------------

-- Sample waypoints for testing
waypointParsing :: Score -> TestTree
waypointParsing sc = testGroup "waypointParsing" [
    scoreTest (\_ -> doParse samp1, (), Just samp1Result, 1, "simple-success")
  , scoreTest (\_ -> doParse samp2, (), Nothing, 1, "fail-missing-time")
  , scoreTest (\_ -> doParse samp3, (), Just samp3Result, 1, "pass-null-lat")
  , scoreTest (\_ -> doParse samp4, (), Just samp4Result, 1, "pass-null-lon")
  , scoreTest (\_ -> doParse samp5, (), Just samp5Result, 1, "pass-null-baro")
  , scoreTest (\_ -> doParse samp6, (), Just samp6Result, 1, "pass-null-track")
  , scoreTest (\_ -> doParse samp7, (), Nothing, 1, "fail-lat-wrong-type")
  , scoreTest (\_ -> doParse samp8, (), Nothing, 1, "fail-lon-wrong-type")
  , scoreTest (\_ -> doParse samp9, (), Nothing, 1, "fail-baro-wrong-type")
  , scoreTest (\_ -> doParse samp10, (), Nothing, 1, "fail-tracl-wrong-type")
  ]
    where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
    doParse :: [DA.Value] -> Maybe Waypoint
    doParse a = do DA.parseMaybe DA.parseJSON (DA.Array (fromList a))
    samp1 = [DA.Number 1639014758, DA.Number 40.6984, DA.Number (-74.1648), DA.Number 0, DA.Number 25, DA.Bool False]
    samp1Result = T.Waypoint 1639014758 (Just 40.6984) (Just (-74.1648)) (Just 0.0) (Just 25.0) False
    samp2 = [DA.Null , DA.Number 40.6984, DA.Number (-74.1648), DA.Number 0, DA.Number 25, DA.Bool False]
    samp3 = [DA.Number 1639014758, DA.Null, DA.Number (-74.1648), DA.Number 0, DA.Number 25, DA.Bool False]
    samp3Result = T.Waypoint 1639014758 Nothing (Just (-74.1648)) (Just 0.0) (Just 25.0) False
    samp4 = [DA.Number 1639014758, DA.Null, DA.Null , DA.Number 0, DA.Number 25, DA.Bool False]
    samp4Result = T.Waypoint 1639014758 Nothing Nothing (Just 0.0) (Just 25.0) False
    samp5 = [DA.Number 1639014758, DA.Null, DA.Null , DA.Null, DA.Number 25, DA.Bool False]
    samp5Result = T.Waypoint 1639014758 Nothing Nothing Nothing (Just 25.0) False
    samp6 = [DA.Number 1639014758, DA.Null, DA.Null , DA.Null, DA.Null , DA.Bool False]
    samp6Result = T.Waypoint 1639014758 Nothing Nothing Nothing Nothing False
    samp7 = [DA.Number 1639014758, DA.Bool False, DA.Null , DA.Null, DA.Null , DA.Bool False]
    samp8 = [DA.Number 1639014758, DA.Null, DA.Bool False , DA.Null, DA.Null , DA.Bool False]
    samp9 = [DA.Number 1639014758, DA.Null, DA.Null , DA.Bool False, DA.Null , DA.Bool False]
    samp10 = [DA.Number 1639014758, DA.Null, DA.Null , DA.Null, DA.Bool False , DA.Bool False]




