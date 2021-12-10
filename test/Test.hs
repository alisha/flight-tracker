{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Prelude hiding (maximum)
import Test.QuickCheck
import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit
import GHC.IO
import GHC.IO.Exception

import qualified Requests as R
import qualified UI as UI
import qualified USA as U
import qualified Types as T
import qualified Arrival as A
import qualified Departure as D

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
  [ pixelMapLocationBounds
  ]

--------------------------------------------------------------------------------
-- Map functions
--------------------------------------------------------------------------------

is_within_bounds :: (Float, Float) -> Bool
is_within_bounds (x, y) = x >= 0 && x < U.mercatorMapPixelTotalWidth && y >= 0 && y < U.mercatorMapPixelTotalHeight

gen_coordinates :: Gen (Float, Float)
gen_coordinates = do
  x <- choose (-90, 90)
  y <- choose (-180, 180)
  return (x, y)

gen_coords_to_pixel_map :: Gen (Float, Float)
gen_coords_to_pixel_map = do
  coords <- gen_coordinates
  return (UI.convertCoordinateToPixelMapLocation coords)

prop_total_map_within_bounds :: Property
prop_total_map_within_bounds = forAll gen_coords_to_pixel_map isWithinBounds

pixelMapLocationBounds :: Score -> TestTree
pixelMapLocationBounds sc = testGroup "Result from convertCoordinateToPixelMapLocation is within map bounds"
    [ scoreProp sc ("prop_total_map_within_bounds", prop_total_map_within_bounds, 1)
    ]

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

showAirportCodeTests :: Score -> TestTree
showAirportCodeTests sc = testGroup "showAirportCode"
  [ scoreTest ((\_ -> UI.showAirportCode (Just "KSAN")), (), "KSAN", 1, "showAirportCode-1")
  , scoreTest ((\_ -> UI.showAirportCode Nothing), (), "????", 1, "showAirportCode-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

unixTimeToLocalTests :: Score -> TestTree
unixTimeToLocalTests sc = testGroup "unixTimeToLocal"
  [ scoreTest ((\_ -> UI.unixTimeToLocal 0), (), "05:00 PM", 1, "test-1")
  -- Note: all times are in PDT (not PST) so below is 10:47 and not 9:47
  , scoreTest ((\_ -> UI.unixTimeToLocal 1639158447), (), "10:47 AM", 1, "test-2")
  , scoreTest ((\_ -> UI.unixTimeToLocal 1023746521), (), "03:02 PM", 1, "test-3")
  , scoreTest ((\_ -> UI.unixTimeToLocal 1653836400), (), "08:00 AM", 1, "test-4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

replaceCharAtIndexTests :: Score -> TestTree
replaceCharAtIndexTests sc = testGroup "replaceCharAtIndex"
  [ scoreTest ((\_ -> UI.replaceCharAtIndex 3 '!' "hello, world"), (), "hel!o, world", 1, "replaceCharAtIndex-1")
  , scoreTest ((\_ -> UI.replaceCharAtIndex 3 'a' ""), (), "", 1, "replaceCharAtIndex-2")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

--------------------------------------------------------------------------------
-- Parsing functions
--------------------------------------------------------------------------------

-- Sample waypoints for testing
-- [1639014758,40.6984,-74.1648,0,25,false],[1639014765,40.7034,-74.1616,0,26,false]