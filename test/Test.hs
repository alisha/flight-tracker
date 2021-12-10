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
prop_total_map_within_bounds = forAll gen_coords_to_pixel_map is_within_bounds

pixelMapLocationBounds :: Score -> TestTree
pixelMapLocationBounds sc = testGroup "Result from convertCoordinateToPixelMapLocation is within map bounds"
    [ scoreProp sc ("prop_total_map_within_bounds", prop_total_map_within_bounds, 1)
    ]