{-# LANGUAGE BlockArguments #-}
module UI
  ( ui,
  )
where

import qualified Graphics.Vty as V


import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Forms
import Brick.Widgets.Core
import Lens.Micro (Lens', lens, (^.))
import Data.Time.Clock.POSIX
import Brick.Main
import Brick.Types
import Brick.Focus

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Table as BT

import qualified Data.Vector as Vec
import qualified Data.Text as T

import Brick
import Data.Time.Clock
import Requests
import Types
import qualified Arrival as A
import qualified Departure as D
import qualified USA as U
import Control.Monad.Cont (MonadIO(liftIO))

data ArrivalsFields = AirportField | BeginField | EndField
  deriving(Eq, Ord, Show)

type BeginTime = Integer
type EndTime = Integer

data ArrivalsInput = ArrivalsInput {
  _airport :: AirportCode,
  _begin :: BeginTime,
  _end :: EndTime
} deriving Show

data ResourceNames = Arrivals | Departures | Map
  deriving (Eq, Ord, Show)

data AppState = AppState {
  -- brick-specific items
  _focusRing :: FocusRing ResourceNames,

  -- application-specific items
  _arrivalsData :: [A.Arrival],
  _departuresData :: [D.Departure],
  _brickArrivalsData :: L.List ResourceNames A.Arrival,
  _brickDeparturesData :: L.List ResourceNames D.Departure
}

-- These are magic functions needed in the "arrivalsForm" function
airport :: Lens' ArrivalsInput AirportCode
airport = lens _airport (\input newCode -> input { _airport = newCode})
begin :: Lens' ArrivalsInput BeginTime
begin = lens _begin (\input newBegin -> input { _begin = newBegin})
end :: Lens' ArrivalsInput EndTime
end = lens _end (\input newEnd -> input { _end = newEnd})

-- form metadata
arrivalsForm :: ArrivalsInput -> Form ArrivalsInput e ArrivalsFields
arrivalsForm = let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
  in newForm [
    label "Airport" @@= editShowableField airport AirportField,
    label "begin" @@= editShowableField begin BeginField,
    label "end" @@= editShowableField end EndField
  ]

-- function that actually renders the application
draw :: Form ArrivalsInput e ArrivalsFields -> [Widget ArrivalsFields]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body = str "ICAO airport code and begin/end epoch times in seconds"

showAirportCode :: Maybe String -> String
showAirportCode s = case s of
  Just code -> code
  Nothing   -> "????"

parseArrivalData :: A.Arrival -> String
parseArrivalData a =
  showAirportCode (A.estDepartureAirport a) ++ " to " ++ showAirportCode (A.estArrivalAirport a)
  -- ++ ". Departure Time: ")

parseDepartureData :: D.Departure -> String
parseDepartureData d =
  showAirportCode (D.estArrivalAirport d) ++ " to " ++ showAirportCode (D.estDepartureAirport d)

-- renders a mercator projection with the origin and destination
-- coordinates highlighted on the ASCII mercator map
-- renderMercator :: Flight -> Flight -> String
-- renderMercator =

-- converts a lat-lon coordinate to an (x, y) coordinate pair on a map

-- latitude    = 41.145556; // (φ)
-- longitude   = -73.995;   // (λ)

-- mapWidth    = 200;
-- mapHeight   = 100;

-- // get x value
-- x = (longitude+180)*(mapWidth/360)

-- // convert from degrees to radians
-- latRad = latitude*PI/180;

-- // get y value
-- mercN = ln(tan((PI/4)+(latRad/2)));
-- y     = (mapHeight/2)-(mapWidth*mercN/(2*PI));
renderMercatorCoords :: (Double, Double) -> String -> String
renderMercatorCoords (lat, lon) map = replaceCharAtIndex calculatedIdx 'X' map
  where
    (x, y) = convertCoordinateToMapLocation (lat, lon)
    calculatedIdx :: Int
    calculatedIdx =  floor ((y * fromIntegral (U.mapCharWidth + 1)) + x)

convertCoordinateToTotalMapLocation :: (Double, Double) -> (Double, Double)
convertCoordinateToTotalMapLocation (lat, lon) = (x, y)
  where
    x = (lon + 180) * (U.mercatorMapTotalWidth / 360)
    y = (U.mercatorMapTotalHeight / 2) - (U.mercatorMapTotalWidth * mercN / (2 * pi))
    mercN = log (tan((pi / 4) + (latRad / 2)))
    latRad = lat * pi / 180
    mapWidth = U.mercatorMapTotalWidth
    mapHeight = U.mercatorMapTotalHeight

convertCoordinateToMapLocation :: (Double, Double) -> (Double, Double)
convertCoordinateToMapLocation (lat, lon) = (x, y)
  where
    (xTot, yTot) = convertCoordinateToTotalMapLocation (lat, lon)
    x = xTot - U.mercatorMapOriginWidth
    y = yTot - U.mercatorMapOriginHeight

-- replace a character at a particualr index
replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex index replacement str = strHead ++ [replacement] ++ drop 1 strAfter
  where (strHead, strAfter) = splitAt index str


drawResults :: AppState -> [Widget ResourceNames]
drawResults f =
  case focusGetCurrent (_focusRing f) of
    Just Arrivals -> [mainScreen]
    Just Departures -> [mainScreen]
    Nothing -> [mainScreen]
    Just Map -> [mainScreen]
    where
      arrivals = borderWithLabel
        (str $ show "Arrivals")
        $ L.renderList
          (\_ t -> str (T.unpack t))
          (focusGetCurrent (_focusRing f) == Just Arrivals)
          (L.list Arrivals (Vec.fromList (map (T.pack . parseArrivalData) (_arrivalsData f))) 1)
      departures = borderWithLabel
        (str $ show "Departures")
        $ L.renderList
          (\_ t -> str (T.unpack t))
          (focusGetCurrent (_focusRing f) == Just Departures)
          (L.list Departures (Vec.fromList (map (T.pack . parseDepartureData) (_departuresData f))) 1)
      aircraftScreen = C.center $ str U.mercatorMap
      mainScreen = hBox [vBox [arrivals, departures], B.vBorder, aircraftScreen]

-- special attribute map...not yet fully understood
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

-- application function defines event handlers, forms, fields, etc
app :: App (Form ArrivalsInput e ArrivalsFields) e ArrivalsFields
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent V.EvResize {}     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey (V.KChar 'q') [])   -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just AirportField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    continue s'

                    -- Example of external validation:
                    -- Require age field to contain a value that is at least 18.
                    -- continue $ setFieldValid ((formState s')^.age >= 18) AgeField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

renderFlightInfo :: AppState -> EventM n (Next AppState)
renderFlightInfo s =
  case L.listSelectedElement (_brickDeparturesData s) of
    Just (_, departure) -> do
      -- Make API request for aircraft tracking
      tracked <- liftIO $ makeAircraftTrackRequest (D.icao24 departure) 0
      let waypoints = path tracked
      continue s
      -- let latestWaypoint = if length waypoints == 1 then waypoints !! 0
      -- let x = renderMercatorCoords (D.estDepartureAirportHorizDistance departure, D.estDepartureAirportVertDistance departure)
      -- let x = renderMercatorCoords (latitude latestWaypoint, longitude latestWaypoint)
    _ -> continue s

-- TODO: improve this
-- Can add focus case so that you only render flight info when in the arrivals/
-- departure windows
resultsApp :: App AppState e ResourceNames
resultsApp =
  App { appDraw = drawResults,
        appHandleEvent = \s ev ->
          case ev of
            VtyEvent (V.EvKey V.KEnter []) -> renderFlightInfo s
            VtyEvent (V.EvKey V.KEsc [])   -> halt s
            VtyEvent (V.EvKey (V.KChar 'q') [])   -> halt s
            _ -> continue s
          -- where focus = F.focusGetCurrent (s ^. focusRing)
        , appChooseCursor = focusRingCursor _focusRing,
        appStartEvent = return,
        appAttrMap = const theMap
  }

-- actual main routine to start the app
ui :: IO ()
ui = do
  -- default begin/end params
  now <- round `fmap` getPOSIXTime :: IO Integer
  let daySeconds = round nominalDay :: Integer
  let beginTime = now - (daySeconds + fromIntegral (daySeconds `div` 2))
  -- need a default set of params
  let defaultArrivals = ArrivalsInput { _airport = KSAN, _begin = beginTime, _end = now  }
  -- idk....
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  -- arrivals form instance with defaults
  let f = arrivalsForm defaultArrivals
  initialVty <- buildVty

  -- this is where the UI is run and rendered
  f' <- customMain initialVty buildVty Nothing app f

  -- after exiting, we'll get to this part where we print the output of the app
  -- putStrLn "The starting form state was:"
  -- print defaultArrivals

  -- print final form parameters and make the API request and print its response
  -- putStrLn "The final form state was:"
  let params = formState f'
  -- print $ params

  putStrLn "making arrivals request"
  arrivals <- makeArrivalsRequest (_airport params) (_begin params) (_end params)
  putStrLn "making departures request"
  departures <- makeDeparturesRequest (_airport params) (_begin params) (_end params)
  let appState = AppState {
    _focusRing = focusRing [Arrivals],
    _arrivalsData = arrivals,
    _brickArrivalsData = L.list Arrivals (Vec.fromList arrivals) 1,
    _departuresData = departures,
    _brickDeparturesData = L.list Departures (Vec.fromList departures) 1
  }
  putStrLn "created app state"

  initialVty <- buildVty
  putStrLn "created initial vty"
  _ <- customMain initialVty buildVty Nothing resultsApp appState
  putStrLn "Thanks for playing!"



