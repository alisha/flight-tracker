{-# LANGUAGE BlockArguments #-}
module UI
  ( ui,
  convertCoordinateToPixelMapLocation,
  showAirportCode,
  unixTimeToLocal
  )
where

import qualified Graphics.Vty as V


import Brick.Forms
import Brick.Widgets.Core
import Lens.Micro (Lens', lens, (^.), (&), (.~))
import Data.Time.Format
import Data.Time.LocalTime
import Text.Printf
import Data.Time.Clock.POSIX
import Brick.Main
import Brick.Types
import Brick.Focus

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L

import qualified Data.Vector as Vec

import Brick
import Requests
import Types
import qualified Arrival as A
import qualified Departure as D
import qualified USA as U
import Control.Monad.Cont (MonadIO(liftIO))
import qualified Data.Maybe
import Data.List.Utils (replace)
import Brick.Widgets.Border (borderWithLabel)
import Data.Text (splitOn, pack, unpack)
import Graphics.Vty.Image (vertCat)
import Graphics.Vty.Attributes
    ( withBackColor, yellow, red, defAttr )
import Data.List (intersperse)
import Graphics.Vty ( horizCat, withForeColor, string )

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
  _brickDeparturesData :: L.List ResourceNames D.Departure,
  _selectedFlight :: Maybe AircraftTrackResponse
}

-- These are magic functions needed in the "arrivalsForm" function
airport :: Lens' ArrivalsInput AirportCode
airport = lens _airport (\input newCode -> input { _airport = newCode})
begin :: Lens' ArrivalsInput BeginTime
begin = lens _begin (\input newBegin -> input { _begin = newBegin})
end :: Lens' ArrivalsInput EndTime
end = lens _end (\input newEnd -> input { _end = newEnd})

focusRingLens :: Lens' AppState (FocusRing ResourceNames)
focusRingLens = lens _focusRing (\input newFocusRing -> input { _focusRing = newFocusRing })
brickArrivalsData :: Lens' AppState (L.List ResourceNames A.Arrival)
brickArrivalsData = lens _brickArrivalsData (\input newList -> input { _brickArrivalsData = newList })
brickDeparturesData :: Lens' AppState (L.List ResourceNames D.Departure)
brickDeparturesData = lens _brickDeparturesData (\input newList -> input { _brickDeparturesData = newList })
selectedFlight :: Lens' AppState (Maybe AircraftTrackResponse)
selectedFlight = lens _selectedFlight (\input newSelectedFlight -> input { _selectedFlight = newSelectedFlight})

-- form metadata
arrivalsForm :: ArrivalsInput -> Form ArrivalsInput e ArrivalsFields
arrivalsForm = let label s w = padBottom (Pad 1) $ vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
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
showAirportCode = Data.Maybe.fromMaybe "????"

unixTimeToLocal :: Int -> String
unixTimeToLocal t = do
  let localtime = utcToLocalTime (read "PDT") (posixSecondsToUTCTime (fromIntegral t))
  formatTime defaultTimeLocale "%I:%M %p" localtime

instance Show AircraftTrackResponse where
  show f = "\
            \ICAO24: "     ++ icao24 f           ++ "\n\
            \start time: " ++ unixTimeToLocal (fromIntegral (startTime f)) ++ "\n\
            \end time: "   ++ unixTimeToLocal (fromIntegral (endTime f))   ++ "\n\
            \callsign: "   ++ callsign f         ++ "\n\
            \"

parseArrivalData :: A.Arrival -> String
parseArrivalData a =
  printf "%s (%s) to %s (%s)" dptAirport dptTime arrAirport arrTime
  where
    dptAirport = showAirportCode (A.estDepartureAirport a)
    dptTime = unixTimeToLocal (A.firstSeen a)
    arrAirport = showAirportCode (A.estArrivalAirport a)
    arrTime = unixTimeToLocal (A.lastSeen a)

parseDepartureData :: D.Departure -> String
parseDepartureData d =
  printf "%s (%s) to %s (%s)" dptAirport dptTime arrAirport arrTime
  where
    dptAirport = showAirportCode (D.estDepartureAirport d)
    dptTime = unixTimeToLocal (D.firstSeen d)
    arrAirport = showAirportCode (D.estArrivalAirport d)
    arrTime = unixTimeToLocal (D.lastSeen d)

-- ansiReplacement = "\ESC[31;43mX\ESC[39;49m"
ansiReplacement = "X"

-- renders the map -- uses the ANSI escape sequence for red FG and yellow BG
-- to replace the waypoint locations to make them more visible
-- it is important to first plot the 'X's on the map before inserting the ANSI
-- codes or else it would not plot the waypoints in the proper location.
renderMap :: [Waypoint] -> String
renderMap points = replace "X" ansiReplacement folded
  where
    folded = foldr func base coords
    filterFunc point = case (latitude point, longitude point) of
      (Just _, Just _) -> True
      _ -> False
    filteredPoints = filter filterFunc points
    mapFunc pt = case (latitude pt, longitude pt) of
      (Just x, Just y) -> (x, y)
      _ -> (0, 0)
    coords = map mapFunc filteredPoints
    base = U.mercatorMap
    func coords map = renderMercatorCoords coords map


renderMercatorCoords :: (Float, Float) -> String -> String
renderMercatorCoords (lat, lon) = replaceCharAtIndex calculatedIdx 'X'
  where
    -- at this point, we have the coordinate pair properly as it would be rendered
    -- for a pixel-based image, but we have an ASCII based one.
    -- so we need to perform one more coordinate transform to go from pixel coordinate
    -- to ascii map coordinates.
    (x, y) = U.convertPixelToAsciiCoordinates (convertCoordinateToMapLocation (lat, lon))
    calculatedIdx :: Int
    calculatedIdx =  (y * fromIntegral (U.mapCharWidth + 1)) + x

-- renders a mercator projection with the origin and destination
-- coordinates highlighted on the ASCII mercator map
-- renderMercator :: Flight -> Flight -> String
-- renderMercator =

-- converts a lat-lon coordinate to an (x, y) coordinate pair on a map

-- latitude    = 41.145556; // (φ)
-- longitude   = -73.995;   // (λ)

-- mapWidth    = 200;
-- mapHeight   = 100;

-- x = (longitude+180)*(mapWidth/360)

-- // convert from degrees to radians
-- latRad = latitude*PI/180;

-- // get y value
-- mercN = ln(tan((PI/4)+(latRad/2)));
-- y     = (mapHeight/2)-(mapWidth*mercN/(2*PI));
convertCoordinateToPixelMapLocation :: (Float, Float) -> (Float, Float)
convertCoordinateToPixelMapLocation (lat, lon) = (x, y)
  where
    x = (lon + 180) * (U.mercatorMapPixelTotalWidth / 360)
    y = (U.mercatorMapPixelTotalHeight / 2) - (U.mercatorMapPixelTotalWidth * mercN / (2 * pi))
    mercN = log (tan((pi / 4) + (latRad / 2)))
    latRad = lat * pi / 180

convertCoordinateToMapLocation :: (Float, Float) -> (Float, Float)
convertCoordinateToMapLocation (lat, lon) = (x, y)
  where
    (xTot, yTot) = convertCoordinateToPixelMapLocation (lat, lon)
    x = xTot - U.mercatorMapPixelOriginWidth
    y = yTot - U.mercatorMapPixelOriginHeight

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
        (str "Arrivals")
        $ L.renderList
          (\_ t -> str (parseArrivalData t))
          (focusGetCurrent (_focusRing f) == Just Arrivals)
          (_brickArrivalsData f)
      departures = borderWithLabel
        (str "Departures")
        $ L.renderList
          (\_ t -> str (parseDepartureData t))
          (focusGetCurrent (_focusRing f) == Just Departures)
          (_brickDeparturesData f)
      aircraftScreen = hLimit 121 $ vLimit 24 $  borderWithLabel (str "Map" ) $ raw rawVtyImage
        where
          rawStringMap = case _selectedFlight f of
            Nothing -> U.mercatorMap
            Just selected -> renderMap (path selected)
          splitMap = splitOn "\n" (pack rawStringMap)
          ansiCodedLines = map horizImg splitMap
          horizImg line = horizCat joined
            where
              splits = splitOn (pack ansiReplacement) line
              splitVtyStr = map (string defAttr . unpack) splits
              joined = intersperse (string ((defAttr `withBackColor` yellow) `withForeColor` red) ansiReplacement) splitVtyStr
          rawVtyImage = vertCat ansiCodedLines
      aircraftResponse = maybe "No Flight Selected" show (_selectedFlight f)
      mainScreen = hBox [vBox [hLimit 37 arrivals, hLimit 37 departures], hLimit 23 (C.center $ str aircraftResponse), C.center aircraftScreen]

-- special attribute map...not yet fully understood
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (L.listSelectedAttr, V.black `on` V.brightBlack)
  , (L.listSelectedFocusedAttr, V.blue `on` V.white)
  ]

-- application function defines event handlers, forms, fields, etc
app :: App (Form ArrivalsInput e ArrivalsFields) e ArrivalsFields
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent V.EvResize {}     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey (V.KChar 'q') [])  -> halt s
                -- Enter quits only when we aren't in the multi-line editor.
                VtyEvent (V.EvKey V.KEnter [])
                    | focusGetCurrent (formFocus s) /= Just AirportField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    continue s'
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

getSelectedFlightTrack :: AppState -> IO (Maybe AircraftTrackResponse)
getSelectedFlightTrack s = do
  case focusGetCurrent (_focusRing s) of
    Just Arrivals -> do
      case L.listSelectedElement (_brickArrivalsData s) of
        Just (_, flight) -> do
          x <- makeAircraftTrackRequest (A.icao24 flight) (fromIntegral (A.firstSeen flight))
          return (Just x)
        Nothing -> return Nothing
    Just Departures -> do
      case L.listSelectedElement (_brickDeparturesData s) of
        Just (_, flight) -> do
          x <- makeAircraftTrackRequest (D.icao24 flight) (fromIntegral (D.firstSeen flight))
          return (Just x)
        Nothing -> return Nothing
    _ -> return Nothing

renderFlightInfo :: AppState -> EventM n (Next AppState)
renderFlightInfo s = do
  selectedFlightResponse <- liftIO $ getSelectedFlightTrack s
  continue (s & (selectedFlight .~ selectedFlightResponse))

handleArrivalsEvent :: AppState -> V.Event -> EventM ResourceNames (Next AppState)
handleArrivalsEvent s e = do
  newArrivals <- L.handleListEvent e (_brickArrivalsData s)
  let newState = s & brickArrivalsData .~ newArrivals
  continue newState

handleDeparturesEvent :: AppState -> V.Event -> EventM ResourceNames (Next AppState)
handleDeparturesEvent s e = do
  newDepartures <- L.handleListEvent e (s ^. brickDeparturesData)
  continue (s & (brickDeparturesData .~ newDepartures))

resultsApp :: App AppState e ResourceNames
resultsApp =
  App { appDraw = drawResults,
        appHandleEvent = \s ev ->
          let focus = focusGetCurrent (_focusRing s)
          in
          case ev of
            VtyEvent (V.EvKey V.KEnter []) -> renderFlightInfo s
            VtyEvent (V.EvKey V.KEsc [])   -> halt s
            VtyEvent (V.EvKey (V.KChar 'q') [])   -> halt s
            VtyEvent (V.EvKey (V.KChar 'r') [])  -> do
                  let newFlight = Nothing
                  let s' = s & (selectedFlight .~ newFlight)
                  continue s'
            VtyEvent (V.EvKey (V.KChar 'a') [])  -> do
              let f' = focusSetCurrent Arrivals (_focusRing s)
              continue (s & focusRingLens .~ f')
            VtyEvent (V.EvKey (V.KChar 'd') [])  -> do
              let f' = focusSetCurrent Departures (_focusRing s)
              continue (s & focusRingLens .~ f')
            _ ->
              case (focus, ev) of
                (Just Arrivals, VtyEvent e) ->
                  handleArrivalsEvent s e
                (Just Departures, VtyEvent e) ->
                  handleDeparturesEvent s e
                _ -> continue s
        , appChooseCursor = focusRingCursor _focusRing,
        appStartEvent = pure,
        appAttrMap = const theMap
  }

-- actual main routine to start the app
ui :: IO ()
ui = do
  -- default begin/end params
  ct <- getCurrentTime
  let now = round (toRational (utcTimeToPOSIXSeconds ct))
  let daySeconds = 60 * 60 * 24
  let hourSeconds = 60 * 60
  let beginTime = now - daySeconds
  let endTime = beginTime + (6 * hourSeconds)
  -- default set of params
  let defaultArrivals = ArrivalsInput { _airport = KSAN, _begin = beginTime, _end = endTime  }
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

  -- arrivals form instance with defaults
  let f = arrivalsForm defaultArrivals
  initialVty <- buildVty

  -- this is where the UI is run and rendered
  f' <- customMain initialVty buildVty Nothing app f
  let params = formState f'

  putStrLn "making arrivals request"
  arrivals <- makeArrivalsRequest (_airport params) (_begin params) (_end params)
  putStrLn "making departures request"
  departures <- makeDeparturesRequest (_airport params) (_begin params) (_end params)

  let appState = AppState {
    _focusRing = focusRing [Arrivals, Departures],
    _arrivalsData = arrivals,
    _brickArrivalsData = L.list Arrivals (Vec.fromList arrivals) 1,
    _departuresData = departures,
    _brickDeparturesData = L.list Departures (Vec.fromList departures) 1,
    _selectedFlight = Nothing
  }

  initialVty <- buildVty
  putStrLn "created initial vty"
  _ <- customMain initialVty buildVty Nothing resultsApp appState
  putStrLn "Thanks for playing!"
