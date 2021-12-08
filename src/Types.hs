{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields #-}
module Types (
  AirportCode (..),
  Airport,
  Flight,
  AircraftTrackResponse (..),
  Waypoint (..),
) where
import Data.Aeson (FromJSON, ToJSON, Array)
import GHC.Generics

-- Simply the ICAO codenames
-- non-exhaustive list
data AirportCode
  = KSAN
  | KSFO
  | KSEA
  | KPHL
  | KEWR
  | KBOS
  | KORD
  | KDCA
  | KLGA
  | KMIA
  | KJFK
  | KLAX
  | KATL
  | KACY
  | KAUS
  | KBWI
  | KSLC
  | KSJC
  deriving (Eq, Show, Enum, Bounded, Read)

-- Airport includes geographic lat/lon coordinates
data Airport = Airport
  { code :: AirportCode,
    lat :: Double,
    lon :: Double
  }

-- custom show impl on airport to display on UI
instance Show Airport where
  show (Airport code _ _) = show code

data Flight = Flight
  { origin :: Airport,
    destination :: Airport
  }

instance Show Flight where
  show (Flight origin destination) = "flying " ++ show origin ++ " to " ++ show destination


data AircraftTrackResponse = AircraftTrackResponse
  {
    icao24 :: String,
    startTime :: Integer,
    endTime :: Integer,
    callsign :: String,
    path :: Array
  } deriving (Eq, Generic, ToJSON, FromJSON)

data Waypoint = Waypoint
  { time :: Integer,
    latitude :: Maybe Float,
    longitude :: Maybe Float,
    baro_altitude :: Maybe Float,
    true_track :: Maybe Float,
    on_ground :: Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


instance Show AircraftTrackResponse where
  show f = "\
            \ICAO24: "     ++ icao24 f           ++ "\n\
            \start time: " ++ show (startTime f) ++ "\n\
            \end time: "   ++ show (endTime f)   ++ "\n\
            \callsign: "   ++ callsign f         ++ "\n\
            \"
