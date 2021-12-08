{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields #-}
module Types (
  AirportCode (..),
  AircraftTrackResponse (..),
  Waypoint (..),
) where
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Array, Number, Null, Bool))
import GHC.Generics
import Data.Vector.Generic ((!))
import Data.Scientific (toRealFloat)

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

data AircraftTrackResponse = AircraftTrackResponse
  {
    icao24 :: String,
    startTime :: Integer,
    endTime :: Integer,
    callsign :: String,
    path :: [Waypoint]
  } deriving (Eq, Generic, ToJSON, FromJSON)

data Waypoint = Waypoint
  { time :: Integer,
    latitude :: Maybe Float,
    longitude :: Maybe Float,
    baro_altitude :: Maybe Float,
    true_track :: Maybe Float,
    on_ground :: Bool
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON Waypoint where
  parseJSON (Array a) = do
    time <- case a ! 0 of
      Number b -> return $ floor b
      _ -> fail "time must be number"
    lat <- case a ! 1 of
      Number b -> return $ Just (toRealFloat b)
      _ -> fail "latitude must be number"
    lon <- case a ! 2 of
      Number b -> return $ Just (toRealFloat b)
      _ -> fail "longitude must be number"
    baro_altitude <- case a ! 3 of
      Number b -> return $ Just (toRealFloat b)
      _ -> fail "baro altitude must be number"
    true_track <- case a ! 4 of
      Number b -> return $ Just (toRealFloat b)
      Null -> return Nothing
      _ -> fail "true track must be number"
    on_ground <- case a ! 5 of
      (Bool b) -> return b
      _ -> fail "on ground must be bool"

    return $ Waypoint time lat lon baro_altitude true_track on_ground
  parseJSON _ = do fail "waypoint must be array"

