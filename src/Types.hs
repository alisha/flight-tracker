{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Types (
  AirportCode (..),
  Airport,
  Flight,
  Arrival,
) where
import Data.Aeson (FromJSON, ToJSON)
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

data Arrival = Arrival {
  -- arrivalAirportCandidatesCount :: Int ,
  callsign :: String,
  -- departureAirportCandidatesCount :: Int,
  estArrivalAirport :: String,
  -- estArrivalAirportHorizDistance :: Int,
  -- estArrivalAirportVertDistance :: Int,
  estDepartureAirport :: Maybe String,
  -- estDepartureAirportHorizDistance :: Int,
  -- estDepartureAirportVertDistance :: Int,
  -- firstSeen :: Int,
  icao24 :: String
  -- lastSeen :: Int
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

