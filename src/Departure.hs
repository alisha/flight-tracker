{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Departure (
  Departure (..),
) where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Departure = Departure {
  icao24                           :: String,
  firstSeen                        :: Int,
  estDepartureAirport              :: Maybe String,
  lastSeen                         :: Int,
  estArrivalAirport                :: Maybe String,
  callsign                         :: String,
  estDepartureAirportHorizDistance :: Maybe Int,
  estDepartureAirportVertDistance  :: Maybe Int,
  estArrivalAirportHorizDistance   :: Maybe Int,
  estArrivalAirportVertDistance    :: Maybe Int,
  departureAirportCandidatesCount  :: Maybe Int,
  arrivalAirportCandidatesCount    :: Maybe Int
} deriving (Eq, Show, Generic, ToJSON, FromJSON)
