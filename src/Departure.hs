{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Departure (
  Departure (..),
) where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Departure = Departure {
  icao24 :: String,
  firstSeen :: Int,
  estDepartureAirport :: String,
  lastSeen :: Int,
  estArrivalAirport :: String,
  callsign :: String,
  estDepartureAirportHorizDistance :: Int,
  estDepartureAirportVertDistance :: Int,
  estArrivalAirportHorizDistance :: Int,
  estArrivalAirportVertDistance :: Int,
  departureAirportCandidatesCount :: Int,
  arrivalAirportCandidatesCount :: Int
} deriving (Eq, Show, Generic, ToJSON, FromJSON)
