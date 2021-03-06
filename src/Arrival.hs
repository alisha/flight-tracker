{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Arrival (
  Arrival (..),
) where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics




data Arrival = Arrival {
  -- arrivalAirportCandidatesCount :: Int ,
  callsign :: Maybe String,
  -- departureAirportCandidatesCount :: Int,
  estArrivalAirport :: Maybe String,
  -- estArrivalAirportHorizDistance :: Int,
  -- estArrivalAirportVertDistance :: Int,
  estDepartureAirport :: Maybe String,
  -- estDepartureAirportHorizDistance :: Int,
  -- estDepartureAirportVertDistance :: Int,
  firstSeen :: Int,
  icao24 :: String,
  lastSeen :: Int
} deriving (Eq, Show, Generic, ToJSON, FromJSON)
