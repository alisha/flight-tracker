{-# LANGUAGE OverloadedStrings #-}

module Requests (
    printAPIRequest,
    makeArrivalsRequest,
    makeDeparturesRequest
) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody), (=:) )
import Data.Text (Text, pack)
import Types

-- TODOs
-- 1) Transform the below function to just make the API request, add functions
-- to parse response
-- 2) Add arguments for desired airport, time, and whether airport is arrival
-- or destination (requires a different API call)

printAPIRequest :: IO ()
printAPIRequest = runReq defaultHttpConfig $ do
    -- https://opensky-network.org/api/flights/arrival?airport=SAN&begin=1517227200&end=1517230800
    -- begin <- getPOSIXTime
    -- end <- begin + (100000000 :: NominalDiffTime )
    let url = https "opensky-network.org" /: "api" /: "flights" /: "arrival"
    -- let params = ("airport" =: ("SAN" :: Text)) <> "begin" =: ("1517227200" :: Text) <> "end" =: ("1517230800" :: Text)
    resp <- req
                GET
                url
                NoReqBody
                jsonResponse
                -- ("airport" =: ("SAN" :: Text))
                ("airport" =: ("KSAN" :: Text) <> "begin" =: ("1637009571" :: Text) <> "end" =: ("1637180971" :: Text))
    liftIO $ print (responseBody resp :: Value)

makeArrivalsRequest :: AirportCode -> Integer -> Integer -> IO [Arrival]
makeArrivalsRequest code begin end = runReq defaultHttpConfig $ do
    let url = https "opensky-network.org" /: "api" /: "flights" /: "arrival"
    resp <- req
                GET
                url
                NoReqBody
                jsonResponse
                ("airport" =: (pack (show code) :: Text) <> "begin" =: (pack (show begin) :: Text) <> "end" =: (pack (show end) :: Text))
    return $ responseBody resp

makeDeparturesRequest :: AirportCode -> Integer -> Integer -> IO [Departure]
makeDeparturesRequest code begin end = runReq defaultHttpConfig $ do
    let url = https "opensky-network.org" /: "api" /: "flights" /: "departure"
    resp <- req
                GET
                url
                NoReqBody
                jsonResponse
                ("airport" =: (pack (show code) :: Text) <> "begin" =: (pack (show begin) :: Text) <> "end" =: (pack (show end) :: Text))
    return $ responseBody resp
