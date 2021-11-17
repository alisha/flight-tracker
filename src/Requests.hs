{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

-- TODOs
-- 1) Transform the below function to just make the API request, add functions
-- to parse response
-- 2) Add arguments for desired airport, time, and whether airport is arrival
-- or destination (requires a different API call)

printAPIRequest :: IO()
printAPIRequest = runReq defaultHttpConfig $ do
    -- https://opensky-network.org/api/flights/arrival?airport=SAN&begin=1517227200&end=1517230800
    let url = https "opensky-network.org" /: "api" /: "flights" /: "arrival"
    resp <- req GET url NoReqBody jsonResponse mempty $
        "airport" =: "SAN" <>
        "begin" =: "1517227200" <>
        "end" =: "1517230800"
    liftIO $ print (responseBody resp :: Value)