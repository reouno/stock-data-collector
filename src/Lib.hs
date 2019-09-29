{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( sampleRequest
  ) where

import           Data.Aeson
import           Network.HTTP.Simple

rapidAPIYahooFinanceSummaryEndpoint =
  "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-summary"

sampleRequest :: IO ()
sampleRequest = do
  initReq <- parseRequest rapidAPIYahooFinanceSummaryEndpoint
  let headers =
        [ ("x-rapidapi-host", "apidojo-yahoo-finance-v1.p.rapidapi.com")
        , ( "x-rapidapi-key"
          , "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a")
        ]
      req = setRequestHeaders headers initReq
  res <- httpLBS req
  --print res
  putStrLn "\nReponse status:"
  print $ getResponseStatus res
  putStrLn "\nResponse headers:"
  print $ getResponseHeaders res
  putStrLn "\nResponse body:"
  print $ getResponseBody res
