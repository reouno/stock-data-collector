{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module App
  ( app
  , sampleRequest
  , getDJIDataOfLastDay
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Reflection                              ( Given, given )
import           Data.Text.Encoding                           ( encodeUtf8 )
import           Network.HTTP.Simple

import           Config.Config                                ( Config (..) )
import           Entity.Stock                                 ( StockTerm (..) )
import           InterfaceAdapter.RapidAPIYahooFinanceFetcher ( YahooFinanceFetcher (..) )
import           Usecase.Interactor.FetchData                 ( fetchStockData )

app :: Given Config => IO ()
app = do
  let config = given :: Config
      fetcher = YahooFinanceFetcher $ (encodeUtf8 . xRapidapiKey) config
  stocks <- fetchStockData fetcher "AAPL" ("5D" :: String)
  print stocks

rapidAPIYahooFinanceSummaryEndpoint =
  "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-summary"

sampleRequest :: Given Config => IO ()
sampleRequest = do
  let config = given :: Config
  initReq <- parseRequest $ rapidapiYahooFinanceSummaryEndpoint config
  let headers =
        [ ("x-rapidapi-host", encodeUtf8 $ xRapidapiHost config)
        , ("x-rapidapi-key", encodeUtf8 $ xRapidapiKey config)
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

--getDJIDataOfLastDay :: IO ()
getDJIDataOfLastDay = do
  initReq <-
    parseRequest
      "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-charts"
  let headers =
        [ ("x-rapidapi-host", "apidojo-yahoo-finance-v1.p.rapidapi.com")
        , ( "x-rapidapi-key"
          , "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a")
        ]
      reqWithHeader = setRequestHeaders headers initReq
      queryParams =
        [ ("region", Just "US")
        , ("lang", Just "en")
        , ("symbol", Just "^DJI")
        , ("interval", Just "1d")
        , ("range", Just "1d")
        ]
      req = setRequestQueryString queryParams reqWithHeader
  res <- httpLBS req
  --print res
  putStrLn "\nReponse status:"
  print $ getResponseStatus res
  putStrLn "\nResponse headers:"
  print $ getResponseHeaders res
  putStrLn "\nResponse body:"
  print $ getResponseBody res
  -- use Lens
  putStrLn "Get JSON Value of open, close, high, and low prices"
  print
    (getResponseBody res ^? key "chart" . key "result" . nth 0 .
     key "indicators" .
     key "quote" .
     nth 0 :: Maybe Value)
  putStrLn "Get a close price"
  print
    (getResponseBody res ^? key "chart" . key "result" . nth 0 .
     key "indicators" .
     key "quote" .
     nth 0 .
     key "close" .
     nth 0 .
     _Double :: Maybe Double)
  --  return res
