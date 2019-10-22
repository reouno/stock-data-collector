{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.RapidapiYahooFinanceAPI where

import           Control.Lens
import           Data.ByteString      ( ByteString )
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        ( foldl' )
import           Network.HTTP.Simple  ( Query, Request, RequestHeaders, Response, httpLBS,
                                        parseRequest_, setRequestHeaders, setRequestQueryString )

import           Lib.ByteStringShow

-- data RapidapiYahooFinanceConfigs =
--   RapidapiYahooFinanceConfigs
--     { _ryfEndpoints :: RapidapiYahooFinanceEndpoints
--     , _ryfHeaders   :: RapidapiYahooFinanceHeaders
--     }
--   deriving (Show, Eq)
data RapidapiYahooFinanceEndpoints =
  RapidapiYahooFinanceEndpoints
    { _ryfGetSummary   :: String
    , _ryfGetCharts    :: String
    , _ryfGetMovers    :: String
    , _ryfGetQuotes    :: String
    , _ryfAutoComplete :: String
    }
  deriving (Show, Eq)

data RapidapiYahooFinanceHeaders =
  RapidapiYahooFinanceHeaders
    { _ryfXRapidapiHost :: ByteString
    , _ryfXRapidapiKey  :: ByteString
    }
  deriving (Show, Eq)

-- makeLenses ''RapidapiYahooFinanceConfigs
makeLenses ''RapidapiYahooFinanceEndpoints

makeLenses ''RapidapiYahooFinanceHeaders

-- ryfDefaultConfigs =
--   RapidapiYahooFinanceConfigs ryfDefaultEndpoints ryfDefaultHeaders
ryfDefaultEndpoints =
  RapidapiYahooFinanceEndpoints
    "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-summary"
    "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-charts"
    "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-movers"
    "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-quotes"
    "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/auto-complete"

-- ryfDefaultHeaders =
--   RapidapiYahooFinanceHeaders
--     "apidojo-yahoo-finance-v1.p.rapidapi.com"
--     "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a"
data Endpoint
  = Summary
  | Charts
  | Movers
  | Quotes
  | AutoComplete
  deriving (Show, Eq)

data RapidapiYahooFinanceHeader
  = RYFHost ByteString
  | RYFKey ByteString

data RapidapiYahooFinanceQuery
  = RYFRegion ByteString
  | RYFLang ByteString
  | RYFStart Int
  | RYFCount Int
  | RYFSymbols [ByteString]
  | RYFSymbol ByteString
  | RYFInterval ByteString
  | RYFRange ByteString
  | RYFComparisons [ByteString]
  | RYFQuery ByteString
  deriving (Show, Eq)

ryfDefaultHeaders :: RequestHeaders
ryfDefaultHeaders =
  [ ("x-rapidapi-host", "apidojo-yahoo-finance-v1.p.rapidapi.com")
  , ("x-rapidapi-key", "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a")
  ]

makeRyfHeaders :: [RapidapiYahooFinanceHeader] -> RequestHeaders
makeRyfHeaders = makeRyfHeaders' ryfDefaultHeaders
  where
    makeRyfHeaders' ::
         RequestHeaders -> [RapidapiYahooFinanceHeader] -> RequestHeaders
    makeRyfHeaders' headers [] = headers
    makeRyfHeaders' headers (RYFHost host:xs) =
      makeRyfHeaders'
        [ if x0 == "x-rapidapi-host"
          then (x0, host)
          else (x0, x1)
        | (x0, x1) <- headers
        ]
        xs
    makeRyfHeaders' headers (RYFKey key:xs) =
      makeRyfHeaders'
        [ if x0 == "x-rapidapi-key"
          then (x0, key)
          else (x0, x1)
        | (x0, x1) <- headers
        ]
        xs

setRyfHeaders :: [RapidapiYahooFinanceHeader] -> Request -> Request
setRyfHeaders headers = setRequestHeaders (makeRyfHeaders headers)

ryfDefaultQueries :: Query
ryfDefaultQueries =
  [ ("region", Just "US")
  , ("lang", Just "en")
  , ("start", Nothing)
  , ("count", Nothing)
  , ("symbols", Just "^DJI,^IXIC,^NDX")
  , ("symbol", Just "^DJI")
  , ("interval", Just "1d")
  , ("range", Just "1d")
  , ("comparisons", Just "AMZN,AAPL,GOOG")
  , ("query", Just "google")
  ]

makeRyfQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfQueries = foldl' parseQuery ryfDefaultQueries

parseQuery :: Query -> RapidapiYahooFinanceQuery -> Query
parseQuery queries (RYFRegion query) =
  replaceElemOfQueries queries "region" $ Just (showB query)
parseQuery queries (RYFLang query) =
  replaceElemOfQueries queries "lang" $ Just (showB query)
parseQuery queries (RYFStart query) =
  replaceElemOfQueries queries "start" $ Just (showB query)
parseQuery queries (RYFCount query) =
  replaceElemOfQueries queries "count" $ Just (showB query)
parseQuery queries (RYFSymbols query) =
  replaceElemOfQueries queries "symbols" $ Just (showB query)
parseQuery queries (RYFSymbol query) =
  replaceElemOfQueries queries "symbol" $ Just (showB query)
parseQuery queries (RYFInterval query) =
  replaceElemOfQueries queries "interval" $ Just (showB query)
parseQuery queries (RYFRange query) =
  replaceElemOfQueries queries "range" $ Just (showB query)
parseQuery queries (RYFComparisons query) =
  replaceElemOfQueries queries "comparisons" $ Just (showB query)
parseQuery queries (RYFQuery query) =
  replaceElemOfQueries queries "query" $ Just (showB query)

replaceElemOfQueries :: Query -> ByteString -> Maybe ByteString -> Query
replaceElemOfQueries xs key value =
  [ if x0 == key
    then (x0, value)
    else (x0, x1)
  | (x0, x1) <- xs
  ]

makeRyfSummaryQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfSummaryQueries queries =
  filter (\(x0, _) -> x0 `elem` ["region", "lang"]) $ makeRyfQueries queries

makeRyfChartsQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfChartsQueries queries =
  filter
    (\(x0, _) ->
       x0 `elem`
       ["region", "lang", "symbol", "interval", "range", "comparisons"]) $
  makeRyfQueries queries

makeRyfMoversQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfMoversQueries queries =
  filter (\(x0, _) -> x0 `elem` ["region", "lang", "start", "count"]) $
  makeRyfQueries queries

makeRyfQuotesQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfQuotesQueries queries =
  filter (\(x0, _) -> x0 `elem` ["region", "lang", "symbols"]) $
  makeRyfQueries queries

makeRyfAutoCompleteQueries :: [RapidapiYahooFinanceQuery] -> Query
makeRyfAutoCompleteQueries queries =
  filter (\(x0, _) -> x0 `elem` ["region", "lang", "query"]) $
  makeRyfQueries queries

setRyfSummaryQueries :: [RapidapiYahooFinanceQuery] -> Request -> Request
setRyfSummaryQueries queries =
  setRequestQueryString $ makeRyfSummaryQueries queries

setRyfChartsQueries :: [RapidapiYahooFinanceQuery] -> Request -> Request
setRyfChartsQueries queries =
  setRequestQueryString $ makeRyfChartsQueries queries

setRyfMoversQueries :: [RapidapiYahooFinanceQuery] -> Request -> Request
setRyfMoversQueries queries =
  setRequestQueryString $ makeRyfMoversQueries queries

setRyfQuotesQueries :: [RapidapiYahooFinanceQuery] -> Request -> Request
setRyfQuotesQueries queries =
  setRequestQueryString $ makeRyfQuotesQueries queries

setRyfAutoCompleteQueries :: [RapidapiYahooFinanceQuery] -> Request -> Request
setRyfAutoCompleteQueries queries =
  setRequestQueryString $ makeRyfAutoCompleteQueries queries

fetchSummary ::
     [RapidapiYahooFinanceHeader]
  -> [RapidapiYahooFinanceQuery]
  -> IO (Response BL.ByteString)
fetchSummary headers queries = do
  let initReq = parseRequest_ $ ryfDefaultEndpoints ^. ryfGetSummary
      reqWithHeaders = setRyfHeaders headers initReq
      req = setRyfSummaryQueries queries reqWithHeaders
  httpLBS req

fetchCharts ::
     [RapidapiYahooFinanceHeader]
  -> [RapidapiYahooFinanceQuery]
  -> IO (Response BL.ByteString)
fetchCharts headers queries = do
  let initReq = parseRequest_ $ ryfDefaultEndpoints ^. ryfGetCharts
      reqWithHeaders = setRyfHeaders headers initReq
      req = setRyfChartsQueries queries reqWithHeaders
  httpLBS req

fetchMovers ::
     [RapidapiYahooFinanceHeader]
  -> [RapidapiYahooFinanceQuery]
  -> IO (Response BL.ByteString)
fetchMovers headers queries = do
  let initReq = parseRequest_ $ ryfDefaultEndpoints ^. ryfGetMovers
      reqWithHeaders = setRyfHeaders headers initReq
      req = setRyfMoversQueries queries reqWithHeaders
  httpLBS req

fetchQuotes ::
     [RapidapiYahooFinanceHeader]
  -> [RapidapiYahooFinanceQuery]
  -> IO (Response BL.ByteString)
fetchQuotes headers queries = do
  let initReq = parseRequest_ $ ryfDefaultEndpoints ^. ryfGetQuotes
      reqWithHeaders = setRyfHeaders headers initReq
      req = setRyfQuotesQueries queries reqWithHeaders
  httpLBS req

fetchAutoComplete ::
     [RapidapiYahooFinanceHeader]
  -> [RapidapiYahooFinanceQuery]
  -> IO (Response BL.ByteString)
fetchAutoComplete headers queries = do
  let initReq = parseRequest_ $ ryfDefaultEndpoints ^. ryfAutoComplete
      reqWithHeaders = setRyfHeaders headers initReq
      req = setRyfAutoCompleteQueries queries reqWithHeaders
  httpLBS req
