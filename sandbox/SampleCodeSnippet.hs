:set -XOverloadedStrings
:set -XDeriveGeneric
:set +m
import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import Network.HTTP.Simple
import Network.HTTP.Types
import GHC.Generics (Generic)
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Text as T


initReq <- parseRequest "https://apidojo-yahoo-finance-v1.p.rapidapi.com/market/get-charts"

:{
headers =
  [ ("x-rapidapi-host", "apidojo-yahoo-finance-v1.p.rapidapi.com")
  , ("x-rapidapi-key", "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a")
  ]
:}

reqWithHeader = setRequestHeaders headers initReq

:{
queryParams =
  [ ("region", Just "US")
  , ("lang", Just "en")
  , ("symbol", Just "^DJI")
  , ("interval", Just "1d")
  , ("range", Just "1d")
  ]
:}

req = setRequestQueryString queryParams reqWithHeader

res <- httpLBS req

resBody = getResponseBody res

quote = resBody ^? key "chart" . key "result" . nth 0 . key "indicators" . key "quote" . nth 0



-- ここからできる
:set -XOverloadedStrings
:set +m

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Data.Text             ( unpack )
import           Data.Time             ( Day, utctDay )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           Numeric.Extra         ( doubleToFloat )


fetcher = YahooFinanceFetcher "bf6a2bf862mshe23d3acd0df4650p1c0239jsnecc20948c78a"
res <- fetchData fetcher "AAPL" T1Day
fromDayUnitFetchedData2Entities res

coreData = res ^? key "chart" . key "result" . nth 0
prettyData = encodePretty coreData

entityStockData = (\n s ps ts -> [Stock n s D1 ps ts]) <$> name <*> symbol <*> prices <*> timestamps

:{
symbol =
  unpack <$> rawData ^? key "chart" . key "result" . nth 0 . key "meta" .
  key "symbol" .
  _String
name = symbol
:}
-- --> Just "AAPL"

:{
prices :: Maybe [StockPrice]
prices =
  (\closePrice ->
     [ StockPrice
         (doubleToFloat <$> open)
         closePrice
         (doubleToFloat <$> high)
         (doubleToFloat <$> low)
     ]) .
  doubleToFloat <$>
  close
:}

:{
open =
  rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
  key "quote" .
  nth 0 .
  key "open" .
  nth 0 .
  _Double :: Maybe Double
-- --> Just 233.3699951171875

close =
  rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
  key "quote" .
  nth 0 .
  key "close" .
  nth 0 .
  _Double :: Maybe Double
-- --> Just 234.3699951171875

high =
  rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
  key "quote" .
  nth 0 .
  key "high" .
  nth 0 .
  _Double :: Maybe Double
-- --> Just 235.23260498046875

low =
  rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
  key "quote" .
  nth 0 .
  key "low" .
  nth 0 .
  _Double :: Maybe Double
-- --> Just 233.1999969482422

timestamps :: Maybe [Day]
timestamps =
  return <$>
  (utctDay . posixSecondsToUTCTime . fromIntegral <$> rawData ^? key "chart" .
   key "result" .
   nth 0 .
   key "timestamp" .
   nth 0 .
   _Integer)
-- -->  Nothing
:}

:{
tss = utctDay . posixSecondsToUTCTime . fromIntegral <$> rawData ^? key "chart" .
  key "result" .
  nth 0 .
  key "timestamp" .
  nth 0 .
  _Integer
:}