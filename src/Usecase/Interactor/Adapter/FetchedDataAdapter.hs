{-# LANGUAGE OverloadedStrings #-}

module Usecase.Interactor.Adapter.FetchedDataAdapter
  ( fromDayUnitFetchedData2Entities
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Lazy  ( ByteString )
import           Data.Text             ( unpack )
import           Data.Time             ( Day, utctDay )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           Numeric.Extra         ( doubleToFloat )

import           Entity.Stock          ( PriceType (..), Stock (..), StockPrice (..) )

fromDayUnitFetchedData2Entities :: ByteString -> Maybe [Stock]
fromDayUnitFetchedData2Entities rawData =
  (\n s ps ts -> [Stock n s D1 ps ts]) <$> name <*> symbol <*> prices <*>
  timestamps
    -- TODO: get right `name`, not ticker symbol
  where
    name = symbol
    symbol =
      unpack <$> rawData ^? key "chart" . key "result" . nth 0 . key "meta" .
      key "symbol" .
      _String
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
    open =
      rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
      key "quote" .
      nth 0 .
      key "open" .
      nth 0 .
      _Double :: Maybe Double
    close =
      rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
      key "quote" .
      nth 0 .
      key "close" .
      nth 0 .
      _Double :: Maybe Double
    high =
      rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
      key "quote" .
      nth 0 .
      key "high" .
      nth 0 .
      _Double :: Maybe Double
    low =
      rawData ^? key "chart" . key "result" . nth 0 . key "indicators" .
      key "quote" .
      nth 0 .
      key "low" .
      nth 0 .
      _Double :: Maybe Double
    timestamps :: Maybe [Day]
    timestamps =
      return <$>
      (utctDay . posixSecondsToUTCTime . fromIntegral <$> rawData ^? key "chart" .
       key "result" .
       nth 0 .
       key "timestamp" .
       nth 0 .
       _Integer)
