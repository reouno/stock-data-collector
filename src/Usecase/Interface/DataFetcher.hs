module Usecase.Interface.DataFetcher
  ( DataFetcher(..)
  ) where

import           Data.ByteString.Lazy ( ByteString )
import           Data.Time            ( Day )
import           Network.HTTP.Simple  ( Request )

import           Entity.Stock         ( StockTerm )

class DataFetcher fetcher where
  fetchData :: fetcher -> String -> StockTerm -> IO ByteString
