module Usecase.Interactor.FetchData
  ( fetchStockData
  ) where

import           Data.Time                                     ( Day )

import           Entity.Stock                                  ( Stock (..), StockTerm (..) )
import           Usecase.Interactor.Adapter.FetchedDataAdapter ( fromDayUnitFetchedData2Entities )
import           Usecase.Interface.DataFetcher                 ( DataFetcher (..) )
import           Usecase.Interface.MayStockTerm                ( MayStockTerm (..) )

{-
 - TODO: Write more appropriate exception handling
 -       Should use Either than Maybe
-}
fetchStockData ::
     (DataFetcher fetcher, MayStockTerm stockTerm)
  => fetcher
  -> String
  -> stockTerm
  -> IO [Stock]
fetchStockData fetcher tickerSymbol term = do
  let stockTerm =
        case toStockTerm term of
          Just t  -> t
          Nothing -> error "Could not parse stock term"
  fetchedData <- fetchData fetcher tickerSymbol stockTerm
  print fetchedData
  let mayStocks = fromDayUnitFetchedData2Entities fetchedData
  case mayStocks of
    Just stocks -> return stocks
    Nothing     -> error "Couldn't parse fetched JSON data"
