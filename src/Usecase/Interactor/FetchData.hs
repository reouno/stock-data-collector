module Usecase.Interactor.FetchData
  ( fetchStockData
  ) where

import           Data.Time                                     ( Day )

import           Entity.Stock                                  ( Stock (..), StockTerm (..) )
import           Usecase.Interactor.Adapter.FetchedDataAdapter ( fromDayUnitFetchedData2Entities )
import           Usecase.Interface.DataFetcher                 ( DataFetcher (..) )

fetchStockData ::
     DataFetcher fetcher => fetcher -> String -> StockTerm -> IO [Stock]
fetchStockData fetcher tickerSymbol term = do
  fetchedData <- fetchData fetcher tickerSymbol term
  print fetchedData
  let mayStocks = fromDayUnitFetchedData2Entities fetchedData
  case mayStocks of
    Just stocks -> return stocks
    Nothing     -> error "Couldn't parse fetched JSON data"
