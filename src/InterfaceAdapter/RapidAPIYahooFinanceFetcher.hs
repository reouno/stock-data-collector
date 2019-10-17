{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module InterfaceAdapter.RapidAPIYahooFinanceFetcher where

import           Control.Lens
import           Control.Lens.TH
import           Data.ByteString                             ( ByteString )
import           Data.Text                                   ( pack )
import           Data.Text.Encoding                          ( encodeUtf8 )
import           Network.HTTP.Simple                         ( Request, getResponseBody, httpLBS,
                                                               parseRequest_, setRequestQueryString )

import           Lib.RapidapiYahooFinanceAPI
import           Usecase.Interactor.Adapter.StockTermAdapter ( stockTerm2String )
import           Usecase.Interface.DataFetcher               ( DataFetcher (..) )

newtype YahooFinanceFetcher =
  YahooFinanceFetcher
    { yahooFinanceFetcherApiKey :: ByteString
    }
  deriving (Show, Eq)

instance DataFetcher YahooFinanceFetcher where
  fetchData fetcher symbol term =
    getResponseBody <$>
    fetchCharts [RYFKey $ yahooFinanceFetcherApiKey fetcher] queries
    where
      queries =
        [ RYFRegion "US"
        , RYFLang "en"
        , RYFSymbol $ (encodeUtf8 . pack) symbol
        , RYFInterval "1d"
        , RYFRange $ (encodeUtf8 . pack . stockTerm2String) term
        ]
