module Usecase.Interactor.Adapter.StockTermAdapter
  ( stockTerm2String
  ) where

import           Entity.Stock ( StockTerm (..) )

stockTerm2String :: StockTerm -> String
stockTerm2String T1Day  = "1d"
stockTerm2String T5Day  = "5d"
stockTerm2String T3Mon  = "3m"
stockTerm2String T6Mon  = "6m"
stockTerm2String T1Year = "1y"
stockTerm2String T5Year = "5y"
stockTerm2String _      = "max"
