{-# LANGUAGE FlexibleInstances #-}

module Usecase.Interactor.Adapter.StockTermAdapter where

import           Data.Char                      ( toUpper )

import           Entity.Stock                   ( StockTerm (..) )
import           Usecase.Interface.MayStockTerm ( MayStockTerm (..) )

instance MayStockTerm String where
  toStockTerm term
    | uTerm == "1D" = Just T1Day
    | uTerm == "5D" = Just T5Day
    | uTerm == "3M" = Just T3Mon
    | uTerm == "6M" = Just T6Mon
    | uTerm == "1Y" = Just T1Year
    | uTerm == "5Y" = Just T5Year
    | uTerm == "MAX" = Just TMax
    | otherwise = Nothing
    where
      uTerm = map toUpper term

stockTerm2String :: StockTerm -> String
stockTerm2String T1Day  = "1d"
stockTerm2String T5Day  = "5d"
stockTerm2String T3Mon  = "3m"
stockTerm2String T6Mon  = "6m"
stockTerm2String T1Year = "1y"
stockTerm2String T5Year = "5y"
stockTerm2String _      = "max"
