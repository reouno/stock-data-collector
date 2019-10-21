module Usecase.Interface.MayStockTerm
  ( MayStockTerm(..)
  ) where

import           Entity.Stock ( StockTerm (..) )

class MayStockTerm a where
  toStockTerm :: a -> Maybe StockTerm
