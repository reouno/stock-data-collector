module Entity.Internal.StockTerm where

data StockTerm
  = T1Day
  | T5Day
  | T3Mon
  | T6Mon
  | T1Year
  | T5Year
  | TMax
  deriving (Show, Eq)
