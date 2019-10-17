{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Reflection             ( Given, give )

import           Config.Config               ( Config (..), loadConfig )
import           Lib
import           Lib.RapidapiYahooFinanceAPI

main :: IO ()
main = do
  config <- loadConfig "config/config.yaml"
  give config app

sampleMainWithConfig :: Given Config => IO ()
sampleMainWithConfig = do
  sampleRequest
  getDJIDataOfLastDay
  putStrLn
    "Summary>>>-----------------------------------------------------------------------------------"
  print =<< fetchSummary [] []
  putStrLn
    "Charts>>>-----------------------------------------------------------------------------------"
  print =<< fetchCharts [] []
  putStrLn
    "Movers>>>-----------------------------------------------------------------------------------"
  print =<< fetchMovers [] []
  putStrLn
    "Quotes>>>-----------------------------------------------------------------------------------"
  print =<< fetchQuotes [] []
  putStrLn
    "AutoComplete>>>-----------------------------------------------------------------------------------"
  print =<< fetchAutoComplete [] []
