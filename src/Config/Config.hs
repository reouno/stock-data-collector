{-# LANGUAGE DeriveGeneric #-}

module Config.Config where

import           Data.Aeson
import           Data.Text
import           Data.Yaml.Config
import           GHC.Generics     ( Generic )

data Config =
  Config
    { rapidapiYahooFinanceSummaryEndpoint :: String
    , rapidapiYahooFinanceChartsEndpoint  :: String
    , xRapidapiHost                       :: Text
    , xRapidapiKey                        :: Text
    }
  deriving (Show, Eq, Generic)

instance FromJSON Config

loadConfig :: FilePath -> IO Config
loadConfig configFile = loadYamlSettings [configFile] [] useEnv
