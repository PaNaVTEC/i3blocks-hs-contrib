{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson (FromJSON(..), decode)
import GHC.Generics (Generic(..))

main :: IO ()
main = do
  manager  <- newManager tlsManagerSettings
  request <- createRequest
  response <- httpLbs request manager
  putStrLn $ maybe "" formatValue (decode $ responseBody response)
  where
    createRequest
      = addUserAgent <$> parseRequest "https://api.gdax.com/products/BTC-EUR/ticker"
      where
        addUserAgent req'
          = req' { requestHeaders = ("User-Agent", "Ticker") : requestHeaders req' }

formatValue :: GadxResponse -> String
formatValue (GadxResponse s) = "\61786 " ++ s ++ " €"

data GadxResponse = GadxResponse { ask :: String } deriving (Generic, FromJSON)
