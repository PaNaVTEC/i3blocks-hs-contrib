{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson (FromJSON(..), decode)
import GHC.Generics (Generic(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args      <- getArgs
  let (icon, mode) = (getIcon args, getMode args)
  mResponse <- makeBtcReq mode
  putStrLn $ maybe "" (formatValue icon mode) mResponse

getIcon :: [String] -> String
getIcon = \case
  _ : icon : _ -> icon
  _            -> ""

getMode :: [String] -> Mode
getMode = read . head

makeBtcReq :: Mode -> IO (Maybe GdaxResponse)
makeBtcReq mode = do
  manager  <- newManager tlsManagerSettings
  request  <- createRequest
  response <- httpLbs request manager
  pure . decode $ responseBody response
  where
    urlForMode
      = "https://api.gdax.com/products/BTC-"<> show mode <>"/ticker"
    createRequest
      = addUserAgent <$> parseRequest urlForMode
    addUserAgent req'
      = req' { requestHeaders = ("User-Agent", "Ticker") : requestHeaders req' }

formatValue :: String -> Mode -> GdaxResponse -> String
formatValue icon mode (GdaxResponse amount)
  = icon <> formatWithCurrency amount mode

formatWithCurrency :: String -> Mode -> String
formatWithCurrency amount = \case
  EUR -> amount <> " €"
  USD -> "$ " <> amount
  GBP -> "£ " <> amount

data GdaxResponse = GdaxResponse { ask :: String } deriving (Generic, FromJSON)
data Mode = EUR | GBP | USD deriving (Eq, Show, Read)
