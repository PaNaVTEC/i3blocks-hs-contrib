#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Data.Text (pack, unpack)
import           Turtle
import           Network.Wreq
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import GHC.Generics

data ResultCount = ResultCount {ask :: Double } deriving (Generic, Show)

instance ToJSON ResultCount
instance FromJSON ResultCount

main :: IO ()
main = do
  response <- get "https://api.gdax.com/products/BTC-EUR/ticker"
  ask <- return $ response ^? responseBody . key "ask"
  putStrLn $ formatValue ask
  return ()

formatValue:: Maybe Value -> String
formatValue (Just (String s)) = "\61786 " ++ unpack s ++ " €"
formatValue _ = ""