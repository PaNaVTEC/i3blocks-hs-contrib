{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text       (unpack)
import           Network.Wreq

main :: IO ()
main = do
  response <- get "https://api.gdax.com/products/BTC-EUR/ticker"
  let ask' = response ^? responseBody . key "ask"
  putStrLn $ formatValue ask'

formatValue :: Maybe Value -> String
formatValue (Just (String s)) = "\61786 " ++ unpack s ++ " €"
formatValue _                 = ""
