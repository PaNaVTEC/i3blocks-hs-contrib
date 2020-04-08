{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Monoid
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle
import qualified Data.Text as Text
import           Data.Maybe
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  sh $ liftIO . putStrLn . formatBlock args =<< getSSID

formatBlock :: [String] -> Text -> Text
formatBlock args ssid
  = if Text.null ssid
  then fromMaybe "No WiFi" $ getNoWifiIcon args
  else fromMaybe "" (getWifiIcon args) <> ssid

getNoWifiIcon :: [String] -> Maybe Text
getNoWifiIcon icons = case icons of
  noWifiIcon : _ -> Just $ Text.pack noWifiIcon
  _              -> Nothing

getWifiIcon :: [String] -> Maybe Text
getWifiIcon icons = case icons of
  _ : wifiIcon : _ -> Just $ Text.pack wifiIcon
  _                -> Nothing

getSSID :: Shell Text
getSSID = do
  (exitCode, out) <- shellStrict "iwgetid -r" empty
  if exitCode == ExitFailure 255 then pure mempty else pure out
