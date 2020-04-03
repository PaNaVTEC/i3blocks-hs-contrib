{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text    (strip, pack)
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle
import           System.Environment (getArgs)
import           Data.Maybe

main :: IO ()
main = do
  icons <- getArgs
  sh $ liftIO . putStrLn . formatBlock icons =<< cpuTemperature

formatBlock :: [String] -> Text -> Text
formatBlock icons temp =
  let icon = fromMaybe "" $ getIcon icons
  in icon <> temp <> "°C"

getIcon :: [String] -> Maybe Text
getIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing

cpuTemperature :: Shell Text
cpuTemperature
  = strip
  <$> strict (inshell "sensors | grep -oP 'Package[^\\+]*\\+\\K[0-9]+'" mempty)
