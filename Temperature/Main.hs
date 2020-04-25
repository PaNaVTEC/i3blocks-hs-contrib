{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text    (strip, pack)
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle
import           System.Environment (getArgs)
import           Data.Maybe

main :: IO ()
main = do
  args <- getArgs
  sh $ liftIO . putStrLn . formatBlock args =<< cpuTemperature (getSensor args)

formatBlock :: [String] -> Text -> Text
formatBlock icons temp =
  let icon = fromMaybe "" $ getIcon icons
  in icon <> temp

getSensor :: [String] -> Text
getSensor = pack . head

getIcon :: [String] -> Maybe Text
getIcon = \case
  _ : icon : _ -> Just $ pack icon
  _            -> Nothing

cpuTemperature :: Text -> Shell Text
cpuTemperature sensor
  = strip <$> strict (inshell cmd mempty)
  where
    cmd = "sensors | grep '" <> sensor <> "' | cut -d':' -f2 | awk '{$1=$1};1' | cut -d' ' -f1"
