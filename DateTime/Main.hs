{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let (format, icon) = (getFormat args, getIcon args)
  zonedTime <- getZonedTime
  putStrLn $ icon <> (formatTime defaultTimeLocale format zonedTime)

getIcon :: [String] -> String
getIcon = \case
  _ : icon : _ -> icon
  _            -> ""

getFormat :: [String] -> String
getFormat = \case
  format : _ -> format
  _ -> defaultFormat
  where
    defaultFormat = "%d-%m-%y %H:%M"
