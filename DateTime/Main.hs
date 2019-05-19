module Main where

import           Data.Maybe          (fromMaybe)
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  getZonedTime >>= printDateTime . formatTime defaultTimeLocale (format $ safeHead args)
  where
    format = fromMaybe "%d-%m-%y %H:%M"
    printDateTime datetime = putStrLn $ "\61747  " ++ datetime

safeHead :: [a] -> Maybe a
safeHead l | not (null l) = Just $ head l
safeHead _ = Nothing
