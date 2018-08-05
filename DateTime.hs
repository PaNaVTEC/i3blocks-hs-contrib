#!/usr/bin/env stack
-- stack --install-ghc runghc

import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  getZonedTime >>= printDateTime . formatTime defaultTimeLocale (format $ safeHead args)
  where
    format = maybe "%d-%m-%y %H:%M" id
    printDateTime datetime = putStrLn $ "\61747  " ++ datetime

safeHead :: [a] -> Maybe a
safeHead l         | length l > 0 = Just $ head l
safeHead         _ = Nothing
