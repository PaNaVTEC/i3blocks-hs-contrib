#!/usr/bin/env stack
-- stack --install-ghc runghc

import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  getZonedTime >>= putStrLn . formatTime defaultTimeLocale (format $ safeHead args)
  where
    format = maybe "%d-%m-%y %H:%M" id

safeHead :: [a] -> Maybe a
safeHead l         | length l > 0 = Just $ head l
safeHead         _ = Nothing
