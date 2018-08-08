#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack)
import           Turtle
import           Common

data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  mem <- memory
  liftIO $ putStrLn $ "\62171" ++ " " ++ formatFloatN mem 2 ++ "G"

memory :: Fractional a => Shell a
memory = do
  let memoryReport = input (filePath "/proc/meminfo")
  memFree <- parseMemory memoryReport MemFree
  memTotal <- parseMemory memoryReport MemTotal
  return $ (fromIntegral $ memTotal - memFree) / 1024 / 1024
  where
    parseMemory memoryReport k =
      head <$> match (parseMem k) <$> (strict $ grep (parseMem k) memoryReport)

parseMem :: MemType -> Pattern Integer
parseMem memType = do
  toMem memType <> ":"
  mem <- spaces1 *> decimal <* spaces1
  star anyChar
  return mem
  where
    toMem MemTotal = "MemTotal"
    toMem MemFree  = "MemFree"

filePath :: String -> Turtle.FilePath
filePath p = fromText $ pack p
