{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle

data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ (liftIO . putStrLn . format') =<< (memory :: Shell Float)
  where
    format' mem = "\62171 " ++ formatFloatN mem 2 ++ "G"

memory :: RealFloat a => Shell a
memory = do
  let memoryReport = input (filePath "/proc/meminfo")
  memFree <- parseMemory memoryReport MemFree
  memTotal <- parseMemory memoryReport MemTotal
  return $ kbsToGb (fromIntegral $ memTotal - memFree)
  where
    kbsToGb kbs = kbs / 1024 / 1024
    parseMemory memoryReport k =
      head . match (parseMem k) <$> strict (grep (parseMem k) memoryReport)

parseMem :: MemType -> Pattern Integer
parseMem memType = parseMemType memType *> spaces1 *> decimal <* star anyChar

parseMemType :: MemType -> Pattern Text
parseMemType t = toMem t <> ":"
  where
    toMem MemTotal = "MemTotal"
    toMem MemFree  = "MemFree"

filePath :: String -> Turtle.FilePath
filePath p = fromText $ pack p
