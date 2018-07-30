#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack)
import           Numeric
import           Turtle

data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  mem <- memory
  liftIO $ putStrLn $ "\62171" ++ " " ++ formatFloatN mem 2 ++ "G"

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

memory :: Fractional a => Shell a
memory = do
  let memoryReport = input (filePath "/proc/meminfo")
  memFree <- head <$> match (parseMem MemFree) <$> (strict $ grep (parseMem MemFree) memoryReport)
  memTotal <- head <$> match (parseMem MemTotal) <$> (strict $ grep (parseMem MemTotal) memoryReport)
  return $ (fromIntegral $ memTotal - memFree) / 1024 / 1024

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

dockerIsRunning :: Shell Bool
dockerIsRunning = ((== ExitSuccess) . fst) <$> shellStrict (pack "pidof dockerd") empty

nImages :: Shell Integer
nImages = fold (inshell (pack "docker ps -q") empty) countLines
