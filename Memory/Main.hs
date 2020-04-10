{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle
import           Data.Maybe
import           System.Environment (getArgs)
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  icons <- liftIO getArgs
  (liftIO . putStrLn . format' icons) =<< (memory :: Shell Float)
  where
    format' icons mem =
      let icon = fromMaybe "" $ getIcon icons
          usedMem = pack $ formatFloatN mem 2
      in icon <> usedMem <> "G"

getIcon :: [String] -> Maybe Text
getIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing

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
