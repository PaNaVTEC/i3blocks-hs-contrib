{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle
import           System.Environment (getArgs)
import           Data.Maybe
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

main :: IO ()
main = sh $ do
  icons <- liftIO getArgs
  isRunning <- processIsRunning "dockerd"
  if isRunning
  then formatCommand icons (pack . show <$> nImages)
  else formatCommand icons (return "x")

getIcon :: [String] -> Maybe Text
getIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing

formatCommand :: [String] -> Shell Text -> Shell ()
formatCommand icons out = do
  out' <- out
  let icon = fromMaybe "Docker " $ getIcon icons
  liftIO $ putStrLn $ icon <> out'

nImages :: Shell Integer
nImages = fold (inshell (pack "docker ps -q") empty) countLines
