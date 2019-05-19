{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle

main :: IO ()
main = sh $ do
  isRunning <- processIsRunning "dockerd"
  if isRunning
  then formatCommand (show <$> nImages)
  else formatCommand (return "x")

formatCommand :: Shell String -> Shell ()
formatCommand out = do
  out' <- out
  liftIO $ putStrLn $ "\61875" ++ " " ++ out'

nImages :: Shell Integer
nImages = fold (inshell (pack "docker ps -q") empty) countLines