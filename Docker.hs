#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack)
import           Turtle

main :: IO ()
main = sh $ do
  isRunning <- dockerIsRunning
  case isRunning of
    False -> formatCommand (return "x")
    True  -> formatCommand (show <$> nImages)

formatCommand :: Shell String -> Shell ()
formatCommand out = do
  out' <- out
  liftIO $ putStrLn $ "\61875" ++ " " ++ out'

dockerIsRunning :: Shell Bool
dockerIsRunning = ((== ExitSuccess) . fst) <$> shellStrict (pack "pidof dockerd") empty

nImages :: Shell Integer
nImages = fold (inshell (pack "docker ps -q") empty) countLines
