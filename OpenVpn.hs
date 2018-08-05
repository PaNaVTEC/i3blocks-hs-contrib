#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Text (pack)
import           Turtle

main :: IO ()
main = sh $ do
  isRunning <- processIsRunning "openvpn"
  case isRunning of
    False -> formatCommand "x"
    True -> formatCommand "✓"

formatCommand :: String -> Shell ()
formatCommand out = liftIO $ putStrLn $ "\61676" ++ " " ++ out
