#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack)
import           Turtle

main :: IO ()
main = sh $ do
  isRunning <- processIsRunning "openvpn"
  case isRunning of
    False -> formatCommand "✓"
    True -> formatCommand "x"

processIsRunning :: String -> Shell Bool
processIsRunning process = ((== ExitSuccess) . fst) <$> shellStrict (pack $ "pidof " ++ process) empty

formatCommand :: String -> Shell ()
formatCommand out = liftIO $ putStrLn $ "\61676" ++ " " ++ out
