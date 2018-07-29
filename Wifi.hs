#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  icon <- icon'
  wifi <- wifi'
  liftIO $ putStrLn $ (unpack icon) ++ " " ++ (unpack wifi)

icon' :: Shell Text
icon' = liftIO $ return "\61931"

wifi' :: Shell Text
wifi' = strict $ inshell (pack "iwgetid -r") empty
