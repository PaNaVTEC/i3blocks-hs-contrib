#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  wifi <- wifi'
  liftIO $ putStrLn $ "\61931" ++ " " ++ (unpack wifi)

wifi' :: Shell Text
wifi' = strict $ inshell (pack "iwgetid -r") empty
