#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack, strip)
import           Turtle

main :: IO ()
main = sh $ do
  vol <- getVolume
  liftIO $ putStrLn $ icon vol ++ " " ++ show vol ++ "%"

icon :: Integer -> String
icon 0 = "\61478"
icon v | v < 33 = "\61479"
icon _ = "\61480"

getVolume :: Shell Integer
getVolume = toInteger <$> (strict $ inshell (pack "ponymix get-volume") empty)
  where toInteger = read . unpack . strip
