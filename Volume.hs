#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack, strip)
import           Turtle

main :: IO ()
main = sh $ do
  muted <- isMuted
  vol <- getVolume
  liftIO $ putStrLn $ formatVol muted vol

formatVol :: Bool -> Integer -> String
formatVol True _ = "\61478" ++ " " ++ "x"
formatVol False vol = icon vol ++ " " ++ show vol ++ "%"

icon :: Integer -> String
icon v | v < 33 = "\61479"
icon _ = "\61480"

isMuted :: Shell Bool
isMuted = (/= ExitSuccess) <$> shell (pack "ponymix is-muted") empty

getVolume :: Shell Integer
getVolume = toVolumeInt <$> (strict $ inshell (pack "ponymix get-volume") empty)
  where toVolumeInt = read . unpack . strip
