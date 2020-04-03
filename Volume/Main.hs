{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Control.Applicative (liftA2)
import           Data.Maybe          (maybe, fromMaybe)
import           System.Environment (getArgs)
import           Data.Text           (pack, strip, unpack)
import           Turtle
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)

main :: IO ()
main = sh $ do
  icons <- liftIO getArgs
  liftIO . putStrLn =<< liftA2 (formatVol icons) isMuted getVolume
  maybe (return ExitSuccess) handleButton =<< currentButton

getMutedIcon :: [String] -> Maybe Text
getMutedIcon icons = case icons of
  mutedIcon : _ -> Just $ pack mutedIcon
  _             -> Nothing

getLowVolIcon :: [String] -> Maybe Text
getLowVolIcon icons = case icons of
  _ : lowVolIcon : _ -> Just $ pack lowVolIcon
  _                  -> Nothing

getHighVolIcon :: [String] -> Maybe Text
getHighVolIcon icons = case icons of
  _ : _ : highVolIcon : _ -> Just $ pack highVolIcon
  _                       -> Nothing

handleButton :: MonadIO io => Button -> io ExitCode
handleButton LeftClick  = shell "pavucontrol" empty
handleButton RightClick = shell "ponymix toggle >/dev/null" empty
handleButton WheelUp    = shell "ponymix increase 5 >/dev/null" empty
handleButton WheelDown  = shell "ponymix decrease 5 >/dev/null" empty
handleButton _          = pure . ExitFailure $ 1

formatVol :: [String] -> Bool -> Integer -> Text
formatVol icons True _    = fromMaybe "Muted" $ getMutedIcon icons
formatVol icons False vol = icon icons vol <> (pack $ show vol) <> "%"

icon :: [String] -> Integer -> Text
icon icons v | v < 33 = fromMaybe "Volume " $ getLowVolIcon icons
icon icons _ = fromMaybe "Volume " $ getHighVolIcon icons

isMuted :: Shell Bool
isMuted = (== ExitSuccess) <$> shell (pack "ponymix is-muted") empty

getVolume :: Shell Integer
getVolume = toVolumeInt <$> strict (inshell (pack "ponymix get-volume") empty)
  where toVolumeInt = read . unpack . strip
