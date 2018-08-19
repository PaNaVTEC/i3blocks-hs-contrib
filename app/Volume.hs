{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Control.Applicative (liftA2)
import           Data.Maybe          (maybe)
import           Data.Text           (pack, strip, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  liftIO . putStrLn =<<liftA2 formatVol isMuted getVolume
  maybe (return ExitSuccess) handleButton =<< currentButton

handleButton :: MonadIO io => Button -> io ExitCode
handleButton LeftClick  = shell "pavucontrol" empty
handleButton RightClick = shell "ponymix toggle >/dev/null" empty
handleButton WheelUp    = shell "ponymix increase 5 >/dev/null" empty
handleButton WheelDown  = shell "ponymix decrease 5 >/dev/null" empty

formatVol :: Bool -> Integer -> String
formatVol True _    = "\61478 x"
formatVol False vol = icon vol ++ " " ++ show vol ++ "%"

icon :: Integer -> String
icon v | v < 33 = "\61479"
icon _ = "\61480"

isMuted :: Shell Bool
isMuted = (== ExitSuccess) <$> shell (pack "ponymix is-muted") empty

getVolume :: Shell Integer
getVolume = toVolumeInt <$> (strict $ inshell (pack "ponymix get-volume") empty)
  where toVolumeInt = read . unpack . strip
