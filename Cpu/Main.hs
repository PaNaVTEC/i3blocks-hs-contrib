{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Text (pack, strip, unpack)
import qualified Data.Text.IO as Text
import           Turtle
import           System.Environment (getArgs)
import           Data.Maybe

main :: IO ()
main = sh $ do
  icons <- liftIO getArgs
  liftIO . Text.putStrLn =<< cpuUsage icons
  maybe (return ExitSuccess) handleButton =<< currentButton

cpuUsage :: [String] -> Shell Text
cpuUsage icons = format' <$> idleCpu
  where format' idle =
          let usage = pack $ formatFloatN (100 - idle) 0
              icon = fromMaybe "" $ getIcon icons
          in icon <> usage <> "%"

getIcon :: [String] -> Maybe Text
getIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing

handleButton :: MonadIO io => Button -> io ExitCode
handleButton LeftClick = shell "urxvt -title pop-up -e htop" empty
handleButton _         = return ExitSuccess

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$>
  strict (inshell (pack "mpstat 1 1 -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"cpu-load\"[0].idle'") empty)
