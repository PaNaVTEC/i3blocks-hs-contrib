#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack, strip)
import           Turtle
import           Common

main :: IO ()
main = sh $ do
  idle <- idleCpu
  liftIO $ putStrLn $ "\61668 " ++ formatFloatN (100 - idle) 2 ++ "%"
  button <- currentButton
  handleButton button

handleButton :: MonadIO io => Maybe Button -> io ExitCode
handleButton Nothing = return ExitSuccess
handleButton (Just LeftClick) = shell "urxvt -title pop-up -e htop" empty

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$> (strict $ inshell (pack "iostat -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"avg-cpu\".idle'") empty)
