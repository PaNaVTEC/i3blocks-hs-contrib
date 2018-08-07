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

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$> (strict $ inshell (pack "iostat -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"avg-cpu\".idle'") empty)
