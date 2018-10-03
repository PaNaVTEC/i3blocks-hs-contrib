{-# LANGUAGE OverloadedStrings #-}

import           Data.Text    (strip)
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle

main :: IO ()
main = sh $ liftIO . putStrLn =<< cpuTemperature

cpuTemperature :: Shell Text
cpuTemperature = (<> "°C") . strip <$>
  strict (inshell "sensors | grep -oP 'Package[^\\+]*\\+\\K[0-9]+'" mempty)
