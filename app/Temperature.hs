{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.IO (putStrLn)
import           Turtle
import           Prelude hiding (putStrLn)

main :: IO ()
main = sh $ liftIO . putStrLn =<< cpuTemperature

cpuTemperature :: Shell Text
cpuTemperature = strict $
  inshell ("sensors | grep -oP 'Package.*?\\+\\K[0-9.°C]+'") mempty
