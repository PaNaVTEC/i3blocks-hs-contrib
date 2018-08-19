{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (unpack)
import           Turtle

main :: IO ()
main = sh $ liftIO . putStrLn =<< cpuTemperature

cpuTemperature :: Shell String
cpuTemperature = unpack <$> (strict $
  inshell ("sensors | grep -oP 'Package.*?\\+\\K[0-9.°C]+'") mempty)
