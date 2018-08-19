{-# LANGUAGE OverloadedStrings #-}

import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle

main :: IO ()
main = sh $ liftIO . putStrLn =<< ip

ip :: Shell Text
ip = strict $
  inshell "dig +short myip.opendns.com @resolver1.opendns.com" empty
