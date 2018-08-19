{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle

main :: IO ()
main = sh $ liftIO . putStrLn . format' =<< ssid

format' :: Text -> Text
format' ssid = "\61931" <> " " <> ssid

ssid :: Shell Text
ssid = strict $ inshell "iwgetid -r" empty
