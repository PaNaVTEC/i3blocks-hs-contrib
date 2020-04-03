{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO (putStrLn)
import           Data.Text    ( pack)
import           Prelude      hiding (putStrLn)
import           Turtle
import           System.Environment (getArgs)
import           Data.Maybe

main :: IO ()
main = do
  icons <- getArgs
  sh $ liftIO . putStrLn . formatBlock icons =<< getIp

formatBlock :: [String] -> Text -> Text
formatBlock icons ip =
  let icon = fromMaybe "" $ getIcon icons
  in icon <> ip

getIp :: Shell Text
getIp = strict $
  inshell "dig +short myip.opendns.com @resolver1.opendns.com" empty

getIcon :: [String] -> Maybe Text
getIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing
