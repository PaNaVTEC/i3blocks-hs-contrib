{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Common
import Data.Bool (bool)
import System.Environment (getArgs)
import Data.Text    (pack)
import Turtle

main :: IO ()
main = do
  args <- getArgs
  let (onClickHandler, icon) = (getOnClickHandler args, getIcon args)
  sh $ do
    processIsRunning "openvpn" >>= bool (printCommand icon "x") (printCommand icon "✓")
    maybe (return ExitSuccess) (handleButton onClickHandler) =<< currentButton

getIcon :: [String] -> String
getIcon = \case
  _ : icon : _ -> icon
  _            -> ""

getOnClickHandler :: [String] -> Maybe OnClickHandlerPath
getOnClickHandler = \case
  clickHandler :  _ -> Just $ pack clickHandler
  _                 -> Nothing

printCommand :: String -> String -> Shell ()
printCommand icon out = liftIO . putStrLn $ icon <> out

handleButton :: MonadIO io => Maybe OnClickHandlerPath -> Button -> io ExitCode
handleButton handler LeftClick  = maybe (pure ExitSuccess) (`shell` empty) handler
handleButton _       _          = pure ExitSuccess

type OnClickHandlerPath = Text
