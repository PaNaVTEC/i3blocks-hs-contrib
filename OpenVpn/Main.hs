{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Data.Bool (bool)
import Turtle

type OnClickHandlerPath = Text

optionsParser :: Parser (Maybe OnClickHandlerPath)
optionsParser = optional $ optText "handler" 'c' "Left click handler script to invoke"

main :: IO ()
main = do
  onClickHandler <- options "Script to show the status of open vpn connection" optionsParser
  sh $ do
    processIsRunning "openvpn" >>= bool (printCommand "x") (printCommand "✓")
    maybe (return ExitSuccess) (handleButton onClickHandler) =<< currentButton

printCommand :: String -> Shell ()
printCommand out = liftIO $ putStrLn $ "\61676" ++ " " ++ out

handleButton :: MonadIO io => Maybe OnClickHandlerPath -> Button -> io ExitCode
handleButton handler LeftClick  = maybe (pure ExitSuccess) (`shell` empty) handler
handleButton _       _          = pure ExitSuccess
