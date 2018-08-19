{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Control.Applicative (liftA3)
import           Data.Bool           (bool)
import           Data.Text           (pack)
import           Turtle

main :: IO ()
main = sh $ processIsRunning "openvpn" >>=
  bool (printCommand "x") (printCommand "✓")

printCommand :: String -> Shell ()
printCommand out = liftIO $ putStrLn $ "\61676" ++ " " ++ out
