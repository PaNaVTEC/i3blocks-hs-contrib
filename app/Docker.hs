{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Text (pack)
import           Turtle

main :: IO ()
main = sh $ do
  isRunning <- processIsRunning "dockerd"
  case isRunning of
    False -> formatCommand (return "x")
    True  -> formatCommand (show <$> nImages)

formatCommand :: Shell String -> Shell ()
formatCommand out = do
  out' <- out
  liftIO $ putStrLn $ "\61875" ++ " " ++ out'

nImages :: Shell Integer
nImages = fold (inshell (pack "docker ps -q") empty) countLines
