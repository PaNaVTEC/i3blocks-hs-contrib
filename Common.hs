module Common where

import           Turtle
import           Data.Text (pack)

processIsRunning :: String -> Shell Bool
processIsRunning process = ((== ExitSuccess) . fst) <$> shellStrict (pack $ "pidof " ++ process) empty
