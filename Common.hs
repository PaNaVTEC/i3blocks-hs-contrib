module Common where

import           Turtle
import           Data.Text (pack)
import           Numeric

processIsRunning :: String -> Shell Bool
processIsRunning process = ((== ExitSuccess) . fst) <$> shellStrict (pack $ "pidof " ++ process) empty

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""
