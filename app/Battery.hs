{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack)
import           Turtle

newtype BatteryPercentage = BatteryPercentage Integer
data BatteryStatus = Discharging | Charging | Plugged | Unknown
data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  acpi <- acpi'
  info <- return $ parse acpi
  liftIO $ putStrLn $ formatBattery info

formatBattery :: (BatteryPercentage, BatteryStatus) -> String
formatBattery (BatteryPercentage per, Discharging) = icon per ++ " " ++ show per ++ "%"
  where
    icon p | p >= 90 = "\62016"
    icon p | p >= 75 = "\62017"
    icon p | p >= 50 = "\62018"
    icon p | p >= 25 = "\62019"
    icon _ = "\62020"
formatBattery (BatteryPercentage per, Unknown) =  "B? " ++ show per ++ "%"
formatBattery (BatteryPercentage per, _) =  "\61926 " ++ show per ++ "%"

parse :: Text -> (BatteryPercentage, BatteryStatus)
parse acpi = head $ match batteryLeft acpi

batteryLeft :: Pattern (BatteryPercentage, BatteryStatus)
batteryLeft = do
  batteryNumber
  state <- spaces1 *> chars1 <* ","
  per <- spaces1 *> decimal <* "%" <* ("," <|> "") <* spaces1 <* star anyChar
  return (BatteryPercentage per, toBatteryStatus state)
  where
    batteryNumber = "Battery" *> spaces1 *> decimal *> ":"
    toBatteryStatus "Discharging" = Discharging
    toBatteryStatus "Charging"    = Charging
    toBatteryStatus "Plugged"     = Plugged
    toBatteryStatus _ = Unknown

acpi' :: Shell Text
acpi' = strict $ inshell (pack "acpi") empty
