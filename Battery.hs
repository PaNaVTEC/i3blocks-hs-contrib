#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack)
import           Numeric
import           Turtle

newtype BatteryPercentage = BatteryPercentage Integer
data BatteryStatus = Discharging | Charging | Plugged
data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  acpi <- acpi'
  info <- return $ parse acpi
  liftIO $ putStrLn $ formatBattery info

formatBattery :: (BatteryPercentage, BatteryStatus) -> String
formatBattery (BatteryPercentage per, Discharging) = icon per ++ " " ++ show per ++ "%"
  where
    icon per | per >= 90 = "\62016" -- 4/4
    icon per | per >= 75 = "\62017" -- 3/4
    icon per | per >= 50 = "\62018" -- 2/4
    icon per | per >= 25 = "\62019" -- 1/4
    icon per | per >= 00 = "\62020" -- 0/4
formatBattery (BatteryPercentage per, _) =  "\61926 " ++ show per ++ "%"

parse :: Text -> (BatteryPercentage, BatteryStatus)
parse acpi = head $ match batteryLeft acpi

batteryLeft :: Pattern (BatteryPercentage, BatteryStatus)
batteryLeft = do
  "Battery"
  spaces1
  decimal
  ":"
  spaces1
  state <- chars1
  ","
  spaces1
  per <- decimal
  "%,"
  spaces1
  star anyChar
  return (BatteryPercentage per, toBatteryStatus state)
  where
    toBatteryStatus "Discharging" = Discharging
    toBatteryStatus "Charging" = Charging
    toBatteryStatus "Plugged" = Charging

acpi' :: Shell Text
acpi' = strict $ inshell (pack "acpi") empty
