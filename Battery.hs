#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (pack, unpack)
import           Numeric
import           Turtle

newtype BatteryPercentage = BatteryPercentage Integer
newtype BatteryStatus = BatteryStatus String
data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  acpi <- acpi'
  info <- return $ parse acpi
  liftIO $ putStrLn $ formatBattery info

formatBattery :: (BatteryPercentage, BatteryStatus) -> String
formatBattery (BatteryPercentage per, _) = icon per ++ " " ++ show per ++ "%"
  where
    icon per | per >= 90 = "\62016" -- 4/4
    icon per | per >= 75 = "\62017" -- 3/4
    icon per | per >= 50 = "\62018" -- 2/4
    icon per | per >= 25 = "\62019" -- 1/4
    icon per | per >= 00 = "\62020" -- 0/4

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
  return (BatteryPercentage per, BatteryStatus $ unpack state)

acpi' :: Shell Text
acpi' = strict $ inshell (pack "acpi") empty
