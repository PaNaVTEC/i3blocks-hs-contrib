{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle

newtype BatteryPercentage = BatteryPercentage Integer
data BatteryStatus = Discharging | Charging | Full | Plugged | Unknown
data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  acpi <- acpi'
  let info = parse acpi
  blockOutput $ OutputReport (makeLongDesc info) Nothing (makeColor $ fst info)
  where
    makeLongDesc = LongDesc . pack . formatBattery
    makeColor (BatteryPercentage per) = Color <$> batteryColor per

formatBattery :: (BatteryPercentage, BatteryStatus) -> String
formatBattery (BatteryPercentage per, Discharging) = format' (icon per) per
formatBattery (BatteryPercentage per, Unknown)     = format' (icon per) per
formatBattery (BatteryPercentage per, _)           = format' "\61926 " per

format' :: Show a => String -> a -> String
format' i per = i ++ " " ++ show per ++ "%"

icon :: Integer -> String
icon p | p >= 90 = "\62016"
icon p | p >= 75 = "\62017"
icon p | p >= 50 = "\62018"
icon p | p >= 25 = "\62019"
icon _ = "\62020"

batteryColor :: Integer -> Maybe Text
batteryColor p | p <= 25 = Just "#ff0000"
batteryColor _ = Nothing

parse :: Text -> (BatteryPercentage, BatteryStatus)
parse acpi = head $ match batteryLeft acpi

batteryLeft :: Pattern (BatteryPercentage, BatteryStatus)
batteryLeft = do
  _ <- batteryNumber
  state <- spaces1 *> chars1 <* ","
  per <- spaces1 *> decimal <* "%" <* ("," <|> "") <* spaces1 <* star anyChar
  return (BatteryPercentage per, toBatteryStatus state)
  where
    batteryNumber = "Battery" *> spaces1 *> decimal @Integer *> ":"
    toBatteryStatus "Discharging" = Discharging
    toBatteryStatus "Charging"    = Charging
    toBatteryStatus "Plugged"     = Plugged
    toBatteryStatus "Full"        = Full
    toBatteryStatus _             = Unknown

acpi' :: Shell Text
acpi' = strict $ inshell (pack "acpi") empty