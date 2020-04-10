{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Common
import           Data.Text (pack)
import           Turtle
import           Data.Maybe (fromMaybe)
import           System.Environment (getArgs)

newtype BatteryPercentage = BatteryPercentage Integer
data BatteryStatus = Discharging | Charging | Full | Plugged | Unknown
data MemType = MemTotal | MemFree

main :: IO ()
main = sh $ do
  acpi <- acpi'
  icons <- liftIO getArgs
  let info = parse acpi
  maybe (renderNoBattery icons) (renderOutput icons) info
  where
    renderNoBattery icons =
      let desc = fromMaybe "No Battery" $ getNoBatteryIcon icons
      in blockOutput $ OutputReport (LongDesc desc) Nothing Nothing
    renderOutput icons info
      = blockOutput $ OutputReport (makeLongDesc info) Nothing (makeColor $ fst info)
      where
        makeLongDesc = LongDesc . formatBattery icons
        makeColor (BatteryPercentage per) = Color <$> batteryColor per

formatBattery :: [String] -> (BatteryPercentage, BatteryStatus) -> Text
formatBattery icons info = case info of
  (BatteryPercentage per, Discharging) -> format' (getIcon icons per) per
  (BatteryPercentage per, Unknown)     -> format' (getIcon icons per) per
  (BatteryPercentage per, _)           -> format' (fromMaybe "" $ getFullBatteryIcon icons) per

format' :: Show a => Text -> a -> Text
format' icon per = icon <> (pack $ show per) <> "%"

getIcon :: [String] -> Integer -> Text
getIcon icons p | p >= 90 = fromMaybe "" $ getHighBatteryIcon icons
getIcon icons p | p >= 75 = fromMaybe "" $ getMediumBatteryIcon icons
getIcon icons p | p >= 25 = fromMaybe "" $ getLowBatteryIcon icons
getIcon icons _ = fromMaybe "" $ getEmptyBatteryIcon icons

getNoBatteryIcon :: [String] -> Maybe Text
getNoBatteryIcon icons = case icons of
  icon : _ -> Just $ pack icon
  _        -> Nothing

getEmptyBatteryIcon :: [String] -> Maybe Text
getEmptyBatteryIcon icons = case icons of
  _ : icon : _ -> Just $ pack icon
  _            -> Nothing

getLowBatteryIcon :: [String] -> Maybe Text
getLowBatteryIcon icons = case icons of
  _ : _ : icon : _ -> Just $ pack icon
  _                -> Nothing

getMediumBatteryIcon :: [String] -> Maybe Text
getMediumBatteryIcon icons = case icons of
  _ : _ : _ : icon : _ -> Just $ pack icon
  _                    -> Nothing

getHighBatteryIcon :: [String] -> Maybe Text
getHighBatteryIcon icons = case icons of
  _ : _ : _ : _ : icon : _ -> Just $ pack icon
  _                        -> Nothing

getFullBatteryIcon :: [String] -> Maybe Text
getFullBatteryIcon icons = case icons of
  _ : _ : _ : _ : _ : icon : _ -> Just $ pack icon
  _                            -> Nothing

batteryColor :: Integer -> Maybe Text
batteryColor p | p <= 25 = Just "#ff0000"
batteryColor _ = Nothing

parse :: Text -> Maybe (BatteryPercentage, BatteryStatus)
parse acpi = case match batteryLeft acpi of
  [] -> Nothing
  info:_ -> Just info

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
