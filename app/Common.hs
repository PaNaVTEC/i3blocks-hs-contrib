{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Turtle
import           Data.Text (pack)
import           Numeric

processIsRunning :: String -> Shell Bool
processIsRunning process = ((== ExitSuccess) . fst) <$> shellStrict (pack $ "pidof " ++ process) empty

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

data Button = LeftClick | RightClick | WheelUp | WheelDown | WheelLeft | WheelRight deriving (Eq, Show)

currentButton :: MonadIO io => io (Maybe Button)
currentButton = maybe Nothing toButton <$> (env >>= return . lookup "BLOCK_BUTTON")

toButton :: Text -> Maybe Button
toButton "1" = Just LeftClick
toButton "3" = Just RightClick
toButton "4" = Just WheelUp
toButton "5" = Just WheelDown
toButton "6" = Just WheelLeft
toButton "7" = Just WheelRight
toButton _ = Nothing

buttonClicked :: MonadIO io => Button -> io Bool
buttonClicked but = toBoolean <$> currentButton
  where
    toBoolean = maybe False (== but)
