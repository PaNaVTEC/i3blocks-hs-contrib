{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Data.Text    (pack, unpack)
import           Data.Text.IO (putStrLn)
import           Numeric
import           Turtle

processIsRunning :: String -> Shell Bool
processIsRunning process = (== ExitSuccess) . fst <$> shellStrict (pack $ "pidof " ++ process) empty

formatFloatN :: RealFloat a => a -> Int -> String
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

data Button = LeftClick | RightClick | WheelUp | WheelDown | WheelLeft | WheelRight deriving (Eq, Show)

currentButton :: MonadIO io => io (Maybe Button)
currentButton = maybe Nothing toButton <$> (lookup "BLOCK_BUTTON" <$> env)

toButton :: Text -> Maybe Button
toButton "1" = Just LeftClick
toButton "3" = Just RightClick
toButton "4" = Just WheelUp
toButton "5" = Just WheelDown
toButton "6" = Just WheelLeft
toButton "7" = Just WheelRight
toButton _   = Nothing

buttonClicked :: MonadIO io => Button -> io Bool
buttonClicked but = (== Just but) <$> currentButton

newtype LongDesc = LongDesc Text deriving (Eq, Show)
newtype ShortDesc = ShortDesc Text deriving (Eq, Show)
newtype Color = Color Text deriving (Eq, Show)
data OutputReport = OutputReport {
  longDesc  :: LongDesc,
  shortDesc :: Maybe ShortDesc,
  color     :: Maybe Color
}

blockOutput :: MonadIO io => OutputReport -> io ()
blockOutput report = do
  let longDesc' = toText . longDesc $ report
  liftIO $ Data.Text.IO.putStrLn longDesc'
  liftIO $ Data.Text.IO.putStrLn $ maybe longDesc' toText' (shortDesc report)
  liftIO $ Data.Text.IO.putStrLn $ maybe "" toText'' (color report)
  where
    toText (LongDesc t) = t
    toText' (ShortDesc t) = t
    toText'' (Color t) = t
