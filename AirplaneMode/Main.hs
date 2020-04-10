{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common
import           Data.Bool
import           Data.Text (lines, pack)
import           Data.Text.IO (putStrLn)
import           Prelude      hiding (putStrLn)
import           Turtle
import           System.Environment (getArgs)
import qualified Data.Text as Text
import           Data.Maybe

type CardName = Text
type CardDev = Text
data BlockType = Soft | Hard | SoftAndHard | NotBlocked deriving (Eq, Show)
type Index = Int
data Card = Card { index     :: Index,
                   cardName  :: CardName,
                   cardDev   :: CardDev,
                   blockType :: BlockType } deriving Show
data AirplaneAction = Activate | Deactivate deriving (Eq, Show)

main :: IO ()
main = sh $ do
  rfkill <- rfkill'
  let cards = head $ match (parseCards rfkill) rfkill
  let isAirplaneOn = isAirplaneMode cards
  icons <- liftIO getArgs
  bool (airplaneModeOff icons cards) (airplaneModeOn icons cards) isAirplaneOn
  where
    airplaneModeOn icons cards = do
      liftIO $ putStrLn $ fromMaybe "Airplane On" $ getAirplaneOnIcon icons
      handleClick Deactivate cards
    airplaneModeOff icons cards = do
      liftIO $ putStrLn $ fromMaybe "Airplane Off" $ getAirplaneOffIcon icons
      handleClick Activate cards

getAirplaneOffIcon :: [String] -> Maybe Text
getAirplaneOffIcon icons = case icons of
  airplaneOffIcon : _ -> Just $ Text.pack airplaneOffIcon
  _                   -> Nothing

getAirplaneOnIcon :: [String] -> Maybe Text
getAirplaneOnIcon icons = case icons of
  _ : airplaneOnIcon : _ -> Just $ Text.pack airplaneOnIcon
  _                      -> Nothing

handleClick :: AirplaneAction -> [Card] -> Shell [ExitCode]
handleClick action cards = do
  leftClicked <- buttonClicked LeftClick
  bool (return []) (sequence $ blockCards action cards) leftClicked

blockCards :: AirplaneAction -> [Card] -> [Shell ExitCode]
blockCards act cards = actionCard act . index <$> cards

actionCard :: (MonadIO io, Show a) => AirplaneAction -> a -> io ExitCode
actionCard Deactivate i = shell (pack $ "rfkill unblock " ++ show i) empty
actionCard Activate i   = shell (pack $ "rfkill block " ++ show i) empty

isAirplaneMode :: [Card] -> Bool
isAirplaneMode = all ((/= NotBlocked) . blockType)

parseCards :: Text -> Pattern [Card]
parseCards rfkill =
  let cardCount = countCards rfkill
  in bounded cardCount cardCount parseCard
  where
    countCards = (`div` linesPerCard) . length . Data.Text.lines
    linesPerCard = 3

parseCard :: Pattern Card
parseCard = do
  index' <- decimal <* separator
  cardDev' <- star alphaNum <* separator
  cardName' <- chars1 <* newline
  soft <- tab *> "Soft blocked" *> separator *> ("yes" <|> "no") <* newline
  hard <- tab *> "Hard blocked" *> separator *> ("yes" <|> "no") <* newline
  return $ Card index' cardName' cardDev' (toBlockType soft hard)
  where
    separator = skip (":" *> spaces1)
    toBlockType "yes" "yes" = SoftAndHard
    toBlockType "yes" _     = Soft
    toBlockType _ "yes"     = Hard
    toBlockType _ _         = NotBlocked

rfkill' :: Shell Text
rfkill' = strict $ inshell (pack "rfkill list") empty
