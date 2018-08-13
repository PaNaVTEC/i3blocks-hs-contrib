#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Bool
import           Data.Text (lines, pack)
import           Turtle

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
  cards <- return $ head $ match (parseCards rfkill) rfkill
  let isAirplaneOn = isAirplaneMode cards
  bool (airplaneModeOff cards) (airplaneModeOn cards) isAirplaneOn
  where
    airplaneModeOn cards = do liftIO $ putStrLn "\61554"; handleClick Deactivate cards
    airplaneModeOff cards = do liftIO $ putStrLn "\61554 x"; handleClick Activate cards

handleClick :: AirplaneAction -> [Card] -> Shell [ExitCode]
handleClick action cards = do
  leftClicked <- buttonClicked LeftClick
  bool (return []) (sequence $ (blockCards action) cards) leftClicked

blockCards :: AirplaneAction -> [Card] -> [Shell ExitCode]
blockCards act cards = (actionCard act . index) <$> cards

actionCard :: (MonadIO io, Show a) => AirplaneAction -> a -> io ExitCode
actionCard Deactivate i = shell (pack $ "rfkill unblock " ++ show i) empty
actionCard Activate i   = shell (pack $ "rfkill block " ++ show i) empty

isAirplaneMode :: [Card] -> Bool
isAirplaneMode cards = all ((/= NotBlocked) . blockType) cards

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
