#!/usr/bin/env stack
-- stack runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (lines, pack, unpack)
import           Turtle
import           Common

type CardName = Text
type CardDev = Text
data BlockType = Soft | Hard | SoftAndHard | NotBlocked deriving Show
type Index = Int
newtype Card = Card (Index, CardName, CardDev, BlockType) deriving Show
data AirplaneAction = Activate | Deactivate deriving (Eq, Show)

main :: IO ()
main = sh $ do
  leftClicked <- buttonClicked LeftClick
  rfkill <- rfkill'
  cards <- return $ head $ match (parseCards rfkill) rfkill
  case isAirplaneMode cards of
    True -> do
      liftIO $ putStrLn "\61554"
      handleLeftClick leftClicked Deactivate cards
    False -> do
      liftIO $ putStrLn "\61554 x"
      handleLeftClick leftClicked Activate cards

handleLeftClick :: Bool -> AirplaneAction -> [Card] -> Shell [ExitCode]
handleLeftClick True action cards = sequence $ (blockCards action) cards
handleLeftClick _ _ _ = return []

blockCards :: AirplaneAction -> [Card] -> [Shell ExitCode]
blockCards act cards = ((actionCard act) . indexCard) <$> cards

indexCard :: Card -> Index
indexCard (Card (i, _, _, _)) = i

actionCard :: (MonadIO io, Show a) => AirplaneAction -> a -> io ExitCode
actionCard Deactivate i = shell (pack $ "rfkill block " ++ show i) empty
actionCard Activate i = shell (pack $ "rfkill unblock " ++ show i) empty

isAirplaneMode :: [Card] -> Bool
isAirplaneMode cards = all isBlocked cards
  where
    isBlocked (Card (_, _, _, NotBlocked)) = False
    isBlocked _ = True

parseCards :: Text -> Pattern [Card]
parseCards rfkill =
  let cardCount = countCards rfkill
  in bounded cardCount cardCount parseCard
  where
    countCards = (`div` 3) . length . Data.Text.lines

parseCard :: Pattern Card
parseCard = do
  index <- decimal <* separator
  cardDev <- star alphaNum <* separator
  cardName <- chars1 <* newline
  soft <- tab *> "Soft blocked" *> separator *> ("yes" <|> "no") <* newline
  hard <- tab *> "Hard blocked" *> separator *> ("yes" <|> "no") <* newline
  return $ Card (index, cardName, cardDev, toBlockType soft hard)
  where
    separator = skip (":" *> spaces1)
    toBlockType "yes" "yes" = SoftAndHard
    toBlockType "yes" _ = Soft
    toBlockType _ "yes" = Hard
    toBlockType _ _ = NotBlocked

rfkill' :: Shell Text
rfkill' = strict $ inshell (pack "rfkill list") empty
