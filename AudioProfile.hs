{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Bool
import           Data.Maybe
import           Data.Text  (pack, unpack)
import           Turtle

newtype Input = Input Text deriving Show
newtype Output = Output Text deriving Show
newtype AudioProfile = AudioProfile (Maybe Input, Output) deriving Show

main :: IO ()
main = sh $ do
  profile <- last <$> match parseAudioProfile <$> outputSource
  liftIO $ putStrLn $ formatProfile profile
  button <- currentButton
  handleButton button

handleButton :: MonadIO io => Maybe Button -> io ExitCode
handleButton (Just LeftClick)  = shell "pavucontrol" empty
handleButton (Just RightClick) = shell "ponymix toggle >/dev/null" empty
handleButton (Just WheelUp)    = shell "ponymix increase 5 >/dev/null" empty
handleButton (Just WheelDown)  = shell "ponymix decrease 5 >/dev/null" empty
handleButton _                 = return ExitSuccess

formatProfile :: AudioProfile -> String
formatProfile (AudioProfile (_, (Output out))) = unpack out

outputSource :: Shell Text
outputSource = lineToText <$> inshell (pack "ponymix get-profile") empty

parseAudioProfile :: Pattern AudioProfile
parseAudioProfile = do
  output' <- "output:" *> chars1
  input' <- optional $ "+input:" *> chars1
  return $ AudioProfile (bool Nothing (Input <$> input') (isJust input')
                        , Output output')
