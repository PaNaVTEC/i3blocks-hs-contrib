{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Text (pack, strip, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  idle <- idleCpu
  liftIO $ putStrLn $ "\61668 " ++ formatFloatN (100 - idle) 2 ++ "%"
  button <- currentButton
  handleButton button

handleButton :: MonadIO io => Maybe Button -> io ExitCode
handleButton (Just LeftClick) = shell "urxvt -title pop-up -e htop" empty
handleButton _                = return ExitSuccess

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$> (strict $ inshell (pack "iostat -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"avg-cpu\".idle'") empty)
