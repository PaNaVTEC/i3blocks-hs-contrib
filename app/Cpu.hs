{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Text (pack, strip, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  liftIO . putStrLn =<< cpuUsage
  maybe (return ExitSuccess) handleButton =<< currentButton

cpuUsage :: Shell String
cpuUsage = idleCpu >>= return . format
 where format idle = let usage = (formatFloatN (100 - idle) 2)
                     in "\61668 " ++ usage ++ "%"

handleButton :: MonadIO io => Button -> io ExitCode
handleButton LeftClick = shell "urxvt -title pop-up -e htop" empty
handleButton _         = return ExitSuccess

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$> (strict $ inshell (pack "iostat -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"avg-cpu\".idle'") empty)
