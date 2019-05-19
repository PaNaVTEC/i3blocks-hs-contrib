{-# LANGUAGE OverloadedStrings #-}

import           Common
import           Data.Text (pack, strip, unpack)
import           Turtle

main :: IO ()
main = sh $ do
  liftIO . putStrLn =<< cpuUsage
  maybe (return ExitSuccess) handleButton =<< currentButton

cpuUsage :: Shell String
cpuUsage = format <$> idleCpu
  where format idle =
          let usage = formatFloatN (100 - idle) 0
          in "\61668 " ++ usage ++ "%"

handleButton :: MonadIO io => Button -> io ExitCode
handleButton LeftClick = shell "urxvt -title pop-up -e htop" empty
handleButton _         = return ExitSuccess

idleCpu :: Shell Double
idleCpu = read . unpack . strip <$>
  strict (inshell (pack "mpstat 1 1 -o JSON | jq -r '.sysstat.hosts[0].statistics[0].\"cpu-load\"[0].idle'") empty)
