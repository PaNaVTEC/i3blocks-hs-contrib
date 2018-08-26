{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative       (liftA3)
import           Control.Exception         (try)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Bool                 (bool)
import           Data.Foldable             (fold)
import           Data.List                 (intersperse)
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (null, pack, strip, unpack)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Prelude                   hiding (FilePath)
import           Text.Printf               (printf)
import           Turtle                    hiding (fold, printf)

data Report = Report {
    uploadRate   :: UploadRate,
    downloadRate :: DownloadRate
  } deriving Show

newtype DownloadRate = DownloadRate { runDownloadRate :: TransferRate } deriving Show
newtype UploadRate = UploadRate { runUploadRate :: TransferRate } deriving Show

data TransferRate = TransferRate {
    value :: Double,
    unit  :: TransferUnit
  } deriving (Show, Eq)

data TransferUnit = Bs | KBs | MBs deriving (Show, Eq, Enum, Ord)

data Record = Record {
    timestamp     :: Timestamp,
    bytesReceived :: TotalBytesIn,
    bytesSent     :: TotalBytesOut
  } deriving Show

newtype TotalBytesIn = TotalBytesIn { runTotalBytesIn :: Integer } deriving (Show, Read)
newtype TotalBytesOut = TotalBytesOut { runTotalBytesOut :: Integer } deriving (Show, Read)
type Timestamp = Integer
type NetworkInterface = Text

path :: FilePath
path = "/dev/shm/i3-blocks-bandwidth-module"

main :: IO ()
main = sh $
  defaultInterface
  >>= maybe handleNoInterface runScript
  >>= liftIO . putStrLn
  where handleNoInterface = return "No interface"

runScript :: NetworkInterface -> Shell String
runScript interface = liftA3 bool (handleInterfaceDown interface) (handleInterfaceUp interface path) (isUp interface)

handleInterfaceDown :: NetworkInterface -> Shell String
handleInterfaceDown interface = return . unpack $ interface <> " is down"

handleInterfaceUp :: NetworkInterface -> FilePath -> Shell String
handleInterfaceUp interface path = maybe handleNoRecordAvailable handleRecord
                                   =<< readRecord path
  where handleRecord oldRecord = return . formatReport . applyBestUnit . speedReport oldRecord
                                 =<< writeRecord interface path
        handleNoRecordAvailable = writeRecord interface path >>= return "No data"

writeRecord :: NetworkInterface -> FilePath -> Shell Record
writeRecord interface path =
  liftA3 Record (liftIO poxisTimeAsInteger) (readBytesReceived interface) (readBytesTransfered interface)
  >>= liftIO . writeRecordToFile
  where recordToText record = pack . fold . intersperse " " $
          [show . timestamp $ record,
           show . runTotalBytesIn . bytesReceived $ record,
           show . runTotalBytesOut . bytesSent $ record,
           "\n"]
        poxisTimeAsInteger = round <$> getPOSIXTime
        writeRecordToFile record = do
          (writeTextFile path) . recordToText $ record
          return record

readRecord :: FilePath -> Shell (Maybe Record)
readRecord path = liftIO . runMaybeT $ extractRecord =<< safeReadTextFile
  where safeReadTextFile = MaybeT $ tryToMaybe <$> try (strip <$> readTextFile path)
        extractRecord = MaybeT . return . matchToMaybe . match (decimal `sepBy` " ")
        matchToMaybe x = case x of
         [[time, received, sent]] -> Just $ Record time (TotalBytesIn received) (TotalBytesOut sent)
         _                        -> Nothing
        tryToMaybe :: Either IOError b -> Maybe b
        tryToMaybe (Left _)  = Nothing
        tryToMaybe (Right x) = Just x

speedReport :: Record -> Record -> Report
speedReport oldRecord newRecord = Report (UploadRate uploadRate) (DownloadRate downloadRate)
  where timediff          = max (timestamp newRecord - timestamp oldRecord) 1
        bytesReceivedDiff = (runTotalBytesIn $ bytesReceived newRecord) - (runTotalBytesIn $ bytesReceived oldRecord)
        bytesSentDiff     = (runTotalBytesOut $ bytesSent newRecord) - (runTotalBytesOut $ bytesSent oldRecord)
        downloadRate      = TransferRate (fromIntegral bytesReceivedDiff / fromIntegral timediff) Bs
        uploadRate        = TransferRate (fromIntegral bytesSentDiff / fromIntegral timediff) Bs

formatReport :: Report -> String
formatReport report =
  "\61677 " ++
  ((printf "%.1f") . value . runDownloadRate . downloadRate $ report) ++
  (show . unit . runDownloadRate . downloadRate $ report) ++
  "  " ++
  "\61678 " ++
  ((printf "%.1f") . value . runUploadRate . uploadRate $ report) ++
  (show . unit . runUploadRate . uploadRate $ report)

applyBestUnit :: Report -> Report
applyBestUnit = liftA2 Report
  (UploadRate . applyBestUnit' . runUploadRate . uploadRate)
  (DownloadRate . applyBestUnit' . runDownloadRate . downloadRate)
  where applyBestUnit' transferRate | value transferRate < 1024 = transferRate
        applyBestUnit' transferRate | otherwise = applyBestUnit' $ convertRate transferRate (succ . unit $ transferRate)

convertRate :: TransferRate -> TransferUnit -> TransferRate
convertRate rate to | unit rate < to = convertRate (convertRateUp rate) to
  where convertRateUp (TransferRate rate from) = (TransferRate (rate / 1024) (succ from))
convertRate rate to | unit rate > to = convertRate (convertRateDown rate) to
  where convertRateDown (TransferRate rate from) = (TransferRate (rate * 1024) (pred from))
convertRate rate _  | otherwise      = rate

isUp :: Text -> Shell Bool
isUp interface = (== "up") . lineToText <$> inshell command mempty
  where command = ("cat /sys/class/net/" <> interface <> "/operstate")

defaultInterface :: Shell (Maybe Text)
defaultInterface = safeHead . match parseInterface <$> onlyDefault

parseInterface :: Pattern Text
parseInterface = "default via " *> ip *> spaces1 *> "dev " *> (star alphaNum) <* star anyChar
  where
    ip = segment >> "." >> segment >> "." >> segment >> "." >> segment
    segment = bounded 1 3 (decimal :: Pattern Integer)

onlyDefault :: Shell Text
onlyDefault = strict $ grep parseLineWithDefault (inshell "ip route" mempty)
  where
    parseLineWithDefault = "default" *> star anyChar

readBytesTransfered :: Text -> Shell TotalBytesOut
readBytesTransfered interface = TotalBytesOut <$> readNumberFrom command
  where command = "cat /sys/class/net/" <> interface <> "/statistics/tx_bytes"

readBytesReceived :: Text -> Shell TotalBytesIn
readBytesReceived interface = TotalBytesIn <$> readNumberFrom command
  where command = "cat /sys/class/net/" <> interface <> "/statistics/rx_bytes"

readNumberFrom :: Text -> Shell Integer
readNumberFrom command = read . unpack . strip <$> (strict $ inshell command mempty)

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (a : _) = Just a
