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
import           Turtle                    hiding (empty, fold, printf)

-- TODO CHECK what happens if i3blocks-bandwidth-module dir does not exist

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

dataPathForInterface :: NetworkInterface -> FilePath
dataPathForInterface interface = dataPath </> fromText interface

dataPath :: FilePath
dataPath = "/dev/shm/i3blocks-bandwidth-module"

main :: IO ()
main = sh $
  defaultInterface
  >>= maybe handleNoInterface runScript
  >>= liftIO . putStrLn
  where handleNoInterface = return "No interface"

initPath :: NetworkInterface -> Shell FilePath
initPath interface = do
  mktree dataPath
  touch $ dataPathForInterface interface
  return $ dataPathForInterface interface

runScript :: NetworkInterface -> Shell String
runScript interface = do
  initPath interface
  liftA3 bool (handleInterfaceDown interface) (handleInterfaceUp interface) (isUp interface)

handleInterfaceDown :: NetworkInterface -> Shell String
handleInterfaceDown interface = return . unpack $ interface <> " is down"

handleInterfaceUp :: NetworkInterface -> Shell String
handleInterfaceUp interface = maybe handleNoRecordAvailable handleRecord
                                   =<< readRecord interface
  where handleRecord oldRecord = return . formatReport . applyBestUnit . speedReport oldRecord
                                 =<< writeRecord interface
        handleNoRecordAvailable = writeRecord interface >>= return "No data"

writeRecord :: NetworkInterface -> Shell Record
writeRecord interface =
  liftA3 Record (liftIO poxisTimeAsInteger) (readBytesReceived interface) (readBytesTransfered interface)
  >>= liftIO . writeRecordToFile
  where recordToText record = pack . fold . intersperse " " $
          [show . timestamp $ record,
           show . runTotalBytesIn . bytesReceived $ record,
           show . runTotalBytesOut . bytesSent $ record,
           "\n"]
        poxisTimeAsInteger = round <$> getPOSIXTime
        writeRecordToFile record = (writeTextFile (dataPathForInterface interface) . recordToText $ record)
          *> return record

readRecord :: NetworkInterface -> Shell (Maybe Record)
readRecord interface = liftIO . runMaybeT $ extractRecord =<< safeReadTextFile
  where safeReadTextFile = MaybeT $ tryToMaybe <$> try (strip <$> readTextFile (dataPathForInterface interface))
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
isUp interface = let state = inshell ("cat /sys/class/net/" <> interface <> "/operstate") mempty
                 in ("up" ==) . lineToText <$> state

defaultInterface :: Shell (Maybe NetworkInterface)
defaultInterface = textToMaybe . strip <$>
  (strict $ inshell ("ip route | awk '/^default/ { print $5 ; exit }'") mempty)
  where textToMaybe text = bool (Just text) Nothing (Data.Text.null text)

readBytesTransfered :: Text -> Shell TotalBytesOut
readBytesTransfered interface = (TotalBytesOut . read . unpack . strip) <$>
  (strict $ inshell ("cat /sys/class/net/" <> interface <> "/statistics/tx_bytes") mempty)

readBytesReceived :: Text -> Shell TotalBytesIn
readBytesReceived interface = (TotalBytesIn . read . unpack . strip) <$>
  (strict $ inshell ("cat /sys/class/net/" <> interface <> "/statistics/rx_bytes") mempty)
