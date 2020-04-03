{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative       (liftA3)
import           Control.Exception         (try)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Bool                 (bool)
import           Data.Foldable             (fold)
import           Data.Functor              (($>))
import           Data.List                 (intersperse)
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (null, pack, strip, unpack)
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           Prelude                   hiding (FilePath, putStrLn)
import           Text.Printf               (printf)
import           Data.Text.IO              (putStrLn)
import           Turtle                    hiding (empty, fold, printf)
import           Data.Maybe
import           System.Environment (getArgs)

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
main = do
  icons <- getArgs
  sh
          $   defaultInterface
          >>= maybe handleNoInterface (runScript icons)
          >>= liftIO
          .   putStrLn
  where
    handleNoInterface = return "No UP interface"

initPath :: NetworkInterface -> Shell FilePath
initPath interface = do
        mktree dataPath
        touch $ dataPathForInterface interface
        return $ dataPathForInterface interface

runScript :: [String] -> NetworkInterface -> Shell Text
runScript icons interface = do
  _ <- initPath interface
  record <- readRecord interface
  maybe handleNoRecordAvailable handleRecord record
    where
      handleRecord oldRecord =
          formatReport icons
          .   applyBestUnit
          .   speedReport oldRecord
          <$> writeRecord interface
      handleNoRecordAvailable = do
        _ <- writeRecord interface
        return $ "No data for " <> (pack $ show interface)

writeRecord :: NetworkInterface -> Shell Record
writeRecord interface =
        liftA3 Record
               (liftIO poxisTimeAsInteger)
               (readBytesReceived interface)
               (readBytesTransfered interface)
                >>= liftIO
                .   writeRecordToFile
    where
        recordToText record =
                pack
                        . fold
                        . intersperse " "
                        $ [ show . timestamp $ record
                          , show . runTotalBytesIn . bytesReceived $ record
                          , show . runTotalBytesOut . bytesSent $ record
                          , "\n"
                          ]
        poxisTimeAsInteger = round <$> getPOSIXTime
        writeRecordToFile record =
                ( writeTextFile (dataPathForInterface interface)
                        . recordToText
                        $ record
                        )
                        $> record

readRecord :: NetworkInterface -> Shell (Maybe Record)
readRecord interface = liftIO . runMaybeT $ extractRecord =<< safeReadTextFile
    where
        safeReadTextFile =
                MaybeT
                        $   tryToMaybe
                        <$> try
                                    (strip <$> readTextFile
                                            (dataPathForInterface interface)
                                    )
        extractRecord =
                MaybeT . return . matchToMaybe . match (decimal `sepBy` " ")
        matchToMaybe = \case
                [[time', received, sent]] -> Just $ Record
                        time'
                        (TotalBytesIn received)
                        (TotalBytesOut sent)
                _ -> Nothing
        tryToMaybe :: Either IOError b -> Maybe b
        tryToMaybe (Left  _)  = Nothing
        tryToMaybe (Right x') = Just x'

speedReport :: Record -> Record -> Report
speedReport oldRecord newRecord = Report (UploadRate uploadRate')
                                         (DownloadRate downloadRate')
    where
        timediff = max (timestamp newRecord - timestamp oldRecord) 1
        bytesReceivedDiff =
                runTotalBytesIn (bytesReceived newRecord)
                        - runTotalBytesIn (bytesReceived oldRecord)
        bytesSentDiff =
                runTotalBytesOut (bytesSent newRecord)
                        - runTotalBytesOut (bytesSent oldRecord)
        downloadRate' = TransferRate
                (fromIntegral bytesReceivedDiff / fromIntegral timediff)
                Bs
        uploadRate' = TransferRate
                (fromIntegral bytesSentDiff / fromIntegral timediff)
                Bs

getUploadIcon :: [String] -> Maybe Text
getUploadIcon icons = case icons of
  uploadIcon : _ -> Just $ pack uploadIcon
  _              -> Nothing

getDownloadIcon :: [String] -> Maybe Text
getDownloadIcon icons = case icons of
  _ : downloadIcon : _ -> Just $ pack downloadIcon
  _                    -> Nothing

formatReport :: [String] -> Report -> Text
formatReport icons report =
  let downloadIcon = fromMaybe "Rx " $ getDownloadIcon icons
      uploadIcon   = fromMaybe "Tx " $ getUploadIcon icons
  in  downloadIcon
                <> ( pack
                   . printf "%.1f"
                   . value
                   . runDownloadRate
                   . downloadRate
                   $ report
                   )
                <> (pack . show . unit . runDownloadRate . downloadRate $ report)
                <> "  "
                <> uploadIcon
                <> ( pack
                   . printf "%.1f"
                   . value
                   . runUploadRate
                   . uploadRate
                   $ report
                   )
                <> (pack . show . unit . runUploadRate . uploadRate $ report)

applyBestUnit :: Report -> Report
applyBestUnit = liftA2
        Report
        (UploadRate . applyBestUnit' . runUploadRate . uploadRate)
        (DownloadRate . applyBestUnit' . runDownloadRate . downloadRate)
    where
        applyBestUnit' transferRate | value transferRate < 1024 = transferRate
        applyBestUnit' transferRate = applyBestUnit'
                $ convertRate transferRate (succ . unit $ transferRate)

convertRate :: TransferRate -> TransferUnit -> TransferRate
convertRate rate to | unit rate < to = convertRate (convertRateUp rate) to
    where
        convertRateUp (TransferRate rate' from)
          = TransferRate (rate' / 1024) (succ from)
convertRate rate' to | unit rate' > to = convertRate (convertRateDown rate') to
    where
        convertRateDown (TransferRate rate from)
          = TransferRate (rate * 1024) (pred from)
convertRate rate _ = rate

defaultInterface :: Shell (Maybe NetworkInterface)
defaultInterface =
        textToMaybe . strip
                <$> strict (inshell
                            "ip addr | awk '/state UP/ {print $2}' | sed 's/.$//'"
                            mempty
                    )
        where textToMaybe text' = bool (Just text') Nothing (Data.Text.null text')

readBytesTransfered :: Text -> Shell TotalBytesOut
readBytesTransfered interface =
        TotalBytesOut . read . unpack . strip
                <$> strict (inshell
                            (  "cat /sys/class/net/"
                            <> interface
                            <> "/statistics/tx_bytes"
                            )
                            mempty
                    )

readBytesReceived :: Text -> Shell TotalBytesIn
readBytesReceived interface =
        TotalBytesIn . read . unpack . strip
                <$> strict (inshell
                            (  "cat /sys/class/net/"
                            <> interface
                            <> "/statistics/rx_bytes"
                            )
                            mempty
                    )
