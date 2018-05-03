{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | File/directory operations on logs.

module Pos.ReportServer.FileOps
       ( LogsHolder (..)
       , initHolder
       , addEntry
       , storeCustomReport
       ) where

import           Universum

import           Control.Concurrent (modifyMVar_)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))

import           Pos.ReportServer.Exception (ReportServerException (MalformedIndex))
import           Pos.ReportServer.Report (ReportInfo (..), ReportType (..))
import           Pos.ReportServer.Util (prettifyJson, withFileWriteLifted)

import           Pos.ForwardClient.Client (getTicketID)

indexFileName :: FilePath
indexFileName = "index.log"

-- | Date format stored in index file.
indexDateFormat, indexDateFormatOld :: [Char]
indexDateFormat = "%F_%T_%Z_%q"
indexDateFormatOld = "%F_%T_%Z"

-- | Datatype that identifies log storage.
data LogsHolder = LogsHolder
    { lhDir    :: FilePath
      -- ^ Root directory we store everything in.
    , lhIndex  :: FilePath
      -- ^ Path to the index file.
    , lhLastIx :: MVar Int
      -- ^ Id that we can take when processing new report.
    }

-- | Given a report info, generates a (relative) path to directory
-- where data will be stored.
genReportPath :: UTCTime -> ReportInfo -> FilePath
genReportPath curTime ReportInfo{..} =
    case rReportType of
        RCustomReport {} -> "custom-reports" </> date
        _                -> date </> repType </> time
  where
    time = formatTime defaultTimeLocale "%T_%Z_%q" curTime
    date = formatTime defaultTimeLocale "%F" curTime
    repType = case rReportType of
        RCrash _        -> "crash"
        RError _        -> "error"
        RMisbehavior{}  -> "misbehavior"
        RInfo _         -> "info"
        RAnalyse        -> "analysis"
        RCustomReport{} -> error "repType is not ever called with RCustomReport"

-- | Parses single line of index -- returns index id, time item created
-- on and subdir name.
parseIndexEntry :: Text -> Either Text (Int, UTCTime, FilePath)
parseIndexEntry line = case T.splitOn "," line of
    [a,b,c] -> do
        ix <- mToE ("Couldn't read index: " <> a) $ readMaybe $ T.unpack a
        time <-
            mToE ("Couldn't parse utctime: " <> b) $
            parseTimeM True defaultTimeLocale indexDateFormat (T.unpack b) <|>
            parseTimeM True defaultTimeLocale indexDateFormatOld (T.unpack b)
        pure $ (ix, time, toString c)
    _ -> Left $ "Expected csv with 3 argument, got: " <> line
  where
    mToE reason = maybe (Left reason) Right

-- | Initializes logs holder -- opens file, creates index.
initHolder :: FilePath -> IO LogsHolder
initHolder dir = do
    createDirectoryIfMissing True dir
    lastIx <- newMVar =<< ifM (doesFileExist filePath) onExist onCreate
    pure $ LogsHolder dir filePath lastIx
  where
    filePath = dir </> indexFileName
    onExist = do
        tlines <- T.lines <$> TIO.readFile filePath
        case NE.nonEmpty tlines of
            Nothing -> onCreate
            Just ne ->
                either failInit (pure . succ . view _1) $
                parseIndexEntry $ NE.last ne
    onCreate = TIO.writeFile filePath "" $> 0
    failInit = throwM . MalformedIndex

-- | Given logs holder and list of (filename,content), create a new
-- logs dir, dump files there and place an entry to index.
addEntry :: LogsHolder -> ReportInfo -> [(FilePath, LByteString)] -> IO ()
addEntry LogsHolder{..} reportInfo files = do
    curTime <- getCurrentTime
    let reportDir = genReportPath curTime reportInfo
    let fullDirname = lhDir </> reportDir
    let timestamp = formatTime defaultTimeLocale indexDateFormat curTime
    createDirectoryIfMissing True fullDirname
    let filesToWrite = ("payload.json", encodeUtf8 $ prettifyJson reportInfo) : files
    forM_ filesToWrite $ \(fname,content) ->
        BSL.writeFile (fullDirname </> fname) content
    modifyMVar_ lhLastIx $ \i -> do
        let entry =
                T.intercalate ","
                ([show i, T.pack timestamp, T.pack fullDirname])
                <> "\n"
        withFileWriteLifted lhIndex $ TIO.appendFile lhIndex entry
        pure $ i + 1

storeCustomReport ::
       LogsHolder
    -> ReportInfo
    -> [(FilePath, LByteString)]
    -> LByteString
    -> IO ()
storeCustomReport holder reportInfo files zResp = do
    let id = fromMaybe (error "invalid zendesk response") $ getTicketID zResp
    let fullDirname = lhDir holder </> "custom-reports" </> show id
    createDirectoryIfMissing True fullDirname
    let filesToWrite = ("payload.json", encodeUtf8 $ prettifyJson reportInfo) : files
    forM_ filesToWrite $ \(fname, content) ->
        BSL.writeFile (fullDirname </> fname) content
