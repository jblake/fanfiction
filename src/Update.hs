-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Update
where

import Control.Concurrent.QSem
import qualified Control.DeepSeq as DS
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Time.Clock.POSIX
import Data.Time.Format
import Database.HDBC
import Database.HDBC.PostgreSQL
import Foreign.C.Types
import Network.Browser
import System.Directory
import System.Environment
import System.Locale
import System.Posix.Files

import Concurrent
import EPub
import Fetch
import Fetch.FanfictionNet as FFNet
import Fetch.HpfanficarchiveCom as HPFFACom
import Fetch.YourfanfictionCom as YFFCom

data DB = DB
  { db :: Connection
  , fileNameStmt :: Statement
  , prunedFileNamesStmt :: Statement
  , sourcesStmt :: Statement
  , unprunedStoriesStmt :: Statement
  , logSuccessStmt :: Statement
  , logFailureStmt :: Statement
  }

type DBM a b = Work (ReaderT DB IO) a b

withDB :: Connection -> ReaderT DB IO a -> IO a
withDB db m = do

  fileNameStmt <- prepare db "select get_filename( ?, ? )"
  prunedFileNamesStmt <- prepare db "select filename from stories where pruned and filename is not null"
  sourcesStmt <- prepare db "select source, ref from sources where story_id = ? order by source asc"
  unprunedStoriesStmt <- prepare db "select story_id from stories where not pruned"
  logSuccessStmt <- prepare db "select log_success( ?, ? )"
  logFailureStmt <- prepare db "select log_failure( ?, ? )"

  runReaderT m $ DB {..}

getFileName :: String -> String -> DBM a String
getFileName unique candidate = do
  DB {..} <- lift ask
  liftIO $ do
    execute fileNameStmt [toSql unique, toSql candidate]
    [[name]] <- fetchAllRows' fileNameStmt
    return $ fromSql name

getPrunedFileNames :: DBM a [String]
getPrunedFileNames = do
  DB {..} <- lift ask
  liftIO $ do
    execute prunedFileNamesStmt []
    rs <- fetchAllRows' prunedFileNamesStmt
    return [ fromSql name | [name] <- rs ]

getSources :: String -> DBM a [(String, String)]
getSources unique = do
  DB {..} <- lift ask
  liftIO $ do
    execute sourcesStmt [toSql unique]
    rs <- fetchAllRows' sourcesStmt
    return [ (fromSql source, fromSql ref) | [source, ref] <- rs ]

getUnprunedStories :: DBM a [String]
getUnprunedStories = do
  DB {..} <- lift ask
  liftIO $ do
    execute unprunedStoriesStmt []
    rs <- fetchAllRows' unprunedStoriesStmt
    return [ fromSql unique | [unique] <- rs ]

logSuccess :: String -> Maybe String -> DBM a ()
logSuccess unique annotation = do
  DB {..} <- lift ask
  liftIO $ do
    execute logSuccessStmt [toSql unique, toSql annotation]
    fetchAllRows' logSuccessStmt
    return ()

logFailure :: String -> Maybe String -> DBM a ()
logFailure unique annotation = do
  DB {..} <- lift ask
  liftIO $ do
    execute logFailureStmt [toSql unique, toSql annotation]
    fetchAllRows' logFailureStmt
    return ()

commitChanges :: DBM a ()
commitChanges = do
  DB {..} <- lift ask
  liftIO $ commit db

update :: Connection -> [String] -> IO ()
update db args = do

  dbWorker <- newWorker 1 $ withDB db

  epubWorker <- newWorker 1 id

  webWorker <- newWorker 4 $ \m -> browse $ do
    setAllowRedirects True
    setOutHandler $ const $ return ()
    m

  let

    writeEPub :: Info -> EPub -> String -> Work IO () ()
    writeEPub info epub path = do
      lift $ do
        putStrLn $ "    " ++ infoUnique info ++ ": Writing " ++ path
        createDirectoryIfMissing False "/srv/epubs"
        epubData <- compileEPub epub
        BS.writeFile path epubData
        let epochTime = CTime $ round $ utcTimeToPOSIXSeconds $ infoUpdated info
        setFileTimes path epochTime epochTime
      pass dbWorker $ logSuccess (infoUnique info) $ Just "Downloaded new update."

    checkUpdated :: (MonadIO m) => Info -> Worker m -> Work m () EPub -> DBM () ()
    checkUpdated info fetchWorker fetchAct = do

      let
        compress :: String -> String
        compress "" = ""
        compress "_" = "_"
        compress ('_':'_':l) = compress ('_':l)
        compress ('_':x:l) = '_' : x : compress l
        compress (x:l) = x : compress l

      fileName <- getFileName (infoUnique info) $ compress $ (map (\c -> if not (isAlphaNum c) then '_' else c) $ infoTitle info ++ "_by_" ++ infoAuthor info ++ "_" ++ infoUnique info) ++ ".epub"
      let path = "/srv/epubs/" ++ fileName

      exists <- liftIO $ fileExist path

      if not exists

        then pass fetchWorker $ do
          liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": New story (as of " ++ formatTime defaultTimeLocale "%F" (infoUpdated info) ++ ")"
          epub <- fetchAct
          DS.deepseq epub $ pass epubWorker $ writeEPub info epub path

        else do

          stat <- liftIO $ getFileStatus path

          if modificationTime stat < CTime (round $ utcTimeToPOSIXSeconds $ infoUpdated info)

            then pass fetchWorker $ do
              liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": Updated (on " ++ formatTime defaultTimeLocale "%F" (infoUpdated info) ++ ")"
              epub <- fetchAct
              DS.deepseq epub $ pass epubWorker $ writeEPub info epub path

--            else liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": No change (last updated " ++ formatTime defaultTimeLocale "%F" (infoUpdated info) ++ ")"
            else return ()

    doFetch :: (MonadIO m) => String -> [(String, String)] -> Work m () ()

    doFetch unique [] = pass dbWorker $ do
      logFailure unique $ Just "No sources."
      liftIO $ putStrLn $ "!!! " ++ unique ++ ": No sources"

    doFetch unique (("fanfiction.net", ref):sources) = pass webWorker $ do
--      liftIO $ putStrLn $ "    " ++ unique ++ ": Examining ffnet/" ++ ref

      maybeInfo <- lift $ FFNet.peek unique ref
      case maybeInfo of

        Just info -> DS.deepseq info $ pass dbWorker $ checkUpdated info webWorker $ do
          liftIO $ putStrLn $ "    " ++ unique ++ ": Downloading ffnet/" ++ ref
          lift $ FFNet.fetch info

        Nothing -> do
          liftIO $ putStrLn $ "!   " ++ unique ++ ": Invalid source ffnet/" ++ ref
          doFetch unique sources

    doFetch unique (("hpfanficarchive.com", ref):sources) = pass webWorker $ do
--      liftIO $ putStrLn $ "    " ++ unique ++ ": Examining hpffacom/" ++ ref

      maybeInfo <- lift $ HPFFACom.peek unique ref
      case maybeInfo of

        Just info -> DS.deepseq info $ pass dbWorker $ checkUpdated info webWorker $ do
          liftIO $ putStrLn $ "    " ++ unique ++ ": Downloading hpffacom/" ++ ref
          lift $ HPFFACom.fetch info

        Nothing -> do
          liftIO $ putStrLn $ "!   " ++ unique ++ ": Invalid source hpffacom/" ++ ref
          doFetch unique sources

    doFetch unique (("yourfanfiction.com", ref):sources) = pass webWorker $ do
--      liftIO $ putStrLn $ "    " ++ unique ++ ": Examining yffcom/" ++ ref

      maybeInfo <- lift $ YFFCom.peek unique ref
      case maybeInfo of

        Just info -> DS.deepseq info $ pass dbWorker $ checkUpdated info webWorker $ do
          liftIO $ putStrLn $ "    " ++ unique ++ ": Downloading yffcom/" ++ ref
          lift $ YFFCom.fetch info

        Nothing -> do
          liftIO $ putStrLn $ "!   " ++ unique ++ ": Invalid source yffcom/" ++ ref
          doFetch unique sources

    doFetch unique ((source, ref):sources) = do
      liftIO $ putStrLn $ "!    " ++ unique ++ ": Unsupported source " ++ source ++ "/" ++ ref
      doFetch unique sources

  putStrLn "    Getting pruned story list"

  pruned <- eval dbWorker Nothing getPrunedFileNames

  putStrLn "    Pruning dead stories"

  forM_ pruned $ \fileName -> do
    let path = "/srv/epubs/" ++ fileName
    exists <- doesFileExist path
    when exists $ removeFile path

  uniques <- if null args
    then do
      putStrLn "    Getting story list"
      eval dbWorker Nothing getUnprunedStories
    else return args

  putStrLn "    Starting story runs"

  signals <- forM uniques $ \unique -> defer dbWorker Nothing $ do
    sources <- getSources unique
    doFetch unique sources

  putStrLn "    Waiting for everything to finish"

  forM_ signals force

  putStrLn "    Committing changes to database"

  eval dbWorker Nothing commitChanges

  putStrLn "    Done"
