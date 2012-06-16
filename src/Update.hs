-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Concurrent.QSem
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

data DB = DB
  { db :: Connection
  , fileNameStmt :: Statement
  , prunedFileNamesStmt :: Statement
  , sourcesStmt :: Statement
  , unprunedStoriesStmt :: Statement
  }

type DBM a b = Work (ReaderT DB IO) a b

withDB :: ReaderT DB IO a -> IO a
withDB m = withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

  fileNameStmt <- prepare db "select get_filename( ?, ? )"
  prunedFileNamesStmt <- prepare db "select filename from stories where pruned and filename is not null"
  sourcesStmt <- prepare db "select source, ref from sources where story = ? order by source asc"
  unprunedStoriesStmt <- prepare db "select id from stories where not pruned"

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

commitChanges :: DBM a ()
commitChanges = do
  DB {..} <- lift ask
  liftIO $ commit db

main :: IO ()
main = do

  dbWorker <- newWorker 1 withDB

  epubWorker <- newWorker 1 id

  ffnetWorker <- newWorker 2 $ \m -> browse $ do
    setAllowRedirects True
    setOutHandler $ const $ return ()
    m

  let

    writeEPub :: Info -> EPub -> String -> Work IO () ()
    writeEPub info epub path = lift $ do
      putStrLn $ "    " ++ infoUnique info ++ ": Writing " ++ path
      createDirectoryIfMissing False "/srv/epubs"
      BS.writeFile path $ compileEPub epub
      let epochTime = CTime $ round $ utcTimeToPOSIXSeconds $ infoUpdated info
      setFileTimes path epochTime epochTime

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
          pass epubWorker $ writeEPub info epub path

        else do

          stat <- liftIO $ getFileStatus path

          if modificationTime stat < CTime (round $ utcTimeToPOSIXSeconds $ infoUpdated info)

            then pass fetchWorker $ do
              liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": Updated (on " ++ formatTime defaultTimeLocale "%F" (infoUpdated info) ++ ")"
              epub <- fetchAct
              pass epubWorker $ writeEPub info epub path

            else liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": No change (last updated " ++ formatTime defaultTimeLocale "%F" (infoUpdated info) ++ ")"

    doFetch :: (MonadIO m) => String -> [(String, String)] -> Work m () ()

    doFetch unique [] = liftIO $ putStrLn $ "!!! " ++ unique ++ ": No sources"

    doFetch unique (("fanfiction.net", ref):sources) = pass ffnetWorker $ do
      liftIO $ putStrLn $ "    " ++ unique ++ ": Examining ffnet/" ++ ref

      maybeInfo <- lift $ FFNet.peek unique ref
      case maybeInfo of

        Just info -> pass dbWorker $ checkUpdated info ffnetWorker $ do
          liftIO $ putStrLn $ "    " ++ unique ++ ": Downloading ffnet/" ++ ref
          lift $ FFNet.fetch info

        Nothing -> do
          liftIO $ putStrLn $ "!   " ++ unique ++ ": Invalid source ffnet/" ++ ref
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

  args <- getArgs

  uniques <- if null args
    then do
      putStrLn "    Getting story list"
      eval dbWorker Nothing getUnprunedStories
    else return args

  sem <- newQSem 25

  putStrLn "    Starting story runs"

  signals <- forM uniques $ \unique -> defer dbWorker (Just sem) $ do
    sources <- getSources unique
    doFetch unique sources

  putStrLn "    Waiting for everything to finish"

  forM_ signals force

  putStrLn "    Committing changes to database"

  eval dbWorker Nothing commitChanges

  putStrLn "    Done"
