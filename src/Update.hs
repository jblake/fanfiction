-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.PostgreSQL
import Foreign.C.Types
import Network.Browser
import System.Posix.Files

import Concurrent
import EPub
import Fetch
import Fetch.FanfictionNet as FFNet

data DB = DB
  { db :: Connection
  , fileNameStmt :: Statement
  , sourcesStmt :: Statement
  , unprunedStoriesStmt :: Statement
  }

type DBM a b = Work (ReaderT DB IO) a b

withDB :: ReaderT DB IO a -> IO a
withDB m = withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

  fileNameStmt <- prepare db "select get_filename( ?, ? )"
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

main :: IO ()
main = do

  dbWorker <- newWorker withDB

  epubWorker <- newWorker id

  ffnetWorker <- newWorker $ \m -> browse $ do
    setAllowRedirects True
    setOutHandler $ const $ return ()
    m

  let

    writeEPub info epub path = lift $ do
      putStrLn $ "    " ++ infoUnique info ++ ": Writing " ++ path
      BS.writeFile path $ compileEPub epub

    checkUpdated info fetchWorker fetchAct = do

      fileName <- getFileName (infoUnique info) $ (map (\c -> if not (isAlphaNum c) then '_' else c) $ infoTitle info ++ "_by_" ++ infoAuthor info ++ "_" ++ infoUnique info) ++ ".epub"
      let path = "import/" ++ fileName

      exists <- liftIO $ fileExist path

      if not exists

        then pass fetchWorker $ do
          liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": New story"
          epub <- fetchAct
          pass epubWorker $ writeEPub info epub path

        else do

          stat <- liftIO $ getFileStatus path

          if modificationTime stat < CTime (round $ utcTimeToPOSIXSeconds $ infoUpdated info)

            then pass fetchWorker $ do
              liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": Updated"
              epub <- fetchAct
              pass epubWorker $ writeEPub info epub path

            else liftIO $ putStrLn $ "    " ++ infoUnique info ++ ": No change"

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

  putStrLn "    Getting story list"

  uniques <- eval dbWorker $ getUnprunedStories

  putStrLn "    Queueing story runs"

  signals <- forM uniques $ \unique -> defer dbWorker $ do
    sources <- getSources unique
    doFetch unique sources

  putStrLn "    Waiting for all pipelines to complete"

  forM_ signals force

  putStrLn "    Done"
