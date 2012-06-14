-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
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

main :: IO ()
main = do

  ffnetWorker <- newWorker $ \m -> browse $ do
    setAllowRedirects True
    setOutHandler $ const $ return ()
    m

  filepathWorker <- newWorker id
  epubWorker <- newWorker id

  signals <- withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

    filepathStmt <- prepare db "select get_filepath( ?, ? )"
    sourcesStmt <- prepare db "select source, ref from sources where story = ? order by source asc"
    unprunedStoriesStmt <- prepare db "select id from stories where not pruned"

    let
      writeEPub storyDone info epub path = do

        BS.writeFile path $ compileEPub epub

        putStrLn $ infoUnique info ++ ": Wrote " ++ path
        putMVar storyDone ()

      considerEPub storyDone info worker act = do

        let
          candidatePath = (map (\c -> if not (isAlphaNum c) then '_' else c) $ infoTitle info ++ "_by_" ++ infoAuthor info ++ "_" ++ infoUnique info) ++ ".epub"

        execute filepathStmt [toSql $ infoUnique info, toSql candidatePath]
        [[pathSql]] <- fetchAllRows' filepathStmt

        let
          path = "import/" ++ fromSql pathSql

        exists <- fileExist path

        if not exists
          then bg $ first worker $ do
            epub <- act
            liftIO $ putStrLn $ infoUnique info ++ ": Downloaded (for the first time)"
            liftIO $ bg $ first epubWorker $ writeEPub storyDone info epub path

          else do

            stat <- getFileStatus path

            if modificationTime stat < CTime (round $ utcTimeToPOSIXSeconds $ infoUpdated info)
              then bg $ first worker $ do
                epub <- act
                liftIO $ putStrLn $ infoUnique info ++ ": Downloaded (updated)"
                liftIO $ bg $ first epubWorker $ writeEPub storyDone info epub path

              else do
                putStrLn $ infoUnique info ++ ": No change"
                putMVar storyDone ()

      tryFetch storyDone storyID [] = do
        putStrLn $ storyID ++ ": No sources left!"
        putMVar storyDone ()

      tryFetch storyDone storyID (("fanfiction.net", ref):fallbacks) = bg $ first ffnetWorker $ do

        liftIO $ putStrLn $ storyID ++ ": Peek fanfiction.net:" ++ ref

        maybeInfo <- FFNet.peek storyID ref
        case maybeInfo of

          Just info -> liftIO $ bg $ first filepathWorker $ considerEPub storyDone info ffnetWorker (FFNet.fetch info)

          Nothing -> liftIO $ do
            putStrLn $ storyID ++ ": Invalid fanfiction.net:" ++ ref ++ "!"
            tryFetch storyDone storyID fallbacks

      tryFetch storyDone storyID ((s, ref):fallbacks) = do
        putStrLn $ storyID ++ ": Unsupported " ++ s ++ ":" ++ ref ++ "!"
        tryFetch storyDone storyID fallbacks

    execute unprunedStoriesStmt []
    rs <- fetchAllRows' unprunedStoriesStmt

    forM [ fromSql idSql | [idSql] <- rs ] $ \storyID -> do

      execute sourcesStmt [toSql storyID]
      rs <- fetchAllRows' sourcesStmt

      storyDone <- newEmptyMVar

      tryFetch storyDone storyID [ (fromSql sourceSql, fromSql refSql) | [sourceSql, refSql] <- rs ]

      return storyDone

  forM_ signals $ takeMVar
