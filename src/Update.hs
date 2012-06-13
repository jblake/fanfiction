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
import Database.HDBC
import Database.HDBC.PostgreSQL
import Network.Browser

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

  epubWorker <- newWorker id

  let
    tryFetch storyDone storyID [] = do
      putStrLn $ "No remaining sources for story " ++ storyID ++ "!"
      putMVar storyDone ()

    tryFetch storyDone storyID (("fanfiction.net", ref):fallbacks) = bg $ first ffnetWorker $ do

        maybeInfo <- FFNet.peek storyID ref
        case maybeInfo of

          Just info -> liftIO $ bg $ first ffnetWorker $ do
            epub <- FFNet.fetch info
            liftIO $ putStrLn $ "Got story " ++ storyID ++ " using fanfiction.net:" ++ ref ++ "."
            liftIO $ bg $ first epubWorker $ do
              let path = "import/" ++ (map (\c -> if not (isAlphaNum c) then '_' else c) $ infoTitle info ++ "_by_" ++ infoAuthor info ++ "_" ++ storyID) ++ ".epub"
              BS.writeFile path $ compileEPub epub
              putStrLn $ "Finished saving epub for story " ++ storyID ++ "."
              putMVar storyDone ()

          Nothing -> liftIO $ do
            putStrLn $ "Can't use fanfiction.net:" ++ ref ++ " for story " ++ storyID ++ ": not found!"
            tryFetch storyDone storyID fallbacks

    tryFetch storyDone storyID ((s, ref):fallbacks) = do
      putStrLn $ "Can't use source " ++ s ++ ":" ++ ref ++ " for story " ++ storyID ++ ": site not supported!"
      tryFetch storyDone storyID fallbacks

  signals <- withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

    unprunedStoriesStmt <- prepare db "select id from stories where not pruned"
    sourcesStmt <- prepare db "select source, ref from sources where story = ? order by source asc"

    execute unprunedStoriesStmt []
    rs <- fetchAllRows' unprunedStoriesStmt

    forM [ fromSql idSql | [idSql] <- rs ] $ \storyID -> do

      execute sourcesStmt [toSql storyID]
      rs <- fetchAllRows' sourcesStmt

      storyDone <- newEmptyMVar

      tryFetch storyDone storyID [ (fromSql sourceSql, fromSql refSql) | [sourceSql, refSql] <- rs ]

      return storyDone

  forM_ signals $ takeMVar
