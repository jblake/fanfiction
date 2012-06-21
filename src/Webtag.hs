-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Monad
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.List
import Data.List.Split
import Network.CGI
import Network.FastCGI
import System.Directory
import System.FilePath
import System.Posix.Files
import Text.JSON.Generic

data Info = Info
  { filename :: String
  , size     :: Integer
  }
  deriving (Data, Read, Show, Typeable)

data Tag = Tag
  { name    :: String
  , checked :: Bool
  }
  deriving (Data, Read, Show, Typeable)

data TagDB = TagDB
  { allTags   :: IO [String]
  , itemTags  :: String -> IO [Tag]
  , setTag    :: String -> String -> IO ()
  , clearTag  :: String -> String -> IO ()
  , findItems :: [Either String String] -> IO [String]
  , done      :: IO ()
  }

getTagDB :: IO TagDB
getTagDB = do

  db <- connectPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp"

  allTagsStmt  <- prepare db "SELECT tag FROM all_tags ORDER BY uses DESC, tag ASC"
  itemTagsStmt <- prepare db "SELECT all_tags.tag, tags.tag IS NOT NULL FROM all_tags LEFT OUTER JOIN tags ON ( all_tags.tag = tags.tag AND story_id = ? ) ORDER BY uses DESC, tag ASC"
  setTagStmt   <- prepare db "SELECT add_tag( ?, ? )"
  clearTagStmt <- prepare db "SELECT del_tag( ?, ? )"

  let

    allTags = do
      _ <- execute allTagsStmt []
      rs <- fetchAllRows' allTagsStmt
      commit db
      return $ map (fromSql . head) rs

    itemTags i = do
      _ <- execute itemTagsStmt [toSql i]
      rs <- fetchAllRows' itemTagsStmt
      commit db
      return $ map (\[ts, is] -> Tag { name = fromSql ts, checked = fromSql is }) rs

    setTag "" _ = return ()
    setTag _ "" = return ()
    setTag i t = handleSql (const $ return ()) $ do
      _ <- execute setTagStmt [toSql i, toSql t]
      commit db

    clearTag "" _ = return ()
    clearTag _ "" = return ()
    clearTag i t = do
      _ <- execute clearTagStmt [toSql i, toSql t]
      commit db

    findItems ts = do

      let

        mksql (Left t)  = (" EXCEPT    SELECT story_id FROM tags WHERE tag = ?", t)
        mksql (Right t) = (" INTERSECT SELECT story_id FROM tags WHERE tag = ?", t)

        (sql, tags) = unzip $ map mksql ts

      stmt <- prepare db $ "SELECT story_id FROM stories" ++ concat sql
      _ <- execute stmt $ map toSql tags
      rs <- fetchAllRows' stmt
      commit db
      return $ map (fromSql . head) rs

    done = do
      disconnect db

  return $ TagDB {..}

main :: IO ()
main = do

  db <- getTagDB

  runFastCGIorCGI $ handleErrors $ do

    Just mode <- getVar "WEBTAG_MODE"

    case mode of

      "info" -> do

        Just path <- getVar "PATH_INFO"
        let item = reverse $ takeWhile (/= '/') $ reverse path

        dir <- liftIO $ getDirectoryContents "/srv/epubs"

        let suffix = "_" ++ item ++ ".epub"

        case filter (isSuffixOf suffix) dir of
          [filename] -> do
            stat <- liftIO $ getFileStatus $ "/srv/epubs" </> filename
            let size = fromIntegral $ fileSize stat

            setStatus 200 "OK"
            setHeader "Cache-control" "no-cache"
            setHeader "Content-type" "application/json"
            output $ encodeJSON $ Info {..}

          [] -> do
            setStatus 404 "Not found"
            setHeader "Cache-control" "no-cache"
            setHeader "Content-type" "text/plain"
            output "Couldn't get info for such an item."

          _ -> do
            setStatus 500 "Internal server error"
            setHeader "Cache-control" "no-cache"
            setHeader "Content-type" "text/plain"
            output "Multiple results for such an item."

      "item" -> do

        Just path <- getVar "PATH_INFO"
        let item = reverse $ takeWhile (/= '/') $ reverse path

        inputs <- getInputs
        forM_ inputs $ \(command, arg) -> do

          case command of
            "clear" -> liftIO $ clearTag db item arg
            "set"   -> liftIO $ setTag db item arg
            _       -> logCGI $ "Unknown command " ++ show command ++ " (arg=" ++ show arg ++ ") ignored."

        tags <- liftIO $ itemTags db item

        setStatus 200 "OK"
        setHeader "Cache-control" "no-cache"
        setHeader "Content-type" "application/json"

        output $ encodeJSON tags

      "find" -> do

        Just path <- getVar "PATH_INFO"

        let
          tags = map tagNot $ filter (not . null) $ splitOn "/" path

          tagNot ('!':t) = Left t
          tagNot t       = Right t

        items <- liftIO $ findItems db tags

        setStatus 200 "OK"
        setHeader "Cache-control" "no-cache"
        setHeader "Content-type" "application/json"

        output $ encodeJSON items

      _ -> error $ "Unknown WEBTAG_MODE " ++ show mode ++ "!"

  done db
