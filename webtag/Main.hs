-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
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

  db <- connectSqlite3 "/srv/tags/tags.db"

  allTagsStmt  <- prepare db "SELECT tag FROM tags GROUP BY tag ORDER BY COUNT(*) DESC, tag ASC"
  itemTagsStmt <- prepare db "SELECT all_tags.tag, tags.item IS NOT NULL FROM all_tags LEFT OUTER JOIN tags ON ( tags.tag = all_tags.tag AND tags.item = ? ) ORDER BY (SELECT COUNT(*) FROM tags x WHERE x.tag = all_tags.tag) DESC, all_tags.tag ASC"
  setTagStmt   <- prepare db "INSERT INTO tags ( item, tag ) VALUES ( ?, ? )"
  clearTagStmt <- prepare db "DELETE FROM tags WHERE item = ? AND tag = ?"

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

        mksql (Left t)  = (" EXCEPT    SELECT item FROM tags WHERE tag = ?", t)
        mksql (Right t) = (" INTERSECT SELECT item FROM tags WHERE tag = ?", t)

        (sql, tags) = unzip $ map mksql ts

      stmt <- prepare db $ "SELECT item FROM all_items" ++ concat sql
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

        dir <- liftIO $ getDirectoryContents "/home/jblake/src/fanfiction/import"

        let suffix = "_" ++ item ++ ".epub"

        case filter (isSuffixOf suffix) dir of
          [filename] -> do
            stat <- liftIO $ getFileStatus $ "/home/jblake/src/fanfiction/import" </> filename
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
