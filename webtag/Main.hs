-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Monad
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.List as L
import Data.List.Split
import Data.Maybe
import Network.CGI
import Network.FastCGI
import Text.JSON.Generic

data Tag = Tag
  { name    :: String
  , checked :: Bool
  }
  deriving (Data, Read, Show, Typeable)

data TagDB = TagDB
  { allTags  :: IO [String]
  , itemTags :: String -> IO [Tag]
  , setTag   :: String -> String -> IO ()
  , clearTag :: String -> String -> IO ()
  , find     :: [String] -> [String] -> IO [String]
  , done     :: IO ()
  }

getTagDB :: IO TagDB
getTagDB = do

  db <- connectSqlite3 "/srv/tags/tags.db"

  allTagsStmt <- prepare db "SELECT tag FROM all_tags ORDER BY tag ASC;"
  itemTagsStmt <- prepare db "SELECT all_tags.tag, tags.item IS NOT NULL FROM all_tags LEFT OUTER JOIN tags ON ( tags.tag = all_tags.tag AND tags.item = ? ) ORDER BY all_tags.tag ASC;"
  setTagStmt <- prepare db "INSERT INTO tags ( item, tag ) VALUES ( ?, ? );"
  clearTagStmt <- prepare db "DELETE FROM tags WHERE item = ? AND tag = ?;"

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

    setTag i t = handleSql (const $ return ()) $ do
      _ <- execute setTagStmt [toSql i, toSql t]
      commit db

    clearTag i t = do
      _ <- execute clearTagStmt [toSql i, toSql t]
      commit db

    find has hasnt = do
      stmt <- prepare db $ L.intercalate " AND " $ "SELECT DISTINCT item FROM tags t1 WHERE 1 " : ["EXISTS (SELECT * FROM tags t2 WHERE t2.item = t1.item AND t2.tag = ?)" | _ <- has] ++ ["NOT EXISTS (SELECT * FROM tags t2 WHERE t2.item = t1.item AND t2.tag = ?)" | _ <- hasnt]
      _ <- execute stmt $ map toSql $ has ++ hasnt
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
          tags = filter (not . null) $ splitOn "/" path

          tagNot ('!':t) = (Nothing, Just t)
          tagNot t       = (Just t, Nothing)

          (hasL, hasntL) = unzip $ map tagNot tags

        items <- liftIO $ find db (catMaybes hasL) (catMaybes hasntL)

        setStatus 200 "OK"
        setHeader "Cache-control" "no-cache"
        setHeader "Content-type" "application/json"

        output $ encodeJSON items

      _ -> error $ "Unknown WEBTAG_MODE " ++ show mode ++ "!"

  done db
