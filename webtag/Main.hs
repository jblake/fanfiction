-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Monad
import Control.Monad.Error.Class
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import Network.CGI
import Network.FastCGI
import Text.JSON

data Tag = Tag
  { name    :: String
  , enabled :: Bool
  }
  deriving (Read, Show)

instance JSON Tag where

  readJSON o = do
    om <- readJSON o >>= return . fromJSObject
    case (lookup "name" om) of
      Just n -> do
        nm <- readJSON n
        case (lookup "checked" om) of
          Just c -> do
            cm <- readJSON c
            return $ Tag { name = nm, enabled = cm }
          _ -> throwError "JSON tag must have 'checked' property!"
      _ -> throwError "JSON tag must have 'name' property!"

  showJSON (Tag {name, enabled}) = showJSON $ toJSObject [ ("name", showJSON name), ("checked", showJSON enabled) ]

data TagDB = TagDB
  { allTags  :: IO [String]
  , itemTags :: String -> IO [Tag]
  , setTag   :: String -> String -> IO ()
  , clearTag :: String -> String -> IO ()
  , done     :: IO ()
  }

getTagDB :: IO TagDB
getTagDB = do

  db <- connectSqlite3 "tags.db"

  allTagsStmt <- prepare db "SELECT tag FROM all_tags ORDER BY tag ASC;"
  itemTagsStmt <- prepare db "SELECT all_tags.tag, tags.item IS NOT NULL FROM all_tags LEFT OUTER JOIN tags ON ( tags.tag = all_tags.tag AND tags.item = ? ) ORDER BY all_tags.tag ASC;"
  setTagStmt <- prepare db "INSERT INTO tags ( item, tag ) VALUES ( ?, ? );"
  clearTagStmt <- prepare db "DELETE FROM tags WHERE item = ? AND tag = ?;"

  let

    allTags = do
      execute allTagsStmt []
      rs <- fetchAllRows' allTagsStmt
      commit db
      return $ map (fromSql . head) rs

    itemTags i = do
      execute itemTagsStmt [toSql i]
      rs <- fetchAllRows' itemTagsStmt
      commit db
      return $ map (\[ts, is] -> Tag { name = fromSql ts, enabled = fromSql is }) rs

    setTag i t = handleSql (const $ return ()) $ do
      execute setTagStmt [toSql i, toSql t]
      commit db

    clearTag i t = do
      execute clearTagStmt [toSql i, toSql t]
      commit db

    done = do
      disconnect db

  return $ TagDB {..}

main :: IO ()
main = do

  db <- getTagDB

  runFastCGIorCGI $ handleErrors $ do

    Just path <- getVar "PATH_INFO"
    let item = reverse $ takeWhile (/= '/') $ reverse path

    inputs <- getInputs
    forM_ inputs $ \(command, arg) -> do

      case command of
        "clear" -> liftIO $ clearTag db item arg
        "set"   -> liftIO $ setTag db item arg
        _       -> logCGI $ "Unknown command " ++ show command ++ " (arg=" ++ show arg ++ ") ignored."

    setStatus 200 "OK"
    setHeader "Cache-control" "no-cache"
    setHeader "Content-type" "application/json"

    tags <- liftIO $ itemTags db item

    output $ encode $ showJSON tags

  done db
