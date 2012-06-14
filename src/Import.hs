-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Monad
import Database.HDBC
import Database.HDBC.PostgreSQL
import Database.HDBC.Sqlite3

main :: IO ()
main = withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \pg -> do

  sq <- connectSqlite3 "/srv/tags/tags.db"

  addStory <- prepare pg "select add_story( )"
  addSource <- prepare pg "select add_source( ?, 'fanfiction.net', ? )"
  addTag <- prepare pg "select add_tag( ?, ? )"

  allItems <- prepare sq "select item from all_items order by cast (item as integer) asc"
  itemTags <- prepare sq "select tag from tags where item = ?"

  execute allItems []
  items <- fetchAllRows allItems

  forM_ items $ \[item] -> do

    putStrLn $ "Importing " ++ fromSql item

    execute addStory []
    [[story]] <- fetchAllRows' addStory

    execute addSource [story, item]

    execute itemTags [item]
    tags <- fetchAllRows itemTags

    forM_ tags $ \[tag] -> do
      execute addTag [story, tag]

  commit pg
