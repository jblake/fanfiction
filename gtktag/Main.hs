-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Main
where

import Database.HDBC
import Database.HDBC.Sqlite3
import Graphics.UI.Gtk

import GtkUI

main :: IO ()
main = do

  initGUI

  db <- connectSqlite3 "/srv/tags/tags.db"

  getTagsStmt <- prepare db "SELECT all_tags.tag, tags.item IS NOT NULL FROM all_tags LEFT OUTER JOIN tags ON ( tags.tag = all_tags.tag AND tags.item = ? ) ORDER BY (SELECT COUNT(*) FROM tags x WHERE x.tag = all_tags.tag) DESC, all_tags.tag ASC"
  setTagStmt <- prepare db "INSERT INTO tags ( item, tag ) VALUES ( ?, ? )"
  clearTagStmt <- prepare db "DELETE FROM tags WHERE item = ? AND tag = ?"

  let

    getTags = filterTags getAllTags

    getAllTags item = do
      _ <- execute getTagsStmt [toSql item]
      rs <- fetchAllRows' getTagsStmt
      commit db
      return $ [ (fromSql tag, fromSql set) | [tag, set] <- rs ]

    setTag item tag = do
      _ <- execute setTagStmt [toSql item, toSql tag]
      commit db

    clearTag item tag = do
      _ <- execute clearTagStmt [toSql item, toSql tag]
      commit db

    endUI = mainQuit

  initWindow $ WindowBackend {..}

  mainGUI
