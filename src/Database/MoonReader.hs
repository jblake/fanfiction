-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Database.MoonReader
where

import Data.DateTime
import Data.Char
import Database.HDBC
import Database.HDBC.Sqlite3

data MoonReader = MoonReader
  { db :: !Connection
  , search :: !Statement
  , update :: !Statement
  , insert :: !Statement
  , delete :: !Statement
  }

connectMoonReader :: IO MoonReader
connectMoonReader = do

  db <- connectSqlite3 "moonreader/mrbooks.db"

  search <- prepare db "SELECT _id FROM books WHERE filename = ?;"
  update <- prepare db "UPDATE books SET favorite = 'default_fav', addTime = ? WHERE _id = ?;"
  insert <- prepare db "INSERT INTO books ( book, filename, lowerFilename, author, addTime, favorite, description, category, thumbFile, coverFile, downloadUrl, rate, bak1, bak2 ) VALUES ( ?, ?, ?, ?, ?, 'default_fav', '', '', '', '', '', '', '', '' );"
  delete <- prepare db "DELETE FROM books WHERE filename = ?;"

  return $ MoonReader
    { db = db
    , search = search
    , update = update
    , insert = insert
    , delete = delete
    }

touchBook :: MoonReader -> String -> (String, String, DateTime) -> IO ()
touchBook mr filename (title, author, date) = withTransaction (db mr) $ const $ do

  let mrfilename = "/sdcard/Books/" ++ filename

  execute (search mr) [toSql mrfilename]
  r <- fetchRow (search mr)

  case r of

    Nothing -> do
      execute (insert mr) [toSql title, toSql mrfilename, toSql (map toLower mrfilename), toSql author, toSql (1000 * toSeconds date)]

    Just [_id] -> do
      execute (update mr) [toSql (1000 * toSeconds date), _id]

  return ()

rmBooks :: MoonReader -> [String] -> IO ()
rmBooks mr filenames = withTransaction (db mr) $ const $ do
  executeMany (delete mr) [ [toSql $ "/sdcard/Books/" ++ filename] | filename <- filenames ]
