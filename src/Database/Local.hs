-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Database.Local
where

import Database.HDBC
import Database.HDBC.PostgreSQL

data LocalDB = LocalDB
  { db :: !Connection
  , allBooks :: !Statement
  , filename :: !Statement
  , newStory :: !Statement
  , prune :: !Statement
  , pruned :: !Statement
  , sources :: !Statement
  }

connectLocal :: IO LocalDB
connectLocal = do

  db <- connectPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp"

  allBooks <- prepare db "SELECT story_id FROM stories WHERE NOT pruned;"
  filename <- prepare db "SELECT get_filename( ?, ? );"
  newStory <- prepare db "SELECT add_story_source( ?, ? );"
  prune <- prepare db "SELECT del_story( ? );"
  pruned <- prepare db "SELECT filename FROM stories WHERE filename IS NOT NULL AND pruned;"
  sources <- prepare db "SELECT source, ref FROM sources WHERE story_id = ? ORDER BY source ASC;"

  return $ LocalDB
    { db = db
    , allBooks = allBooks
    , filename = filename
    , newStory = newStory
    , prune = prune
    , pruned = pruned
    , sources = sources
    }

iterateM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
iterateM gen iter = do
  x <- gen
  case x of
    Nothing -> return ()
    Just x' -> iter x' >> iterateM gen iter

foreachBook :: LocalDB -> (String -> [(String, String)] -> IO ()) -> IO ()
foreachBook pg act = withTransaction (db pg) $ const $ do

  execute (allBooks pg) []

  iterateM (fetchRow $ allBooks pg) $ \[storyID] -> do

    execute (sources pg) [storyID]
    info <- fetchAllRows' $ sources pg

    act (fromSql storyID) [ (fromSql source, fromSql ref) | [source, ref] <- info ]

pruneBooks :: LocalDB -> [String] -> IO ()
pruneBooks pg books = withTransaction (db pg) $ const $ executeMany (prune pg) [[toSql book] | book <- books]

deadBooks :: LocalDB -> IO [String]
deadBooks pg = withTransaction (db pg) $ const $ do

  execute (pruned pg) []
  dead <- fetchAllRows' $ pruned pg

  return [ fromSql book | [book] <- dead ]

makeFilename :: LocalDB -> String -> String -> IO String
makeFilename pg storyID defaultName = withTransaction (db pg) $ const $ do

  execute (filename pg) [toSql storyID, toSql defaultName]
  Just [file] <- fetchRow $ filename pg

  return $ fromSql file

addStory :: LocalDB -> String -> String -> IO String
addStory pg site ref = withTransaction (db pg) $ const $ do

  execute (newStory pg) [toSql site, toSql ref]
  Just [ref] <- fetchRow $ newStory pg

  return $ fromSql ref
