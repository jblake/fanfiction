-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Applicative
import Control.Monad
import Data.List
import Database.HDBC
import Database.HDBC.PostgreSQL

import CommandLine
import qualified Fetch.FanfictionNet as FFNet
import qualified Fetch.HpfanficarchiveCom as HPFFACom
import qualified Fetch.YourfanfictionCom as YFFCom
import Import
import Update

data Tag
  = Set String
  | Clear String

instance Read Tag where
  readsPrec _ ('!':tag) = [(Clear tag, "")]
  readsPrec _ ('-':tag) = [(Clear tag, "")]
  readsPrec _ ('+':tag) = [(Set tag, "")]
  readsPrec _      tag  = [(Set tag, "")]

instance Show Tag where
  show (Set tag) = tag
  show (Clear tag) = '-' : tag

foldrTag :: Tag -> ([String], [String]) -> ([String], [String])
foldrTag (Set tag)   (set, clear) = (tag : set, clear)
foldrTag (Clear tag) (set, clear) = (set, tag : clear)

parseURL :: String -> Maybe (String, String)
parseURL url = tryFFNet <|> tryHPFFACom <|> tryYFFCom
  where
    tryFFNet = (\u -> ("fanfiction.net",u)) <$> FFNet.parse url
    tryHPFFACom = (\u -> ("hpfanficarchive.com",u)) <$> HPFFACom.parse url
    tryYFFCom = (\u -> ("yourfanfiction.com",u)) <$> YFFCom.parse url

main :: IO ()
main = withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

  addStoryStmt <- prepare db "select add_story_source( ?, ? )"
  addSourceStmt <- prepare db "select add_source( ?, ?, ? )"
  addTagStmt <- prepare db "select add_tag( ?, ? )"
  allTagsStmt <- prepare db "select tag, uses from all_tags"
  delTagStmt <- prepare db "select del_tag( ?, ? )"
  getTagsStmt <- prepare db "select tag from tags where story_id = ? order by tag asc"
  searchStmt <- prepare db "select story_id, filename from story_tags inner join stories using ( story_id ) where tags @> ? and not tags && ?"
  pruneStmt <- prepare db "select del_story( ? )"

  let

    addSources :: String -> [String] -> IO ()
    addSources storyID sources = do
      forM_ sources $ \source -> do
        case parseURL source of
          Just (site, ref) -> do
            execute addSourceStmt [toSql storyID, toSql site, toSql ref]
            fetchAllRows' addSourceStmt
            return ()
          Nothing -> putStrLn $ "Couldn't parse source URL: " ++ source
      commit db

    addStories :: [String] -> IO ()
    addStories sources = do
      forM_ sources $ \source -> do
        case parseURL source of
          Just (site, ref) -> do
            execute addStoryStmt [toSql site, toSql ref]
            [[idSql]] <- fetchAllRows' addStoryStmt
            putStrLn $ source ++ " -> " ++ fromSql idSql
          Nothing -> putStrLn $ "Couldn't parse source URL: " ++ source
      commit db

    lsTags :: IO ()
    lsTags = do
      execute allTagsStmt []
      rs <- fetchAllRows' allTagsStmt
      forM_ rs $ \[tagSql, usesSql] -> do
        putStrLn $ fromSql tagSql ++ "\t" ++ fromSql usesSql

    setTags :: String -> [Tag] -> IO ()
    setTags storyID tags = do
      forM_ tags $ \t -> case t of
        Set tag -> do
          execute addTagStmt [toSql storyID, toSql tag]
          fetchAllRows' addTagStmt
        Clear tag -> do
          execute delTagStmt [toSql storyID, toSql tag]
          fetchAllRows' delTagStmt
      commit db
      execute getTagsStmt [toSql storyID]
      rs <- fetchAllRows' getTagsStmt
      forM_ rs $ \[tag] -> putStrLn $ fromSql tag

    searchTags :: [Tag] -> IO ()
    searchTags tags = do
      let (setTags, clearTags) = foldr foldrTag ([],[]) tags
      execute searchStmt [toSql $ "{" ++ intercalate "," (map show setTags) ++ "}", toSql $ "{" ++ intercalate "," (map show clearTags) ++ "}"]
      rs <- fetchAllRows' searchStmt
      forM_ rs $ \[storyIDSql, maybeFileNameSql] -> do
        let
          storyID = fromSql storyIDSql
          maybeFileName = fromSql maybeFileNameSql
        putStrLn $ storyID ++ "\t" ++ maybe "(never downloaded)" id maybeFileName

    pruneStories :: [String] -> IO ()
    pruneStories storyIDs = do
      forM_ storyIDs $ \storyID -> do
        execute pruneStmt [toSql storyID]
        fetchAllRows' pruneStmt
      commit db

    commands :: [Command (IO ())]
    commands =
      [ Cmd "add-source" "Add sources as alternates for a story" addSources :@ "storyID" :@/ "URL"
      , Cmd "add" "Add a new story for each source provided" addStories :@/ "URL"
      , Cmd "import" "Import stories and tags from the legacy sqlite database" (importStories db)
      , Cmd "lstags" "List all tags and their usage counts" lsTags
      , Cmd "prune" "Prune stories" pruneStories :@/ "storyID"
      , Cmd "tag" "Tag or untag a story" setTags :@ "storyID" :@@/ "tag|+tag|-tag|!tag"
      , Cmd "search" "Search for stories that match a set of tags" searchTags :@@/ "tag|+tag|-tag|!tag"
      , Cmd "update" "Update stories" (update db) :@/ "storyID"
      ]

  command <- parseCommands commands
  maybe (printHelp commands) id command
