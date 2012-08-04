-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}

module Main
where

import Data.List
import Data.List.Split
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.Console.Editline
import System.Console.Editline.Readline

type DB a =
  ( ?db :: Connection

  , ?addTag :: Statement
  , ?delTag :: Statement
  , ?getNoTags :: Statement
  , ?getTags :: Statement
  , ?prune :: Statement
  , ?storyLookup :: Statement
  , ?tagLookupExists :: Statement
  , ?tagLookupNoExists :: Statement

  ) => a

withDB :: DB (IO a) -> IO a
withDB act = withPostgreSQL "dbname=fanfiction user=fanfiction host=/tmp" $ \db -> do

  addTag <- prepare db "select add_tag( ?, ? )"
  delTag <- prepare db "select del_tag( ?, ? )"
  getNoTags <- prepare db "select tag from all_tags as t1 where not exists (select * from tags as t2 where story_id = ? and t2.tag = t1.tag) order by tag asc"
  getTags <- prepare db "select tag from tags where story_id = ? order by tag asc"
  prune <- prepare db "select del_story( ? )"
  storyLookup <- prepare db "select story_id from stories where not pruned and story_id :: text like ? order by story_id asc"
  tagLookupExists <- prepare db "select tag from tags where story_id = ? and tag like ? order by tag asc"
  tagLookupNoExists <- prepare db "select tag from all_tags as t1 where not exists (select * from tags as t2 where story_id = ? and t2.tag = t1.tag) and tag like ? order by tag asc"

  let

    ?db = db

    ?addTag = addTag
    ?delTag = delTag
    ?getNoTags = getNoTags
    ?getTags = getTags
    ?prune = prune
    ?storyLookup = storyLookup
    ?tagLookupExists = tagLookupExists
    ?tagLookupNoExists = tagLookupNoExists

  act

completeStory :: String -> DB (IO [String])
completeStory partial = do

  execute ?storyLookup [toSql $ partial ++ "%"]
  stories <- fetchAllRows' ?storyLookup

  return [ fromSql s | [s] <- stories ]

completeTag :: String -> String -> DB (IO [String])
completeTag story "" = do

  execute ?getNoTags [toSql story]
  hasntTags <- fetchAllRows' ?getNoTags

  execute ?getTags [toSql story]
  hasTags <- fetchAllRows' ?getTags

  return $ [ '-' : fromSql s ++ " " | [s] <- hasTags ] ++ [ fromSql s ++ " " | [s] <- hasntTags ]

completeTag story ('-':partial) = do

  execute ?tagLookupExists [toSql story, toSql $ partial ++ "%"]
  tags <- fetchAllRows' ?tagLookupExists

  return [ '-' : fromSql s ++ " " | [s] <- tags ]

completeTag story partial = do

  execute ?tagLookupNoExists [toSql story, toSql $ partial ++ "%"]
  tags <- fetchAllRows' ?tagLookupNoExists

  return [ fromSql s ++ " " | [s] <- tags ]

main :: IO ()
main = withDB $ do

  el <- elInit "ffs"

  setBasicWordBreakCharacters " "
  setCompleterWordBreakCharacters " "
  setCompleterQuoteCharacters ""
  setSpecialPrefixes ""
  setCompletionAppendCharacter Nothing

  promptStory

promptStory :: DB (IO ())
promptStory = do

  setCompletionEntryFunction $ Just completeStory

  cmd <- readline "> "

  case cmd of
    Nothing -> return ()
    Just "" -> return ()
    Just story -> do
      promptTag story
      promptStory

handleTag :: String -> String -> DB (IO ())
handleTag story "" = return ()
handleTag story ('-':tag) = do
  execute ?delTag [toSql story, toSql tag]
  fetchAllRows' ?delTag
  return ()
handleTag story tag = do
  execute ?addTag [toSql story, toSql tag]
  fetchAllRows' ?addTag
  return ()

promptTag :: String -> DB (IO ())
promptTag story = do

  execute ?getTags [toSql story]
  tags <- fetchAllRows' ?getTags
  putStrLn $ intercalate " " $ ["["] ++ [ fromSql s | [s] <- tags ] ++ ["]"]

  setCompletionEntryFunction $ Just $ completeTag story

  cmd <- readline $ story ++ "> "

  case cmd of
    Nothing -> return ()
    Just "" -> return ()
    Just "--" -> do
      execute ?prune [toSql story]
      fetchAllRows' ?prune
      commit ?db
    Just tags -> do
      mapM_ (handleTag story) $ endBy " " tags
      commit ?db
      promptTag story
