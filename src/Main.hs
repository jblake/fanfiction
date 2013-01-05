-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Monad
import Data.Char
import Data.Data
import Data.DateTime
import Data.List
import Data.Maybe
import System.Console.CmdArgs.Implicit
import System.Directory
import System.IO.Error
import System.Posix.Files
import Text.Regex.Posix
import Text.XML.HXT.Core

import Database.Local
import Database.MoonReader
import Source.FanfictionNet

data RunMode
  = Add
    { sources :: [String]
    }
  | Update
  | Prune
    { storyIDs :: [String]
    }
  deriving (Data, Typeable)

main :: IO ()
main = do

  mode <- cmdArgs $ modes
    [ Add
      { sources = [] &= args
      } &= help "Add new stories to the database"
    , Update &= help "Update all ebooks to their latest revisions"
    , Prune
      { storyIDs = [] &= args
      } &= help "Prune ebooks that aren't worth updating"
    ] &= program "ff" &= summary "Fan fiction ebook library manager"

  runMode mode

runMode :: RunMode -> IO ()

runMode (Add {..}) = do
  local <- connectLocal

  let
    addSource source = msum
      [ do
        [_,_,ref] <- listToMaybe $ source =~ "^http://(m|www)\\.fanfiction\\.net/s/([0-9]+)"
        return $ do
          storyID <- addStory local "fanfiction.net" ref
          putStrLn $ source ++ " -> fanfiction.net:" ++ ref ++ " -> " ++ storyID
      ]

  forM_ sources $ \source -> case addSource source of
    Nothing -> putStrLn $ "Unrecognized source " ++ source
    Just act -> act

runMode Update = do
  local <- connectLocal
  moon <- connectMoonReader

  dead <- deadBooks local
  rmBooks moon dead
  forM_ dead $ \filename -> do
    e <- tryIOError $ removeFile $ "books/" ++ filename
    case e of
      Right _ -> return ()
      Left e | isDoesNotExistError e -> return ()
             | otherwise -> ioError e

  let
    processBook storyID [] = putStrLn $ "No sources for " ++ storyID ++ "!"

    processBook storyID (("fanfiction.net", ref):sources) = do

      info <- runX $ infoFanfictionNet ref

      case info of
        (title,author,date):_ -> do

          filename <- makeFilename local storyID $ defaultFilename storyID title author

          let
            getBook = do
              putStrLn $ "   Fetching " ++ storyID ++ " from fanfiction.net:" ++ ref

              book <- runX $ fetchFanfictionNet ref >>> writeDocument [] ("books/" ++ filename)

              case book of
                [] -> do
                  putStrLn $ "Can't fetch fanfiction.net:" ++ ref ++ " for " ++ storyID ++ "!"
                  processBook storyID sources
                _ -> do
                  setFileTimes ("books/" ++ filename) (fromIntegral $ toSeconds date) (fromIntegral $ toSeconds date)
                  touchBook moon filename (title, author, date)

          e <- tryIOError $ getFileStatus $ "books/" ++ filename
          case e of
            Right s | fromIntegral (toSeconds date) > modificationTime s -> getBook
                    | otherwise -> return ()
            Left e | isDoesNotExistError e -> getBook
                   | otherwise -> ioError e

        _ -> do
          putStrLn $ "Can't get info fanfiction.net:" ++ ref ++ " for " ++ storyID ++ "!"
          processBook storyID sources

    processBook storyID ((source, ref):sources) = do
      putStrLn $ "Unsupported source " ++ source ++ ":" ++ ref ++ " for " ++ storyID ++ "!"
      processBook storyID sources

  foreachBook local processBook

runMode (Prune {..}) = do
  local <- connectLocal
  pruneBooks local storyIDs

defaultFilename :: String -> String -> String -> String
defaultFilename storyID title author = mangle storyID ++ "_" ++ mangle title ++ "_by_" ++ mangle author ++ ".fb2"
  where
    mangle s = intercalate "_" $ filter (/= "_") $ groupBy (\a b -> a /= '_' && b /= '_') $ [ if allowed c then c else '_' | c <- s ]
    allowed c = isAscii c && isAlphaNum c
