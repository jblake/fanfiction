-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fetch.FanfictionNet
  ( peek
  , fetch
  )
where

import Codec.Text.IConv
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Time.Clock
import Data.Time.Format
import Network.Browser
import Network.HTTP
import Network.URI
import System.Locale
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import EPub
import Fetch

peek :: String -> String -> BrowserAction (HandleStream BS.ByteString) (Maybe Info)
peek = fetchChapter 1

fetch :: Info -> BrowserAction (HandleStream BS.ByteString) EPub
fetch first = do

  let
    fetchRemain n = do

      i <- fetchChapter n (infoUnique first) (infoStoryID first)

      case i of
        Nothing -> return []
        Just next -> do

          remain <- fetchRemain (n + 1)

          return $ infoChapter next : remain

  remain <- fetchRemain 2

  let
    uniqueID = infoUnique first
    title = infoTitle first
    author = infoAuthor first
    modified = infoUpdated first
    chapters = infoChapter first : remain

  return $ EPub {..}

fetchChapter :: Int -> String -> String -> BrowserAction (HandleStream BS.ByteString) (Maybe Info)
fetchChapter n infoUnique infoStoryID = do

  (uri, resp) <- request $ mkRequest GET $ fromJust $ parseURI $ "http://m.fanfiction.net/s/" ++ urlEncode infoStoryID ++ "/" ++ urlEncode (show n)

  case rspCode resp of
    (2, 0, 0) -> do

      let
        body = tagTree $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp
        header = [ t | t@(TagBranch "center" _ _) <- universeTree body ]
        [TagText titleText] = parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "b" _ cs) <- universeTree header ]
        [TagText authorText] = parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "a" _ cs) <- universeTree header ]
        chpTitle = T.strip $ renderTags $ reverse $ takeWhile (~== TagText ("" :: T.Text)) $ dropWhile (~/= TagText ("" :: T.Text)) $ reverse $ flattenTree $ head [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "content") `elem` as ]
        [TagText chpTitleText] = parseTags chpTitle
        chpContent = T.strip $ renderTags $ flattenTree $ concat [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "storycontent") `elem` as ]
        infoTitle = T.unpack titleText
        infoAuthor = T.unpack authorText
        infoUpdated = readTime defaultTimeLocale "%m-%d-%y" "1-1-01"
        infoChapter = (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` chpTitle `T.append` "</title><style type=\"text/css\">p{text-align:justify;text-justify:newspaper;}</style></head><body>" `T.append` chpContent `T.append` "</body></html>")

      case header of
        [] -> return Nothing
        _ -> return $ Just $ Info {..}

    _ -> return Nothing
