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
import Data.Time.Calendar
import Data.Time.Clock
import Network.Browser
import Network.HTTP
import Network.URI
import System.Locale
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Regex.Posix

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

      now <- liftIO getCurrentTime

      let
        (thisYear, _, _) = toGregorian $ utctDay now
        thisCentury = 100 * (thisYear `div` 100)
        fixYear y | y + thisCentury > thisYear = y + thisCentury - 100
                  | otherwise                  = y + thisCentury
        body = tagTree $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp
        header = [ t | t@(TagBranch "center" _ _) <- universeTree body ]
        titleText = case parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "b" _ cs) <- universeTree header ] of
          [TagText t] -> t
          _ -> error $ "Can't parse title in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n
        authorText = case parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "a" _ cs) <- universeTree header ] of
          [TagText t] -> t
          _ -> error $ "Can't parse author in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n
        miscInfo = head [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "content") `elem` as ]
        chpTitle = T.strip $ renderTags $ reverse $ takeWhile (~== TagText ("" :: T.Text)) $ dropWhile (~/= TagText ("" :: T.Text)) $ reverse $ flattenTree miscInfo
        flatMiscInfo = T.unpack $ renderTags [ t | t <- flattenTree miscInfo, t ~== TagText ("" :: T.Text) ]
        postedRegexMDY = makeRegex ("P:([0-9]{1,2})-([0-9]{1,2})-([0-9]{2})" :: String) :: Regex
        postedRegexMD = makeRegex ("P:([0-9]{1,2})-([0-9]{1,2})" :: String) :: Regex
        updatedRegexMDY = makeRegex ("U:([0-9]{1,2})-([0-9]{1,2})-([0-9]{2})" :: String) :: Regex
        updatedRegexMD = makeRegex ("U:([0-9]{1,2})-([0-9]{1,2})" :: String) :: Regex
        infoUpdated = case updatedRegexMDY `match` flatMiscInfo of
          [[_,m,d,y]] -> UTCTime (fromGregorian (fixYear $ read y) (read m) (read d)) 0
          _ -> case updatedRegexMD `match` flatMiscInfo of
            [[_,m,d]] -> UTCTime (fromGregorian thisYear (read m) (read d)) 0
            _ -> case postedRegexMDY `match` flatMiscInfo of
              [[_,m,d,y]] -> UTCTime (fromGregorian (fixYear $ read y) (read m) (read d)) 0
              _ -> case postedRegexMD `match` flatMiscInfo of
                [[_,m,d]] -> UTCTime (fromGregorian thisYear (read m) (read d)) 0
                _ -> error $ "Can't parse date in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n
        chpTitleText = case parseTags chpTitle of
          [TagText t] -> t
          [] -> ""
          _ -> error $ "Can't parse chapter title in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n
        chpContent = T.strip $ renderTags $ flattenTree $ concat [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "storycontent") `elem` as ]
        infoTitle = T.unpack titleText
        infoAuthor = T.unpack authorText
        infoChapter = (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` chpTitle `T.append` "</title><style type=\"text/css\">p{text-align:justify;text-justify:newspaper;}</style></head><body>" `T.append` chpContent `T.append` "</body></html>")

      case header of
        [] -> return Nothing
        _ -> return $ Just $ Info {..}

    _ -> return Nothing
