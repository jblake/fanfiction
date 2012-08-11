-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fetch.HpfanficarchiveCom
  ( peek
  , fetch
  , parse
  )
where

import Codec.Text.IConv
import Control.Applicative
import Control.DeepSeq
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
import Safe
import System.Locale
import Text.HTML.TagSoup
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

  return $ force $ EPub {..}

lowerTags :: [Tag T.Text] -> [Tag T.Text]
lowerTags [] = []
lowerTags ((TagOpen t as):ts) = TagOpen (T.toLower t) as : lowerTags ts
lowerTags ((TagClose t):ts) = TagClose (T.toLower t) : lowerTags ts
lowerTags (t:ts) = t : lowerTags ts

closeTags :: [Tag T.Text] -> [Tag T.Text]
closeTags [] = []
closeTags ((TagOpen "br" as):ts) = TagOpen "br" as : TagClose "br" : closeTags ts
closeTags ((TagOpen "img" as):ts) = TagOpen "img" as : TagClose "img" : closeTags ts
closeTags (t:ts) = t : closeTags ts

fixTags :: [Tag T.Text] -> [Tag T.Text]
fixTags = closeTags . lowerTags

getTag :: (Tag T.Text -> Bool) -> [Tag T.Text] -> ([(T.Text, T.Text)], [Tag T.Text])
getTag cond tags = (as, init $ extractTree [o] tags')
  where

    (o, as, tags') = case dropWhile (not . cond) tags of
      (TagOpen xo xas):xtags' -> (xo, xas, xtags')
      [] -> ("x", [], [TagClose "x"])
      x -> error $ "getTag condition did not find an open tag! (" ++ show x ++ ")"

    extractTree []    _                                           = []
    extractTree s     (t@(TagOpen h _):ts)                        = t : extractTree (h:s) ts
    extractTree (h:s) (t@(TagClose h'):ts) | h == h'              = t : extractTree s ts
                                           | h' `elem` structural = TagClose h : extractTree s (t:ts)
                                           | otherwise            = extractTree (h:s) ts
    extractTree s     (t:ts)                                      = t : extractTree s ts
    extractTree s     []                                          = map TagClose s
    structural = [ "html", "body", "div" ]

monthNumber :: String -> Int
monthNumber "January" = 1
monthNumber "February" = 2
monthNumber "March" = 3
monthNumber "April" = 4
monthNumber "May" = 5
monthNumber "June" = 6
monthNumber "July" = 7
monthNumber "August" = 8
monthNumber "September" = 9
monthNumber "October" = 10
monthNumber "November" = 11
monthNumber "December" = 12
monthNumber m     = error $ "Couldn't understand month " ++ show m

fetchChapter :: Int -> String -> String -> BrowserAction (HandleStream BS.ByteString) (Maybe Info)
fetchChapter n infoUnique infoStoryID = do

  (uri, resp) <- request $ mkRequest GET $ fromJust $ parseURI $ "http://hpfanficarchive.com/stories/viewstory.php?action=printable&ageconsent=ok&warning=5&sid=" ++ urlEncode infoStoryID ++ "&chapter=" ++ urlEncode (show n)

  case rspCode resp of
    (2, 0, 0) -> do

      let

        page = fixTags $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp

        (titleText, authorText) = case snd $ getTag (~== TagOpen ("div" :: T.Text) [("id", "pagetitle")]) page of
          [TagOpen "a" _, TagText t, TagClose "a", TagText _, TagOpen "a" _, TagText a, TagClose "a"] -> (t, a)
          _ -> error $ "Can't parse title/author in hpfanficarchive.com/" ++ infoStoryID ++ "/" ++ show n

        chpTitleText = case snd $ getTag (~== TagOpen ("div" :: T.Text) [("class", "chaptertitle")]) page of
          [TagText t] -> t
          _ -> error $ "Can't find chapter title in hpfanficarchive.com/" ++ infoStoryID ++ "/" ++ show n

        infobox = snd $ getTag (~== TagOpen ("div" :: T.Text) [("class", "infobox")]) page
        infoString = concat $ [ T.unpack t | TagText t <- filter (~== TagText ("" :: T.Text)) infobox ]

        postedRegex = makeRegex ("Published: ([a-zA-Z]+) ([0-9]{1,2}), ([0-9]{4})" :: String) :: Regex
        updatedRegex = makeRegex ("Updated: ([a-zA-Z]+) ([0-9]{1,2}), ([0-9]{4})" :: String) :: Regex

        infoUpdated = case updatedRegex `match` infoString of
          [[_,m,d,y]] -> UTCTime (fromGregorian (read y) (monthNumber m) (read d)) 0
          _ -> case postedRegex `match` infoString of
            [[_,m,d,y]] -> UTCTime (fromGregorian (read y) (monthNumber m) (read d)) 0
            _ -> error $ "Can't parse date in hpfanficarchive.com/" ++ infoStoryID ++ "/" ++ show n

        chpContent = snd $ getTag (~== TagOpen ("div" :: T.Text) [("class", "chapter")]) page

        infoTitle = T.unpack titleText
        infoAuthor = T.unpack authorText
        infoChapter = (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` renderTags [TagText chpTitleText] `T.append` "</title><style type=\"text/css\">div,p{text-align:justify;text-justify:newspaper;}hr{background-color:#000;height:1px;width:100%;}</style></head><body>" `T.append` renderTags chpContent `T.append` "</body></html>")

      case chpContent of
        [] -> return Nothing
        _ -> return $ force $ Just $ Info {..}

    _ -> return Nothing

parse :: String -> Maybe String
parse url = do
  [[_,_,_,ref]] <- matchM storyPage url
  return ref
  where

    storyPage = makeRegex ("^http://(www\\.|)hpfanficarchive.com/stories/viewstory.php\\?(.+&|)sid=([0-9]+)" :: String) :: Regex
