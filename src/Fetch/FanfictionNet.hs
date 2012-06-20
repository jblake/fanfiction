-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fetch.FanfictionNet
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

        page = fixTags $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp

        header = snd $ getTag (~== TagOpen ("div" :: T.Text) [("id", "content")]) page

        titleText = case snd $ getTag (~== TagOpen ("b" :: T.Text) []) header of
          [TagText t] -> t
          x -> error $ "Can't parse title in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n ++ " (" ++ show x ++ ")"

        authorText = case snd $ getTag (~== TagOpen ("a" :: T.Text) []) header of
          [TagText t] -> t
          _ -> error $ "Can't parse author in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n

        chpTitleText = case lastMay $ filter (~== TagText ("" :: T.Text)) header of
          Just (TagText t) | t /= "" -> t
          _ -> error $ "Can't parse chapter title in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n

        headerString = concat $ [ T.unpack t | TagText t <- filter (~== TagText ("" :: T.Text)) header ]

        postedRegexMDY = makeRegex ("P:([0-9]{1,2})-([0-9]{1,2})-([0-9]{2})" :: String) :: Regex
        postedRegexMD = makeRegex ("P:([0-9]{1,2})-([0-9]{1,2})" :: String) :: Regex
        updatedRegexMDY = makeRegex ("U:([0-9]{1,2})-([0-9]{1,2})-([0-9]{2})" :: String) :: Regex
        updatedRegexMD = makeRegex ("U:([0-9]{1,2})-([0-9]{1,2})" :: String) :: Regex

        infoUpdated = case updatedRegexMDY `match` headerString of
          [[_,m,d,y]] -> UTCTime (fromGregorian (fixYear $ read y) (read m) (read d)) 0
          _ -> case updatedRegexMD `match` headerString of
            [[_,m,d]] -> UTCTime (fromGregorian thisYear (read m) (read d)) 0
            _ -> case postedRegexMDY `match` headerString of
              [[_,m,d,y]] -> UTCTime (fromGregorian (fixYear $ read y) (read m) (read d)) 0
              _ -> case postedRegexMD `match` headerString of
                [[_,m,d]] -> UTCTime (fromGregorian thisYear (read m) (read d)) 0
                _ -> error $ "Can't parse date in fanfiction.net/" ++ infoStoryID ++ "/" ++ show n

        chpContent = snd $ getTag (~== TagOpen ("div" :: T.Text) [("id", "storycontent")]) page

        infoTitle = T.unpack titleText
        infoAuthor = T.unpack authorText
        infoChapter = (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` renderTags [TagText chpTitleText] `T.append` "</title><style type=\"text/css\">div,p{text-align:justify;text-justify:newspaper;}div.sep,hr{background-color:#000;height:1px;width:100%;}</style></head><body>" `T.append` renderTags chpContent `T.append` "</body></html>")

      case chpContent of
        [] -> return Nothing
        _ -> return $ force $ Just $ Info {..}

    _ -> return Nothing

parse :: String -> Maybe String
parse url = do
  [[_,_,ref]] <- matchM storyPage url
  return ref
  where

    storyPage = makeRegex ("^http://(www\\.|m\\.|)fanfiction.net/s/([0-9]+)" :: String) :: Regex
