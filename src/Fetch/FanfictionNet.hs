-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fetch.FanfictionNet
where

import Codec.Text.IConv
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Network.Browser
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import EPub

fetch :: String -> String -> IO (Maybe EPub)
fetch uniqueID storyid = browse $ do

  setAllowRedirects True

  (uri, resp) <- request $ mkRequest GET $ fromJust $ parseURI $ "http://m.fanfiction.net/s/" ++ urlEncode storyid ++ "/1"

  case rspCode resp of
    (2, 0, 0) -> do

      let
        body = tagTree $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp
        header = [ t | t@(TagBranch "center" _ _) <- universeTree body ]
        [TagText titleText] = parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "b" _ cs) <- universeTree header ]
        [TagText authorText] = parseTags $ renderTags $ flattenTree $ head [ cs | (TagBranch "a" _ cs) <- universeTree header ]
        title = T.unpack titleText
        author = T.unpack authorText
        chpTitle = T.strip $ renderTags $ reverse $ takeWhile (~== TagText ("" :: T.Text)) $ dropWhile (~/= TagText ("" :: T.Text)) $ reverse $ flattenTree $ head [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "content") `elem` as ]
        [TagText chpTitleText] = parseTags chpTitle
        chpContent = T.strip $ renderTags $ flattenTree $ concat [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "storycontent") `elem` as ]
        fetchRemain n = do

          (uri, resp) <- request $ mkRequest GET $ fromJust $ parseURI $ "http://m.fanfiction.net/s/" ++ urlEncode storyid ++ "/" ++ urlEncode (show n)

          case rspCode resp of
            (2, 0, 0) -> do

              let
                header = [ t | t@(TagBranch "center" _ _) <- universeTree body ]
                body = tagTree $ parseTags $ T.decodeUtf8 $ convertFuzzy Transliterate "utf-8" "utf-8" $ rspBody resp
                chpTitle = T.strip $ renderTags $ reverse $ takeWhile (~== TagText ("" :: T.Text)) $ dropWhile (~/= TagText ("" :: T.Text)) $ reverse $ flattenTree $ head [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "content") `elem` as ]
                [TagText chpTitleText] = parseTags chpTitle
                chpContent = T.strip $ renderTags $ flattenTree $ concat [ cs | (TagBranch "div" as cs) <- universeTree body, ("id", "storycontent") `elem` as ]

              case header of
                [] -> return []
                _ -> do

                  remain <- fetchRemain $ n + 1

                  return $ (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` chpTitle `T.append` "</title><style type=\"text/css\">p{text-align:justify;text-justify:newspaper;}</style></head><body>" `T.append` chpContent `T.append` "</body></html>") : remain

            _ -> return []

      case header of
        [] -> return Nothing
        _ -> do

          remain <- fetchRemain 2

          let
            chapters = (T.unpack chpTitleText, T.encodeUtf8 $ "<?xml version=\"1.0\" encoding=\"utf-8\" ?><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta http-equiv=\"Content-type\" content=\"application/xhtml+xml; charset=utf-8\" /><title>" `T.append` chpTitle `T.append` "</title><style type=\"text/css\">p{text-align:justify;text-justify:newspaper;}</style></head><body>" `T.append` chpContent `T.append` "</body></html>") : remain

          return $ Just $ EPub {..}

    _ -> return Nothing
