-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE Arrows #-}

module Source.FanfictionNet
  ( fetchFanfictionNet
  , infoFanfictionNet
  )
where

import Data.DateTime
import Data.Char
import Data.List
import Text.Regex.Posix
import Text.XML.HXT.Cache
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

fetchFanfictionNet :: String -> IOStateArrow s b XmlTree
fetchFanfictionNet storyID = proc _ -> do

  (title, author, date) <- fetchAuxilliary storyID -< ()

  sections <- listA $ fetchChapter storyID 1 -< ()

  root []
    [ mkelem "FictionBook" [sattr "xmlns" "http://www.gribuser.ru/xml/fictionbook/2.0"]
      [ selem "description"
        [ selem "title-info"
          [ selem "author"
            [ selem "nickname"
              [ txt author
              ]
            ]
          , selem "book-title"
            [ txt title
            ]
          ]
        , selem "document-info"
          [ selem "author"
            [ selem "nickname"
              [ txt "jblake"
              ]
            ]
          , selem "date"
            [ txt $ formatDateTime "%F" date
            ]
          , selem "program-used"
            [ txt "ff"
            ]
          ]
        ]
      , eelem "body" >>> setChildren sections
      ]
    ] -<< ()

infoFanfictionNet :: String -> IOStateArrow s b (String, String, DateTime)
infoFanfictionNet = fetchAuxilliary

downloadXML :: String -> Int -> IOStateArrow s b XmlTree
downloadXML storyID chapter = proc _ -> do

  readDocument
    -- We use a cache because the auxilliary would otherwise require a separate fetch.
    [ withCache "cache" 3600 False
    , withHTTP []
    , withParseHTML True
    , withStrictInput True
    , withWarnings False
    ] $ uri -< ()

  where
    uri = "http://m.fanfiction.net/s/" ++ storyID ++ "/" ++ show chapter

fetchAuxilliary :: String -> IOStateArrow s b (String, String, DateTime)
fetchAuxilliary storyID = proc _ -> do

  xml <- downloadXML storyID 1 -< ()

  title <- storyTitle -< xml
  author <- storyAuthor -< xml
  date <- updateDate -< xml

  returnA -< (title, author, date)

fetchChapter :: String -> Int -> IOStateArrow s b XmlTree
fetchChapter storyID chapter = proc _ -> do

  xml <- downloadXML storyID chapter -< ()

  title <- single $ chapterTitle <+> constA ("Chapter " ++ show chapter) -< xml
  titleE <- selem "title" [ selem "p" [ mkText ] ] -< title

  body <- bodyHTML -< xml
  bodyEs <- listA $ processBottomUp formatBody -< body

  section <- eelem "section" >>> setChildren (titleE : bodyEs) -<< ()

  remainder <- ifA nextChapterLink (constA () >>> (listA $ fetchChapter storyID $ chapter + 1)) (constA []) -< xml

  unlistA -< section : remainder

storyTitle :: IOStateArrow s XmlTree String
storyTitle = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "content") />
  hasName "center" />
  hasName "b" />
  getText

storyAuthor :: IOStateArrow s XmlTree String
storyAuthor = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "content") />
  hasName "center" />
  hasName "a" >>> hasAttr "href" />
  getText

updateDate :: IOStateArrow s XmlTree DateTime
updateDate = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "content") />
  getText >>> (matchUpdated <+> matchPublished)

matchUpdated :: IOStateArrow s String DateTime
matchUpdated = proc text -> do
  res <- arr (\t -> getAllTextSubmatches $ t =~ "Updated: ([0-9]{1,2})-([0-9]{1,2})-([0-9]{1,2})") -< text
  case res of
    [_,month,day,year] -> do returnA -< fromGregorian' (let y = read year in if y < 90 then y + 2000 else y + 1900) (read month) (read day)
    _ -> do none -< ()

matchPublished :: IOStateArrow s String DateTime
matchPublished = proc text -> do
  res <- arr (\t -> getAllTextSubmatches $ t =~ "Published: ([0-9]{1,2})-([0-9]{1,2})-([0-9]{1,2})") -< text
  case res of
    [_,month,day,year] -> returnA -< fromGregorian' (let y = read year in if y < 90 then y + 2000 else y + 1900) (read month) (read day)
    _ -> none -< ()

-- This could probably be made more robust. Look for the last text node in that block, maybe?
chapterTitle :: IOStateArrow s XmlTree String
chapterTitle = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "content") />
  getText >>> arr (reverse . dropWhile isSpace . reverse . dropWhile isSpace) >>> guardsP (isPrefixOf "Chapter") returnA

bodyHTML :: IOStateArrow s XmlTree XmlTree
bodyHTML = single $ deep $
  hasName "div" >>> hasAttrValue "id" (== "storycontent")

nextChapterLink :: IOStateArrow s XmlTree ()
nextChapterLink = single $ deepest $
  hasName "a" >>> hasAttr "href" />
  hasText (== "Next »") >>> constA ()

formatBody :: IOStateArrow s XmlTree XmlTree
formatBody = proc n -> do

  res <- listA $ catA

    -- Primitive nodes that we copy directly.
    [ isBlob
    , isCdata
    , isCharRef
    , isEntityRef
    , isText

    -- Horizontal rules are replaced.
    , hasName "hr" >>> proc _ -> eelem "empty-line" -< ()

    -- Markup nodes that we rewrite as a different type.
    , hasName "b" >>> listA getChildren >>> proc cs -> eelem "strong" >>> setChildren cs -<< ()
    , hasName "i" >>> listA getChildren >>> proc cs -> eelem "emphasis" >>> setChildren cs -<< ()
    , hasName "p" >>> listA getChildren >>> proc cs -> eelem "p" >>> setChildren cs -<< ()

    -- Markup nodes that we just ditch and use the children from.
    , hasName "div" >>> getChildren
    ]

    -< n

  -- If we didn't get any acceptable result from processing this node, then ditch it and try to process its children.
  case res of
    [] -> (getName >>> arr ("unrecognized node: " ++) >>> mkCmt) <+> getChildren -< n
    _  -> unlistA -< res
