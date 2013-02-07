-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE Arrows #-}

module Source.HPFanficArchiveCom
  ( fetchHPFanficArchiveCom
  , infoHPFanficArchiveCom
  )
where

import Data.DateTime
import Data.Char
import Data.List
import Data.List.Split
import Text.Regex.Posix
import Text.XML.HXT.Cache
import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import qualified Text.XML.HXT.DOM.XmlNode as XN

fetchHPFanficArchiveCom :: String -> IOStateArrow s b XmlTree
fetchHPFanficArchiveCom storyID = proc _ -> do

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

infoHPFanficArchiveCom :: String -> IOStateArrow s b (String, String, DateTime)
infoHPFanficArchiveCom = fetchAuxilliary

downloadXML :: String -> Int -> IOStateArrow s b XmlTree
downloadXML storyID chapter = proc _ -> do

  readDocument
    -- We use a cache because the auxilliary would otherwise require a separate fetch.
    [ withCache "cache" 3600 False
    , withHTTP []
    , withInputEncoding isoLatin1
    , withParseHTML True
    , withStrictInput True
    , withWarnings False
    ] $ uri -< ()

  where
    uri = "http://www.hpfanficarchive.com/stories/viewstory.php?action=printable&sid=" ++ storyID ++ "&chapter=" ++ show chapter

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

  case bodyEs of
    [] -> zeroArrow -< ()
    _ -> do

      section <- eelem "section" >>> setChildren (titleE : bodyEs) -<< ()

      remainder <- listA $ fetchChapter storyID $ chapter + 1 -< xml

      unlistA -< section : remainder

storyTitle :: IOStateArrow s XmlTree String
storyTitle = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "pagetitle") />
  hasName "a" >>> hasAttrValue "href" (isPrefixOf "viewstory.php") />
  getText

storyAuthor :: IOStateArrow s XmlTree String
storyAuthor = single $ deepest $
  hasName "div" >>> hasAttrValue "id" (== "pagetitle") />
  hasName "a" >>> hasAttrValue "href" (isPrefixOf "viewuser.php") />
  getText

updateDate :: IOStateArrow s XmlTree DateTime
updateDate = single $ deepest $
  hasName "div" >>> hasAttrValue "class" (== "infobox") />
  (listA $ deep getText) >>> arr concat >>> (matchUpdated <+> matchPublished)

matchUpdated :: IOStateArrow s String DateTime
matchUpdated = proc text -> do
  res <- arr (\t -> getAllTextSubmatches $ t =~ "Updated: (January|February|March|April|May|June|July|August|September|October|November|December) ([0-9]{1,2}), ([0-9]{4})") -< text
  case res of
    [_,month,day,year] -> do returnA -< fromGregorian' (read year) (parseMonth month) (read day)
    _ -> do none -< ()

matchPublished :: IOStateArrow s String DateTime
matchPublished = proc text -> do
  res <- arr (\t -> getAllTextSubmatches $ t =~ "Published: (January|February|March|April|May|June|July|August|September|October|November|December) ([0-9]{1,2}), ([0-9]{4})") -< text
  case res of
    [_,month,day,year] -> returnA -< fromGregorian' (read year) (parseMonth month) (read day)
    _ -> none -< ()

parseMonth :: String -> Int
parseMonth "January"    = 1
parseMonth "February"   = 2
parseMonth "March"      = 3
parseMonth "April"      = 4
parseMonth "May"        = 5
parseMonth "June"       = 6
parseMonth "July"       = 7
parseMonth "August"     = 8
parseMonth "September"  = 9
parseMonth "October"    = 10
parseMonth "November"   = 11
parseMonth "December"   = 12
parseMonth m            = error $ "parseMonth called with invalid month: " ++ show m

chapterTitle :: IOStateArrow s XmlTree String
chapterTitle = deepest $
  hasName "div" >>> hasAttrValue "class" (== "chaptertitle") />
  getText >>> arr (head . splitOn " by ")

bodyHTML :: IOStateArrow s XmlTree XmlTree
bodyHTML = deepest $
  hasName "div" >>> hasAttrValue "class" (== "chapter")

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
    , hasName "strong" >>> listA getChildren >>> proc cs -> eelem "strong" >>> setChildren cs -<< ()
    , hasName "em" >>> listA getChildren >>> proc cs -> eelem "emphasis" >>> setChildren cs -<< ()

    -- We have to do some special handling to break paragraphs up on <br/> nodes.
    , hasName "p" >>> listA getChildren >>> proc cs -> catA [ eelem "p" >>> setChildren cs' | cs' <- splitWhen (\n -> XN.getLocalPart n == Just "br") cs, not $ null cs' ] -<< ()

    -- Because we are processing bottom-up, in order for the above to work we need to pass <br/> nodes through.
    , hasName "br" >>> proc _ -> eelem "br" -< ()

    -- Markup nodes that we just ditch and use the children from.
    , hasName "div" >>> getChildren
    ]

    -< n

  -- If we didn't get any acceptable result from processing this node, then ditch it and try to process its children.
  case res of
    --[] -> (getName >>> arr ("unrecognized node: " ++) >>> mkCmt) <+> getChildren -< n
    [] -> getChildren -< n
    _  -> unlistA -< res
