-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module EPub
  ( EPub(..)
  , compileEPub
  )
where

import Codec.Archive.Zip
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Lazy
import Data.ByteString.Lazy.UTF8
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.XML.Light

import Cover
import Fetch

data EPub = EPub
  { uniqueID :: String
  , title    :: String
  , author   :: String
  , modified :: UTCTime
  , chapters :: [(String, ByteString)]
  }

instance NFData EPub where
  rnf (EPub {..}) = uniqueID `deepseq` title `deepseq` author `deepseq` modified `deepseq` chapters `deepseq` ()

type XML = State Element

runXML :: String -> XML () -> ByteString
runXML name xml = fromString $ showTopElement $ execState xml $ node (unqual name) ()

child :: String -> XML () -> XML ()
child name xml = do
  parent <- get
  put $ unode name ()
  xml
  modify $ \c -> parent { elContent = elContent parent ++ [Elem c] }

text :: String -> XML ()
text t = modify $ \x -> x { elContent = elContent x ++ [Text $ CData CDataText t Nothing] }

attr :: String -> String -> XML ()
attr name val = modify $ add_attr $ Attr (unqual name) val

compileEPub :: EPub -> IO ByteString
compileEPub (EPub {..}) = do

  let

    textSize = sum [ Data.ByteString.Lazy.length $ snd c | c <- chapters ]

  coverData <- makeCover uniqueID title author textSize

  let

    modTime = round $ utcTimeToPOSIXSeconds modified

    mimetype = toEntry "mimetype" modTime $ fromString "application/epub+zip"

    containerxml = toEntry "META-INF/container.xml" modTime $ runXML "container" $ do
      attr "version" "1.0"
      attr "xmlns" "urn:oasis:names:tc:opendocument:xmlns:container"
      child "rootfiles" $ do
        child "rootfile" $ do
          attr "full-path" "OPS/book.opf"
          attr "media-type" "application/oebps-package+xml"

    bookopf = toEntry "OPS/book.opf" modTime $ runXML "package" $ do
      attr "version" "2.0"
      attr "xmlns" "http://www.idpf.org/2007/opf"
      attr "unique-identifier" uniqueID

      child "metadata" $ do
        attr "xmlns:dc" "http://purl.org/dc/elements/1.1/"
        child "dc:title" $ text $ title ++ " " ++ uniqueID
        child "dc:identifier" $ do
          attr "id" "BookId"
          text uniqueID
        child "dc:creator" $ text author
        child "meta" $ do
          attr "name" "cover"
          attr "content" "cover"

      child "manifest" $ do

        child "item" $ do
          attr "id" "ncx"
          attr "href" "book.ncx"
          attr "media-type" "application/x-dtbncx+xml"

        child "item" $ do
          attr "id" "cover"
          attr "href" "cover.png"
          attr "media-type" "image/png"
          attr "properties" "cover-image"

        forM_ (Prelude.zip [1..] chapters) $ \(n, (ctitle, content)) -> do
          child "item" $ do
            attr "id" $ "chapter" ++ show n
            attr "href" $ "chapter" ++ show n ++ ".xhtml"
            attr "media-type" "application/xhtml+xml"

      child "spine" $ do
        attr "toc" "ncx"

        forM_ (Prelude.zip [1..] chapters) $ \(n, (ctitle, content)) -> do
          child "itemref" $ attr "idref" $ "chapter" ++ show n

    bookncx = toEntry "OPS/book.ncx" modTime $ runXML "ncx" $ do
      attr "version" "2005-1"
      attr "xmlns" "http://www.daisy.org/z3986/2005/ncx/"

      child "head" $ do

        child "meta" $ do
          attr "name" "dtb:uid"
          attr "content" uniqueID

        child "meta" $ do
          attr "name" "dtb:depth"
          attr "content" "1"

      child "docTitle" $ child "text" $ text $ title ++ " " ++ show uniqueID
      child "docAuthor" $ child "text" $ text author

      child "navMap" $ do

        forM_ (Prelude.zip [1..] chapters) $ \(n, (ctitle, content)) -> do
          child "navPoint" $ do
            attr "class" "chapter"
            attr "id" $ "chapter" ++ show n
            attr "playOrder" $ show n

            child "navLabel" $ child "text" $ text ctitle
            child "content" $ attr "src" $ "chapter" ++ show n ++ ".xhtml"

    cover = toEntry "OPS/cover.png" modTime coverData

    chapterEntries = [ toEntry ("OPS/chapter" ++ show n ++ ".xhtml") modTime $ content |  (n, (ctitle, content)) <- Prelude.zip [1..] chapters ]

  return $ fromArchive $ Prelude.foldr addEntryToArchive emptyArchive (mimetype : containerxml : bookopf : bookncx : cover : chapterEntries)
