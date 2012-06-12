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
import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Lazy
import Data.ByteString.Lazy.UTF8
import Text.XML.Light

data EPub = EPub
  { uniqueID :: String
  , title    :: String
  , author   :: String
  , chapters :: [(String, ByteString)]
  }
  deriving (Read, Show)

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

compileEPub :: EPub -> ByteString
compileEPub (EPub {..}) = fromArchive $ Prelude.foldr addEntryToArchive emptyArchive (mimetype : containerxml : bookopf : bookncx : chapterEntries)
  where

    mimetype = toEntry "mimetype" 0 $ fromString "application/epub+zip"

    containerxml = toEntry "META-INF/container.xml" 0 $ runXML "container" $ do
      attr "version" "1.0"
      attr "xmlns" "urn:oasis:names:tc:opendocument:xmlns:container"
      child "rootfiles" $ do
        child "rootfile" $ do
          attr "full-path" "OPS/book.opf"
          attr "media-type" "application/oebps-package+xml"

    bookopf = toEntry "OPS/book.opf" 0 $ runXML "package" $ do
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

      child "manifest" $ do

        child "item" $ do
          attr "id" "ncx"
          attr "href" "book.ncx"
          attr "media-type" "application/x-dtbncx+xml"

        forM_ (Prelude.zip [1..] chapters) $ \(n, (ctitle, content)) -> do
          child "item" $ do
            attr "id" $ "chapter" ++ show n
            attr "href" $ "chapter" ++ show n ++ ".xhtml"
            attr "media-type" "application/xhtml+xml"

      child "spine" $ do
        attr "toc" "ncx"

        forM_ (Prelude.zip [1..] chapters) $ \(n, (ctitle, content)) -> do
          child "itemref" $ attr "idref" $ "chapter" ++ show n

    bookncx = toEntry "OPS/book.ncx" 0 $ runXML "ncx" $ do
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

    chapterEntries = [ toEntry ("OPS/chapter" ++ show n ++ ".xhtml") 0 $ content |  (n, (ctitle, content)) <- Prelude.zip [1..] chapters ]