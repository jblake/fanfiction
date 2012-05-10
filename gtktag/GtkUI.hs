-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GtkUI
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Language.Haskell.TH

data TagBackend = TagBackend
  { getTags :: String -> String -> IO [(String, Bool)]
  , setTag :: String -> String -> IO ()
  , clearTag :: String -> String -> IO ()
  , endUI :: IO ()
  }

tagWindowXML :: String
tagWindowXML = $( runIO (readFile "tagwin.glade") >>= stringE )

initTagWindow :: TagBackend -> IO ()
initTagWindow (TagBackend {..}) = do

  initialTags <- getTags "" ""
  tagStore <- listStoreNew initialTags

  builder <- builderNew
  builderAddFromString builder tagWindowXML

  tagWindow <- builderGetObject builder castToWindow "tagWindow"
  itemEntry <- builderGetObject builder castToEntry "itemEntry"
  tagSearchEntry <- builderGetObject builder castToEntry "tagSearchEntry"
  addTag <- builderGetObject builder castToButton "addTag"
  tagView <- builderGetObject builder castToIconView "tagView"
  tagToggle <- builderGetObject builder castToCellRendererToggle "tagToggle"
  tagName <- builderGetObject builder castToCellRendererText "tagName"

  tagWindow `on` deleteEvent $ liftIO endUI >> return True

  let
    updateTags = do
      item <- editableGetChars itemEntry 0 (-1)
      tagSearch <- editableGetChars tagSearchEntry 0 (-1)
      tags <- getTags item tagSearch
      listStoreClear tagStore
      mapM_ (listStoreAppend tagStore) tags

  itemEntry `on` editableChanged $ liftIO updateTags

  tagSearchEntry `on` editableChanged $ liftIO updateTags

  addTag `on` buttonActivated $ do
    item <- editableGetChars itemEntry 0 (-1)
    tag <- editableGetChars tagSearchEntry 0 (-1)
    when ( item /= "" && tag /= "" ) $ setTag item tag >> updateTags

  iconViewSetModel tagView $ Just tagStore

  cellLayoutSetAttributes tagView tagToggle tagStore $ \(name, set) -> [ cellToggleActive := set ]
  cellLayoutSetAttributes tagView tagName   tagStore $ \(name, set) -> [ cellText := name, cellTextForeground := if set then "#ff0000" else "#000000" ]

  set tagToggle [ cellToggleActivatable := True ]
  set tagName [ cellTextFont := "xx-large" ]

  tagToggle `on` cellToggled $ \pathStr -> do
    let [index] = stringToTreePath pathStr
    item <- editableGetChars itemEntry 0 (-1)
    (tag, oldSet) <- listStoreGetValue tagStore index
    if oldSet
      then clearTag item tag
      else setTag item tag
    updateTags

  widgetShowAll tagWindow

filterTags :: (String -> IO [(String, Bool)]) -> (String -> String -> IO [(String, Bool)])
filterTags getTags item tagSearch = do
  allTags <- getTags item
  return $ filter (\(tag, _) -> tagSearch `isInfixOf` tag) allTags
