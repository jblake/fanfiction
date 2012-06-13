-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Fetch
where

import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock

import EPub

data Info = Info
  { infoUnique :: String
  , infoStoryID :: String
  , infoTitle :: String
  , infoAuthor :: String
  , infoUpdated :: UTCTime
  , infoChapter :: (String, BS.ByteString)
  }
