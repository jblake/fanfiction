-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Fetch
where

import Control.DeepSeq
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock

data Info = Info
  { infoUnique :: String
  , infoStoryID :: String
  , infoTitle :: String
  , infoAuthor :: String
  , infoUpdated :: UTCTime
  , infoChapter :: (String, BS.ByteString)
  }

instance NFData BS.ByteString where
  rnf bs = rnf $ BS.length bs

instance NFData Info where
  rnf (Info {..}) = infoUnique `deepseq` infoStoryID `deepseq` infoTitle `deepseq` infoAuthor `deepseq` infoUpdated `deepseq` infoChapter `deepseq` ()
