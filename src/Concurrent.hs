-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Concurrent
  ( Worker
  , newWorker
  , defer
  , immediate
  )
where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import Network.Browser

data Command c where
  Command :: Chan a -> BrowserAction c a -> Command c

newtype Worker c = Worker (forall a . Chan a -> BrowserAction c a -> IO ())

newWorker :: IO (Worker c)
newWorker = do
  commandChan <- newChan

  forkIO $ browse $ do
    setAllowRedirects True

    forever $ do
      Command responseChan cmd <- liftIO $ readChan commandChan
      cmd >>= (liftIO . writeChan responseChan)

  return $ Worker $ \responseChan cmd -> writeChan commandChan $ Command responseChan cmd

defer :: Worker c -> Chan a -> BrowserAction c a -> IO ()
defer (Worker worker) = worker

immediate :: Worker c -> BrowserAction c a -> IO a
immediate (Worker worker) cmd = do
  chan <- newChan
  worker chan cmd
  readChan chan
