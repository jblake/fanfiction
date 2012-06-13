-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Concurrent
  ( Worker
  , newWorker
  , Pipe
  , first
  , (|||)
  , fg
  , bg
  )
where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

data Command m where
  Command :: (a -> IO ()) -> m a -> Command m

newtype Worker m = Worker (Chan (Command m))

newWorker :: (Monad m, MonadIO m) => (m () -> IO ()) -> IO (Worker m)
newWorker run = do
  commandChan <- newChan
  forkIO $ run $ forever $ do
    Command result act <- liftIO $ readChan commandChan
    act >>= liftIO . result
  return $ Worker commandChan

data Pipe r where
  Done :: Worker m -> m a -> Pipe a
  Pass :: Pipe a -> Worker m -> (a -> m b) -> Pipe b

first :: Worker m -> m a -> Pipe a
first = Done

(|||) :: Pipe a -> Worker m -> (a -> m b) -> Pipe b
(|||) = Pass

start :: Pipe r -> (r -> IO ()) -> IO ()
start (Done      (Worker chan) act) result = writeChan chan $ Command result act
start (Pass prev (Worker chan) act) result = start prev $ \x -> writeChan chan $ Command result $ act x

fg :: Pipe r -> IO r
fg pipe = do
  var <- newEmptyMVar
  start pipe $ putMVar var
  takeMVar var

bg :: Pipe () -> IO ()
bg pipe = start pipe $ const $ return ()
