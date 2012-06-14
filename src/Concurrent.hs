-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Concurrent
  ( Job
  , Worker
  , Work
  , newWorker
  , defer
  , force
  , eval
  , pass
  )
where

import Control.Concurrent
import Control.Concurrent.Chan.Strict
import Control.Concurrent.MVar.Strict
import Control.Concurrent.QSem
import Control.Monad
import Control.Monad.Exception.Synchronous hiding
  ( force
  )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data Pass a where Pass :: Worker m -> ExceptionalT (Pass a) m a -> Pass a
data Command m where Command :: ExceptionalT (Pass a) m a -> MVar a -> Maybe QSem -> Command m

newtype Job a = Job (MVar a)
newtype Worker m = Worker (Chan (Command m))

type Work m a = ExceptionalT (Pass a) m

instance (MonadIO m) => MonadIO (Work m a) where
  liftIO = lift . liftIO

newWorker :: (MonadIO m, MonadIO o) => (m () -> IO ()) -> o (Worker m)
newWorker runM = liftIO $ do
  chan <- newChan
  forkIO $ runM $ forever $ do
    Command act result sem <- liftIO $ readChan chan
    mx <- runExceptionalT act
    case mx of
      Success x -> liftIO $ putMVar result x >> maybe (return ()) signalQSem sem
      Exception (Pass (Worker chan') act') -> liftIO $ writeChan chan' $ Command act' result sem
  return $ Worker chan

defer :: (MonadIO o) => Worker m -> Maybe QSem -> Work m a a -> o (Job a)
defer (Worker chan) sem act = liftIO $ do
  result <- newEmptyMVar
  maybe (return ()) waitQSem sem
  writeChan chan $ Command act result sem
  return $ Job result

force :: (MonadIO o) => Job a -> o a
force (Job result) = liftIO $ takeMVar result

eval :: (MonadIO o) => Worker m -> Maybe QSem -> Work m a a -> o a
eval worker sem act = defer worker sem act >>= force

pass :: (MonadIO o) => Worker m -> Work m a a -> Work o a b
pass worker act = throwT $ Pass worker act
