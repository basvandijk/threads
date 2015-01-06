{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , ImpredicativeTypes
           , RankNTypes #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread.Group
-- Copyright  : (c) 2010-2012 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module extends @Control.Concurrent.Thread@ with the ability to wait for
-- a group of threads to terminate.
--
-- This module exports equivalently named functions from @Control.Concurrent@,
-- (@GHC.Conc@), and @Control.Concurrent.Thread@. Avoid ambiguities by importing
-- this module qualified. May we suggest:
--
-- @
-- import Control.Concurrent.Thread.Group ( ThreadGroup )
-- import qualified Control.Concurrent.Thread.Group as ThreadGroup ( ... )
-- @
--
--------------------------------------------------------------------------------

module Control.Concurrent.Thread.Group
    ( ThreadGroup
    , new
    , nrOfRunning
    , wait
    , waitN

      -- * Forking threads
    , forkIO
    , forkOS
    , forkOn
    , forkIOWithUnmask
    , forkOnWithUnmask
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent     ( forkOS
                                        , forkIOWithUnmask
                                        , forkOnWithUnmask
                                        )
import Control.Concurrent               ( ThreadId )
import Control.Concurrent.MVar          ( newEmptyMVar, putMVar, readMVar )
import Control.Exception                ( try, mask )
import Control.Monad                    ( return, (>>=), when )
import Data.Function                    ( (.), ($) )
import Data.Functor                     ( fmap )
import Data.Eq                          ( Eq )
import Data.Ord                         ( (>=) )
import Data.Int                         ( Int )
import Data.Typeable                    ( Typeable )
import Prelude                          ( ($!), (+), subtract )
import System.IO                        ( IO )

-- from stm:
import Control.Concurrent.STM.TVar      ( TVar, newTVarIO, readTVar, writeTVar )
import Control.Concurrent.STM           ( STM, atomically, retry )

-- from threads:
import Control.Concurrent.Thread        ( Result )
import Control.Concurrent.Raw           ( rawForkIO, rawForkOn )
#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO
                                                     , forkOS
                                                     , forkOn
                                                     , forkIOWithUnmask
                                                     , forkOnWithUnmask
                                                     )
#endif


--------------------------------------------------------------------------------
-- * Thread groups
--------------------------------------------------------------------------------

{-| A @ThreadGroup@ can be understood as a counter which counts the number of
threads that were added to the group minus the ones that have terminated.

More formally a @ThreadGroup@ has the following semantics:

* 'new' initializes the counter to 0.

* Forking a thread increments the counter.

* When a forked thread terminates, whether normally or by raising an exception,
  the counter is decremented.

* 'nrOfRunning' yields a transaction that returns the counter.

* 'wait' blocks as long as the counter is greater than 0.

* 'waitN' blocks as long as the counter is greater or equal to the
   specified number.
-}
newtype ThreadGroup = ThreadGroup (TVar Int) deriving (Eq, Typeable)

-- | Create an empty group of threads.
new :: IO ThreadGroup
new = fmap ThreadGroup $ newTVarIO 0

{-| Yield a transaction that returns the number of running threads in the
group.

Note that because this function yields a 'STM' computation, the returned number
is guaranteed to be consistent inside the transaction.
-}
nrOfRunning :: ThreadGroup -> STM Int
nrOfRunning (ThreadGroup numThreadsTV) = readTVar numThreadsTV

-- | Block until all threads in the group have terminated.
--
-- Note that: @wait = 'waitN' 1@.
wait :: ThreadGroup -> IO ()
wait = waitN 1

-- | Block until there are fewer than @N@ running threads in the group.
waitN :: Int -> ThreadGroup -> IO ()
waitN i tg = atomically $ nrOfRunning tg >>= \n -> when (n >= i) retry


--------------------------------------------------------------------------------
-- * Forking threads
--------------------------------------------------------------------------------

-- | Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds
-- the thread to the group.
forkIO :: ThreadGroup -> IO a -> IO (ThreadId, IO (Result a))
forkIO = fork rawForkIO

-- | Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds
-- the thread to the group.
forkOS :: ThreadGroup -> IO a -> IO (ThreadId, IO (Result a))
forkOS = fork Control.Concurrent.forkOS

-- | Same as @Control.Concurrent.Thread.'Thread.forkOn'@ but
-- additionaly adds the thread to the group.
forkOn :: Int -> ThreadGroup -> IO a -> IO (ThreadId, IO (Result a))
forkOn = fork . rawForkOn

-- | Same as @Control.Concurrent.Thread.'Thread.forkIOWithUnmask'@ but
-- additionaly adds the thread to the group.
forkIOWithUnmask
    :: ThreadGroup
    -> ((forall b. IO b -> IO b) -> IO a)
    -> IO (ThreadId, IO (Result a))
forkIOWithUnmask = forkWithUnmask Control.Concurrent.forkIOWithUnmask

-- | Like @Control.Concurrent.Thread.'Thread.forkOnWithUnmask'@ but
-- additionaly adds the thread to the group.
forkOnWithUnmask
    :: Int
    -> ThreadGroup
    -> ((forall b. IO b -> IO b) -> IO a)
    -> IO (ThreadId, IO (Result a))
forkOnWithUnmask = forkWithUnmask . Control.Concurrent.forkOnWithUnmask


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

fork :: (IO () -> IO ThreadId)
     -> ThreadGroup
     -> IO a
     -> IO (ThreadId, IO (Result a))
fork doFork (ThreadGroup numThreadsTV) a = do
  res <- newEmptyMVar
  tid <- mask $ \restore -> do
    atomically $ modifyTVar numThreadsTV (+ 1)
    doFork $ do
      try (restore a) >>= putMVar res
      atomically $ modifyTVar numThreadsTV (subtract 1)
  return (tid, readMVar res)

forkWithUnmask
    :: (((forall b. IO b -> IO b) -> IO ()) -> IO ThreadId)
    -> ThreadGroup
    -> ((forall b. IO b -> IO b) -> IO a)
    -> IO (ThreadId, IO (Result a))
forkWithUnmask doForkWithUnmask = \(ThreadGroup numThreadsTV) f -> do
  res <- newEmptyMVar
  tid <- mask $ \restore -> do
    atomically $ modifyTVar numThreadsTV (+ 1)
    doForkWithUnmask $ \unmask -> do
      try (restore $ f unmask) >>= putMVar res
      atomically $ modifyTVar numThreadsTV (subtract 1)
  return (tid, readMVar res)

-- | Strictly modify the contents of a 'TVar'.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = readTVar tv >>= writeTVar tv .! f

-- | Strict function composition
(.!) :: (b -> c) -> (a -> b) -> (a -> c)
f .! g = \x -> f $! g x
