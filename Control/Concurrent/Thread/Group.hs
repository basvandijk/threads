{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread.Group
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- This module extends @Control.Concurrent.Thread@ with the ability to wait for
-- a group of threads to terminate.
--
-- This module exports equivalently named functions from @Control.Concurrent@
-- and @Control.Concurrent.Thread@. Avoid ambiguities by importing one or both
-- qualified. We suggest importing this module like:
--
-- @
-- import Control.Concurrent.Thread.Group ( ThreadGroup )
-- import qualified Control.Concurrent.Thread.Group as ThreadGroup ( ... )
-- @
--
---------------------------------------------------------------------------------

module Control.Concurrent.Thread.Group
    ( -- * Creating
      ThreadGroup
    , new

      -- * Forking threads
    , forkIO
    , forkOS
#ifdef __GLASGOW_HASKELL__
    , forkOnIO
#endif

      -- * Waiting & Status
    , wait
    , isAnyRunning
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Exception       ( blocked, block, unblock, try )
import Control.Monad           ( (>>=), (>>), fail, when, liftM2 )
import Data.Bool               ( Bool(..) )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Int                ( Int )
import Data.Typeable           ( Typeable )
import System.IO               ( IO )
import qualified Control.Concurrent as C ( ThreadId, forkIO, forkOS )
import Prelude                 ( Integer, fromInteger, (+), (-), ($!) )

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc      ( forkOnIO )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )

-- from stm:
import Control.Concurrent.STM.TVar  ( TVar, newTVar, writeTVar, readTVar )
import Control.Concurrent.STM.TMVar ( newEmptyTMVarIO, putTMVar )
import Control.Concurrent.STM       ( atomically )

-- from concurrent-extra:
import Control.Concurrent.STM.Lock ( Lock )
import qualified Control.Concurrent.STM.Lock as Lock ( new
                                                     , acquire, release
                                                     , wait, locked
                                                     )

-- from threads:
import Control.Concurrent.Thread.Internal ( ThreadId( ThreadId ) )

#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO
                                                     , forkOS
#ifdef __GLASGOW_HASKELL__
                                                     , forkOnIO
#endif
                                                     )
#endif


-------------------------------------------------------------------------------
-- Thread groups
-------------------------------------------------------------------------------

data ThreadGroup = ThreadGroup (TVar Integer) Lock deriving Typeable

-- | Create a new empty group of threads.
new ∷ IO ThreadGroup
new = atomically $ liftM2 ThreadGroup (newTVar 0) (Lock.new)

-- | Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds
-- the thread to the group.
forkIO ∷ ThreadGroup → IO α → IO (ThreadId α)
forkIO = fork C.forkIO

-- | Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds
-- the thread to the group.
forkOS ∷ ThreadGroup → IO α → IO (ThreadId α)
forkOS = fork C.forkOS

#ifdef __GLASGOW_HASKELL__
-- | Same as @Control.Concurrent.Thread.'Thread.forkOnIO'@ but
-- additionaly adds the thread to the group. (GHC only)
forkOnIO ∷ Int → ThreadGroup → IO α → IO (ThreadId α)
forkOnIO = fork ∘ GHC.Conc.forkOnIO
#endif

fork ∷ (IO () → IO C.ThreadId) → ThreadGroup → IO α → IO (ThreadId α)
fork doFork (ThreadGroup mc l) a = do
  res ← newEmptyTMVarIO
  b ← blocked
  block $ do
    atomically increment
    fmap (ThreadId res) $ doFork $ do
      r ← try (if b then a else unblock a)
      atomically $ putTMVar res r >> decrement
  where
    increment = do numThreads ← readTVar mc
                   when (numThreads ≡ 0) $ Lock.acquire l
                   writeTVar mc $! numThreads + 1

    decrement = do numThreads ← readTVar mc
                   when (numThreads ≡ 1) $ Lock.release l
                   writeTVar mc $! numThreads - 1

lock ∷ ThreadGroup → Lock
lock (ThreadGroup _ l) = l

-- | Block until all threads, that were added to the group have terminated.
wait ∷ ThreadGroup → IO ()
wait = atomically ∘ Lock.wait ∘ lock

{-|
Returns 'True' if any thread in the group is running and returns 'False'
otherwise.

Notice that this observation is only a snapshot of a group's state. By the time
a program reacts on its result it may already be out of date.
-}
isAnyRunning ∷ ThreadGroup → IO Bool
isAnyRunning = atomically ∘ Lock.locked ∘ lock


-- The End ---------------------------------------------------------------------
