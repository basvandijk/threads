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

      -- * Waiting & Status
    , wait
    , isAnyRunning
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative     ( (<*>) )
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, putMVar
                               )
import Control.Exception       ( blocked, block, unblock, try )
import Control.Monad           ( (>>=), (>>), fail )
import Data.Bool               ( Bool(..) )
import Data.Function           ( ($) )
import Data.Functor            ( (<$>), fmap )
import Data.Typeable           ( Typeable )
import System.IO               ( IO )
import qualified Control.Concurrent as C ( ThreadId, forkIO, forkOS )
import Prelude                 ( Integer, fromInteger, (+), (-), ($!) )

-- from base-unicode-symbols:
import Data.Eq.Unicode         ( (≡) )
import Data.Function.Unicode   ( (∘) )

-- from concurrent-extra:
import Control.Concurrent.Lock ( Lock )
import qualified Control.Concurrent.Lock as Lock ( new
                                                 , acquire, release
                                                 , wait, locked
                                                 )
-- from threads:
import Control.Concurrent.Thread.Internal ( ThreadId( ThreadId ) )

import Utils ( whenThen )

#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO, forkOS )
#endif

-------------------------------------------------------------------------------
-- Thread groups
-------------------------------------------------------------------------------

data ThreadGroup = ThreadGroup (MVar Integer) Lock deriving Typeable

-- | Create a new empty group.
new ∷ IO ThreadGroup
new = ThreadGroup <$> newMVar 0 <*> Lock.new

-- | Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds
-- the thread to the group.
forkIO ∷ ThreadGroup → IO α → IO (ThreadId α)
forkIO = fork C.forkIO

-- | Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds
-- the thread to the group.
forkOS ∷ ThreadGroup → IO α → IO (ThreadId α)
forkOS = fork C.forkOS

fork ∷ (IO () → IO C.ThreadId) → ThreadGroup → IO α → IO (ThreadId α)
fork doFork (ThreadGroup mc l) a = do
  res ← newEmptyMVar
  b ← blocked
  block $ do
    increment
    fmap (ThreadId res) $ doFork $ do
      try (if b then a else unblock a) >>= putMVar res
      decrement
  where
    increment = do numThreads ← takeMVar mc
                   whenThen (numThreads ≡ 0) (Lock.acquire l)
                     (putMVar mc $! numThreads + 1)

    decrement = do numThreads ← takeMVar mc
                   whenThen (numThreads ≡ 1) (Lock.release l)
                     (putMVar mc $! numThreads - 1)

lock ∷ ThreadGroup → Lock
lock (ThreadGroup _ l) = l

-- | Block until all threads, that were added to the group before calling this
-- function, have terminated.
wait ∷ ThreadGroup → IO ()
wait = Lock.wait ∘ lock

{-|
Returns 'True' if any thread in the group is running and returns 'False'
otherwise.

Notice that this observation is only a snapshot of a group's state. By the time
a program reacts on its result it may already be out of date.
-}
isAnyRunning ∷ ThreadGroup → IO Bool
isAnyRunning = Lock.locked ∘ lock


-- The End ---------------------------------------------------------------------
