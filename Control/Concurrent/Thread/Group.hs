{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

--------------------------------------------------------------------------------
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
    ( -- * Groups of threads
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


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent     ( forkIO, forkOS )
import Control.Concurrent               ( ThreadId )
import Control.Exception                ( blocked, block, unblock, try )
import Control.Monad                    ( return, (>>=), (>>), fail, when )
import Data.Bool                        ( Bool(..) )
import Data.Function                    ( ($) )
import Data.Functor                     ( fmap )
import Data.Typeable                    ( Typeable )
import Prelude                          ( Integer, fromInteger, succ, pred )
import System.IO                        ( IO )

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc               ( forkOnIO )
import Data.Int                         ( Int )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode                  ( (≢) )
import Data.Function.Unicode            ( (∘) )

-- from stm:
import Control.Concurrent.STM.TVar      ( TVar, newTVar, readTVar )
import Control.Concurrent.STM.TMVar     ( newEmptyTMVarIO, putTMVar )
import Control.Concurrent.STM           ( atomically, retry )

-- from threads:
import Control.Concurrent.Thread.Result ( Result(Result) )

import Utils ( modifyTVar )

#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO
                                                     , forkOS
#ifdef __GLASGOW_HASKELL__
                                                     , forkOnIO
#endif
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

* 'wait' blocks as long as the counter is not 0.
-}
newtype ThreadGroup = ThreadGroup (TVar Integer) deriving Typeable

-- | Create an empty group of threads.
new ∷ IO ThreadGroup
new = atomically $ fmap ThreadGroup $ newTVar 0


--------------------------------------------------------------------------------
-- * Forking threads
--------------------------------------------------------------------------------

-- | Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds
-- the thread to the group.
forkIO ∷ ThreadGroup → IO α → IO (ThreadId, Result α)
forkIO = fork Control.Concurrent.forkIO

-- | Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds
-- the thread to the group.
forkOS ∷ ThreadGroup → IO α → IO (ThreadId, Result α)
forkOS = fork Control.Concurrent.forkOS

#ifdef __GLASGOW_HASKELL__
-- | Same as @Control.Concurrent.Thread.'Thread.forkOnIO'@ but
-- additionaly adds the thread to the group. (GHC only)
forkOnIO ∷ Int → ThreadGroup → IO α → IO (ThreadId, Result α)
forkOnIO = fork ∘ GHC.Conc.forkOnIO
#endif

--------------------------------------------------------------------------------

-- | Internally used function which generalises 'forkIO', 'forkOS' and
-- 'forkOnIO' by parameterizing the function which does the actual forking.
fork ∷ (IO () → IO ThreadId) → ThreadGroup → IO α → IO (ThreadId, Result α)
fork doFork (ThreadGroup numThreadsTV) a = do
  res ← newEmptyTMVarIO
  parentIsBlocked ← blocked
  tid ← block $ do
    atomically $ modifyTVar numThreadsTV succ
    doFork $ do
      r ← try $ if parentIsBlocked then a else unblock a
      atomically $ modifyTVar numThreadsTV pred >> putTMVar res r
  return (tid, Result res)


--------------------------------------------------------------------------------
-- * Waiting & Status
--------------------------------------------------------------------------------

-- | Block until all threads, that were added to the group have terminated.
wait ∷ ThreadGroup → IO ()
wait (ThreadGroup numThreadsTV) = atomically $ do
                                    numThreads ← readTVar numThreadsTV
                                    when (numThreads ≢ 0) retry

{-|
Returns 'True' if any thread in the group is running and returns 'False'
otherwise.

Notice that this observation is only a snapshot of a group's state. By the time
a program reacts on its result it may already be out of date.
-}
isAnyRunning ∷ ThreadGroup → IO Bool
isAnyRunning (ThreadGroup numThreadsTV) = atomically $
                                            fmap (≢ 0) $ readTVar numThreadsTV


-- The End ---------------------------------------------------------------------
