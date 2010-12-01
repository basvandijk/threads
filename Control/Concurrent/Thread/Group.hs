{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

#if MIN_VERSION_base(4,3,0)
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- For block and unblock
#endif

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
    ( ThreadGroup
    , new
    , nrOfRunning
    , wait

      -- * Forking threads
    , forkIO
    , forkOS
#ifdef __GLASGOW_HASKELL__
    , forkOnIO
#if MIN_VERSION_base(4,3,0)
    , forkIOUnmasked
#endif
#endif
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent     ( forkIO, forkOS )
import Control.Concurrent               ( ThreadId )
import Control.Concurrent.MVar          ( newEmptyMVar, putMVar, readMVar )
import Control.Exception                ( try )
#if MIN_VERSION_base(4,3,0)
import Control.Exception                ( block, unblock )
#endif
import Control.Monad                    ( return, (>>=), when )
import Data.Function                    ( ($) )
import Data.Functor                     ( fmap )
import Data.Eq                          ( Eq )
import Data.Typeable                    ( Typeable )
import Prelude                          ( ($!), Integer, succ, pred )
import System.IO                        ( IO )

#if __GLASGOW_HASKELL__ < 701
import Prelude                          ( fromInteger )
import Control.Monad                    ( (>>), fail )
#endif

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc               ( forkOnIO )
import Data.Int                         ( Int )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode                  ( (≢) )
import Data.Function.Unicode            ( (∘) )

-- from stm:
import Control.Concurrent.STM.TVar      ( TVar, newTVarIO, readTVar, writeTVar )
import Control.Concurrent.STM           ( STM, atomically, retry )

-- from threads:
import Control.Concurrent.Thread        ( Result )

#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO
                                                     , forkOS
#ifdef __GLASGOW_HASKELL__
                                                     , forkOnIO
#if MIN_VERSION_base(4,3,0)
                                                     , forkIOUnmasked
#endif
#endif
                                                     )
#endif

-- from ourselves:
import Mask                             ( mask )


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

* 'wait' blocks as long as the counter is not 0.
-}
newtype ThreadGroup = ThreadGroup (TVar Integer) deriving (Eq, Typeable)

-- | Create an empty group of threads.
new ∷ IO ThreadGroup
new = fmap ThreadGroup $ newTVarIO 0

{-| Yield a transaction that returns the number of running threads in the
group.

Note that because this function yields a 'STM' computation, the returned number
is guaranteed to be consistent inside the transaction.
-}
nrOfRunning ∷ ThreadGroup → STM Integer
nrOfRunning (ThreadGroup numThreadsTV) = readTVar numThreadsTV

-- | Convenience function which blocks until all threads, that were added to the
-- group have terminated.
wait ∷ ThreadGroup → IO ()
wait tg = atomically $ nrOfRunning tg >>= \n → when (n ≢ 0) retry


--------------------------------------------------------------------------------
-- * Forking threads
--------------------------------------------------------------------------------

-- | Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds
-- the thread to the group.
forkIO ∷ ThreadGroup → IO α → IO (ThreadId, IO (Result α))
forkIO = fork Control.Concurrent.forkIO

-- | Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds
-- the thread to the group.
forkOS ∷ ThreadGroup → IO α → IO (ThreadId, IO (Result α))
forkOS = fork Control.Concurrent.forkOS

#ifdef __GLASGOW_HASKELL__
-- | Same as @Control.Concurrent.Thread.'Thread.forkOnIO'@ but
-- additionaly adds the thread to the group. (GHC only)
forkOnIO ∷ Int → ThreadGroup → IO α → IO (ThreadId, IO (Result α))
forkOnIO = fork ∘ GHC.Conc.forkOnIO

#if MIN_VERSION_base(4,3,0)
-- | Same as @Control.Concurrent.Thread.'Thread.forkIOUnmasked'@ but
-- additionaly adds the thread to the group. (GHC only)
forkIOUnmasked ∷ ThreadGroup → IO α → IO (ThreadId, IO (Result α))
forkIOUnmasked (ThreadGroup numThreadsTV) a = do
  res ← newEmptyMVar
  tid ← block $ do
    atomically $ modifyTVar numThreadsTV succ
    Control.Concurrent.forkIO $ do
      try (unblock a) >>= putMVar res
      atomically $ modifyTVar numThreadsTV pred
  return (tid, readMVar res)
#endif
#endif

--------------------------------------------------------------------------------

-- | Internally used function which generalises 'forkIO', 'forkOS' and
-- 'forkOnIO' by parameterizing the function which does the actual forking.
fork ∷ (IO () → IO ThreadId) → ThreadGroup → IO α → IO (ThreadId, IO (Result α))
fork doFork (ThreadGroup numThreadsTV) a = do
  res ← newEmptyMVar
  tid ← mask $ \restore → do
    atomically $ modifyTVar numThreadsTV succ
    doFork $ do
      try (restore a) >>= putMVar res
      atomically $ modifyTVar numThreadsTV pred
  return (tid, readMVar res)

-- | Strictly modify the contents of a 'TVar'.
modifyTVar ∷ TVar α → (α → α) → STM ()
modifyTVar tv f = readTVar tv >>= writeTVar tv ∘! f

-- | Strict function composition
(∘!) ∷ (β → γ) → (α → β) → (α → γ)
f ∘! g = \x → f $! g x


-- The End ---------------------------------------------------------------------
