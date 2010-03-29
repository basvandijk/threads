{-# LANGUAGE CPP
           , DeriveDataTypeable
           , NoImplicitPrelude
           , UnicodeSyntax
           , ScopedTypeVariables
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
-- This module provides equivalently named functions from @Control.Concurrent@
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

      -- * Manually adding and removing
    , add
    , remove
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, newMVar, newEmptyMVar
                               , takeMVar, putMVar
                               , readMVar
                               )
import Control.Exception       ( try, block, catch, SomeException )
import Control.Monad           ( (>>=), (>>), return, fail, mapM_)
import Data.Bool               ( Bool(..) )
import Data.Function           ( ($) )
import Data.Functor            ( (<$>) )
import Data.Typeable           ( Typeable )
import System.IO               ( IO )
import Control.Concurrent      ( myThreadId )
import qualified Control.Concurrent as C ( ThreadId, forkIO, forkOS )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import Control.Concurrent.Thread.Internal ( ThreadId( ThreadId ), threadId )
import Control.Concurrent.Thread ( wait_, isRunning )

import Utils ( (∘!)
             , void
             , anyM
             , deleteFirstWhich'
             , blockedApply
             , purelyModifyMVar
             )

#ifdef __HADDOCK__
import qualified Control.Concurrent.Thread as Thread ( forkIO, forkOS )
#endif


-------------------------------------------------------------------------------
-- Thread groups
-------------------------------------------------------------------------------

{-|
A group of threads that are executing or have executed a computation of type
@'IO' &#x3B1;@.

(Note that terminated threads will be removed from the group automatically as
soon as possible so they don't leak space.)
-}
newtype ThreadGroup α = ThreadGroup (MVar [ThreadId α]) deriving Typeable

-- | Create a new empty group.
new ∷ IO (ThreadGroup α)
new = ThreadGroup <$> newMVar []

{-|
Same as @Control.Concurrent.Thread.'Thread.forkIO'@ but additionaly adds the
thread to the group.

The behaviour of this function is equivalent to the following but more
efficient:

@
forkIO g m = block $ do tid <- Control.Concurrent.Thread.'Thread.forkIO' m
                        'add' g tid
                        return tid
@
-}
forkIO ∷ ThreadGroup α → IO α → IO (ThreadId α)
forkIO = fork C.forkIO

{-|
Same as @Control.Concurrent.Thread.'Thread.forkOS'@ but additionaly adds the
thread to the group.

The behaviour of this function is equivalent to the following but more
efficient:

@
forkOS g m = block $ do tid <- Control.Concurrent.Thread.'Thread.forkOS' m
                        'add' g tid
                        return tid
@
-}
forkOS ∷ ThreadGroup α → IO α → IO (ThreadId α)
forkOS = fork C.forkOS

fork ∷ (IO () → IO C.ThreadId) → ThreadGroup α → IO α → IO (ThreadId α)
fork doFork (ThreadGroup mv) a = do
  res ← newEmptyMVar
  blockedApply a $ \a' → do
    tids ← takeMVar mv

    nativeTid ← doFork $ do
      r ← try a'
      let final = putMVar res r
      deleteMyTid `catch` \(_ ∷ SomeException) → final
      final

    let tid = ThreadId res nativeTid
    putMVar mv $ tid:tids
    return tid
  where
    deleteMyTid = do
      nativeTid ← myThreadId
      takeMVar mv >>= putMVar mv ∘! deleteFirstWhich' ((nativeTid ≡) ∘ threadId)

-- | Block until all threads in the group have terminated.
wait ∷ ThreadGroup α → IO ()
wait (ThreadGroup mv) = readMVar mv >>= mapM_ wait_

{-|
Returns 'True' if any thread in the group is running and returns 'False'
otherwise.

Notice that this observation is only a snapshot of a group's state. By the time
a program reacts on its result it may already be out of date.
-}
isAnyRunning ∷ ThreadGroup α → IO Bool
isAnyRunning (ThreadGroup mv) = readMVar mv >>= anyM isRunning

{-|
If for some reason you can only use @Control.Concurrent.Thread.'Thread.forkIO'@
or @Control.Concurrent.Thread.'Thread.forkOS'@ instead of the 'forkIO' or
'forkOS' from this module, you can use this function to add the resulting thread
to the group.

Note however that it is more efficient to use the forks in this module directly!
-}
add ∷ ThreadGroup α → ThreadId α → IO ()
add (ThreadGroup mv) tid = block $ do
  purelyModifyMVar mv (tid:)
  void $ C.forkIO $ do wait_ tid
                       purelyModifyMVar mv $ deleteFirstWhich' (≡ tid)

-- | Remove a thread from the group. If the thread was not in the group, the
-- group remains unchanged.
remove ∷ ThreadGroup α → ThreadId α → IO ()
remove (ThreadGroup mv) = purelyModifyMVar mv ∘ deleteFirstWhich' ∘ (≡)


-- The End ---------------------------------------------------------------------
