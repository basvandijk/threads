{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread
-- Copyright  : (c) 2010 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Standard threads extended with the ability to wait for their termination.
--
-- This module exports equivalently named functions from @Control.Concurrent@
-- (and @GHC.Conc@). Avoid ambiguities by importing this module qualified. May
-- we suggest:
--
-- @
-- import qualified Control.Concurrent.Thread as Thread ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.Thread
  ( -- * The result of a thread
    Result

    -- * Forking threads
  , forkIO
  , forkOS
#ifdef __GLASGOW_HASKELL__
  , forkOnIO
#endif

    -- * Waiting for results
  , wait
  , wait_
  , unsafeWait
  , unsafeWait_

    -- * Querying results
  , status
  , isRunning
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent as C ( forkIO, forkOS )
import Control.Concurrent ( ThreadId )
import Control.Exception  ( SomeException , blocked, block, unblock, try )
import Control.Monad      ( return, (>>=), fail )
import Data.Bool          ( Bool(..) )
import Data.Either        ( Either(..), either )
import Data.Function      ( ($), const )
import Data.Functor       ( fmap )
import Data.Maybe         ( Maybe(..), isNothing )
import System.IO          ( IO )

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc ( forkOnIO )
import Data.Int           ( Int )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from stm:
import Control.Concurrent.STM.TMVar ( newEmptyTMVarIO, putTMVar, readTMVar )
import Control.Concurrent.STM       ( atomically )

-- from threads:
import Control.Concurrent.Thread.Result ( Result(Result), unResult )

import Utils ( void, throwInner, tryReadTMVar )


-------------------------------------------------------------------------------
-- * Forking threads
-------------------------------------------------------------------------------

{-|
Sparks off a new thread to run the given 'IO' computation and returns the
'ThreadId' of the newly created thread paired with the 'Result' of the thread
which can be @'wait'ed@ upon.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.

GHC note: the new thread inherits the blocked state of the parent (see
'Control.Exception.block').
-}
forkIO ∷ IO α → IO (ThreadId, Result α)
forkIO = fork C.forkIO

{-|
Like 'forkIO', this sparks off a new thread to run the given 'IO' computation
and returns the 'ThreadId' of the newly created thread paired with the 'Result'
of the thread which can be @'wait'ed@ upon.

Unlike 'forkIO', 'forkOS' creates a /bound/ thread, which is necessary if you
need to call foreign (non-Haskell) libraries that make use of thread-local
state, such as OpenGL (see 'Control.Concurrent').

Using 'forkOS' instead of 'forkIO' makes no difference at all to the scheduling
behaviour of the Haskell runtime system. It is a common misconception that you
need to use 'forkOS' instead of 'forkIO' to avoid blocking all the Haskell
threads when making a foreign call; this isn't the case. To allow foreign calls
to be made without blocking all the Haskell threads (with GHC), it is only
necessary to use the @-threaded@ option when linking your program, and to make
sure the foreign import is not marked @unsafe@.
-}
forkOS ∷ IO α → IO (ThreadId, Result α)
forkOS = fork C.forkOS

#ifdef __GLASGOW_HASKELL__
{-|
Like 'forkIO', but lets you specify on which CPU the thread is
created.  Unlike a 'forkIO' thread, a thread created by 'forkOnIO'
will stay on the same CPU for its entire lifetime ('forkIO' threads
can migrate between CPUs according to the scheduling policy).
'forkOnIO' is useful for overriding the scheduling policy when you
know in advance how best to distribute the threads.

The 'Int' argument specifies the CPU number; it is interpreted modulo
'numCapabilities' (note that it actually specifies a capability number
rather than a CPU number, but to a first approximation the two are
equivalent).
-}
forkOnIO ∷ Int → IO α → IO (ThreadId, Result α)
forkOnIO = fork ∘ GHC.Conc.forkOnIO
#endif

-- | Internally used function which generalises 'forkIO', 'forkOS' and
-- 'forkOnIO'. Parametrised by the function which does the actual forking.
fork ∷ (IO () → IO ThreadId) → (IO α → IO (ThreadId, Result α))
fork doFork = \a → do
  res ← newEmptyTMVarIO
  parentIsBlocked ← blocked
  tid ← block $ doFork $
    try (if parentIsBlocked then a else unblock a) >>=
      atomically ∘ putTMVar res
  return (tid, Result res)


-------------------------------------------------------------------------------
-- * Waiting for results
-------------------------------------------------------------------------------

{-|
Block until the thread, to which the given 'Result' belongs, is terminated.

* Returns @'Right' x@ if the thread terminated normally and returned @x@.

* Returns @'Left' e@ if some exception @e@ was thrown in the thread and wasn't
caught.
-}
wait ∷ Result α → IO (Either SomeException α)
wait = atomically ∘ readTMVar ∘ unResult

-- | Like 'wait' but will ignore the value returned by the thread.
wait_ ∷ Result α → IO ()
wait_ = void ∘ wait

-- | Like 'wait' but will either rethrow the exception that was thrown in the
-- thread or return the value that was returned by the thread.
unsafeWait ∷ Result α → IO α
unsafeWait result = wait result >>= either throwInner return

-- | Like 'unsafeWait' in that it will rethrow the exception that was thrown in
-- the thread but it will ignore the value returned by the thread.
unsafeWait_ ∷ Result α → IO ()
unsafeWait_ result = wait result >>= either throwInner (const $ return ())


-------------------------------------------------------------------------------
-- * Quering results
-------------------------------------------------------------------------------

{-|
A non-blocking 'wait'.

* Returns 'Nothing' if the thread is still running.

* Returns @'Just' ('Right' x)@ if the thread terminated normally and returned @x@.

* Returns @'Just' ('Left' e)@ if some exception @e@ was thrown in the thread and
wasn't caught.

Notice that this observation is only a snapshot of a thread's state. By the time
a program reacts on its result it may already be out of date.
-}
status ∷ Result α → IO (Maybe (Either SomeException α))
status = atomically ∘ tryReadTMVar ∘ unResult

{-|
If the thread, to which the given 'Result' belongs, is currently running return
'True' and return 'False' otherwise.

Notice that this observation is only a snapshot of a thread's state. By the time
a program reacts on its result it may already be out of date.
-}
isRunning ∷ Result α → IO Bool
isRunning = fmap isNothing ∘ status


-- The End ---------------------------------------------------------------------
