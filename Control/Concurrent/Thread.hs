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
-- This module exports equivalently named functions from
-- @Control.Concurrent@. Avoid ambiguities by importing one or both
-- qualified. We suggest importing this module like:
--
-- @
-- import qualified Control.Concurrent.Thread as Thread ( ... )
-- @
--
-------------------------------------------------------------------------------

module Control.Concurrent.Thread
  ( ThreadId
  , threadId

    -- * Forking threads
  , forkIO
  , forkOS

    -- * Waiting on threads
  , wait
  , wait_
  , unsafeWait
  , unsafeWait_

    -- * Querying thread status
  , status
  , isRunning

    -- * Convenience functions
  , throwTo
  , killThread
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent as C ( ThreadId, forkIO, forkOS, throwTo )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, readMVar )
import Control.Exception  ( Exception, SomeException
                          , AsyncException(ThreadKilled)
                          , blocked, block, unblock, try
                          )
#ifdef __HADDOCK__
import qualified Control.Concurrent as C ( killThread )
#endif
import Control.Monad      ( return, (>>=), (>>), fail )
import Data.Bool          ( Bool(..) )
import Data.Either        ( Either(..), either )
import Data.Function      ( ($), const )
import Data.Functor       ( fmap )
import Data.Maybe         ( Maybe(..), isNothing )
import System.IO          ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import Utils ( void, throwInner, tryRead )

import Control.Concurrent.Thread.Internal ( ThreadId(ThreadId)
                                          , result, threadId
                                          )


-------------------------------------------------------------------------------
-- * Forking threads
-------------------------------------------------------------------------------

{-|
Sparks off a new thread to run the given 'IO' computation and returns the
'ThreadId' of the newly created thread.

The new thread will be a lightweight thread; if you want to use a foreign
library that uses thread-local storage, use 'forkOS' instead.

GHC note: the new thread inherits the blocked state of the parent (see
'Control.Exception.block').
-}
forkIO ∷ IO α → IO (ThreadId α)
forkIO = fork C.forkIO

{-|
Like 'forkIO', this sparks off a new thread to run the given 'IO' computation
and returns the 'ThreadId' of the newly created thread.

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
forkOS ∷ IO α → IO (ThreadId α)
forkOS = fork C.forkOS

{-|
Internally used function which generalises 'forkIO' and 'forkOS'. Parametrised
by the function which does the actual forking.
-}
fork ∷ (IO () → IO C.ThreadId) → IO α → IO (ThreadId α)
fork doFork a = do
  res ← newEmptyMVar
  b ← blocked
  fmap (ThreadId res) $ block $ doFork $ try (if b then a else unblock a) >>=
                                         putMVar res


-------------------------------------------------------------------------------
-- * Waiting on threads
-------------------------------------------------------------------------------

{-|
Block until the given thread is terminated.

* Returns @'Right' x@ if the thread terminated normally and returned @x@.

* Returns @'Left' e@ if some exception @e@ was thrown in the thread and wasn't
caught.
-}
wait ∷ ThreadId α → IO (Either SomeException α)
wait = readMVar ∘ result

-- | Like 'wait' but will ignore the value returned by the thread.
wait_ ∷ ThreadId α → IO ()
wait_ = void ∘ wait

-- | Like 'wait' but will either rethrow the exception that was thrown in the
-- thread or return the value that was returned by the thread.
unsafeWait ∷ ThreadId α → IO α
unsafeWait tid = wait tid >>= either throwInner return

-- | Like 'unsafeWait' in that it will rethrow the exception that was thrown in
-- the thread but it will ignore the value returned by the thread.
unsafeWait_ ∷ ThreadId α → IO ()
unsafeWait_ tid = wait tid >>= either throwInner (const $ return ())


-------------------------------------------------------------------------------
-- * Quering thread status
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
status ∷ ThreadId α → IO (Maybe (Either SomeException α))
status = tryRead ∘ result

{-|
Returns 'True' if the thread is currently running and 'False' otherwise.

Notice that this observation is only a snapshot of a thread's state. By the time
a program reacts on its result it may already be out of date.
-}
isRunning ∷ ThreadId α → IO Bool
isRunning = fmap isNothing ∘ status


-------------------------------------------------------------------------------
-- * Convenience functions
-------------------------------------------------------------------------------

{-|
'throwTo' raises an arbitrary exception in the target thread (GHC only).

'throwTo' does not return until the exception has been raised in the target
thread. The calling thread can thus be certain that the target thread has
received the exception. This is a useful property to know when dealing with race
conditions: eg. if there are two threads that can kill each other, it is
guaranteed that only one of the threads will get to kill the other.

If the target thread is currently making a foreign call, then the exception will
not be raised (and hence 'throwTo' will not return) until the call has
completed. This is the case regardless of whether the call is inside a 'block'
or not.

Important note: the behaviour of 'throwTo' differs from that described in the
paper \"Asynchronous exceptions in Haskell\"
(<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>). In the paper,
'throwTo' is non-blocking; but the library implementation adopts a more
synchronous design in which 'throwTo' does not return until the exception is
received by the target thread. The trade-off is discussed in Section 9 of the
paper. Like any blocking operation, 'throwTo' is therefore interruptible (see
Section 5.3 of the paper).

There is currently no guarantee that the exception delivered by 'throwTo' will
be delivered at the first possible opportunity. In particular, a thread may
'unblock' and then re-'block' exceptions without receiving a pending
'throwTo'. This is arguably undesirable behaviour.
-}
throwTo ∷ Exception e ⇒ ThreadId α → e → IO ()
throwTo = C.throwTo ∘ threadId

{-|
'killThread' terminates the given thread (GHC only). Any work already done by
the thread isn't lost: the computation is suspended until required by another
thread. The memory used by the thread will be garbage collected if it isn't
referenced from anywhere. The 'killThread' function is defined in terms of
'throwTo'.

Note that this function is different than @Control.Concurrent.'C.killThread'@ in
that it blocks until the target thread is terminated:

@killThread tid = 'throwTo' tid 'ThreadKilled' '>>' 'wait_' tid@
-}
killThread ∷ ThreadId α → IO ()
killThread tid = throwTo tid ThreadKilled >> wait_ tid


-- The End ---------------------------------------------------------------------
