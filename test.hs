{-# LANGUAGE CPP
           , NoImplicitPrelude
           , UnicodeSyntax
           , DeriveDataTypeable
 #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( ThreadId, threadDelay, throwTo, killThread )
import Control.Exception  ( Exception, fromException
                          , AsyncException(ThreadKilled)
                          , throwIO
                          , unblock, block, blocked
                          )
import Control.Monad      ( return, (>>=), fail, (>>) )
import Data.Bool          ( Bool(False, True), not )
import Data.Eq            ( Eq )
import Data.Either        ( either )
import Data.Function      ( ($), const )
import Data.Functor       ( fmap, (<$>) )
import Data.Int           ( Int )
import Data.IORef         ( newIORef, readIORef, writeIORef )
import Data.Typeable      ( Typeable )
import Prelude            ( fromInteger )
import System.Timeout     ( timeout )
import System.IO          ( IO )
import Text.Show          ( Show )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock as Lock

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )

-- from threads:
import Control.Concurrent.Thread       ( Result )
import Control.Concurrent.Thread.Group ( ThreadGroup )

import qualified Control.Concurrent.Thread       as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup

import Utils ( isJustTrue, justEq, (<$$>) )


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Thread" $
          [ testGroup "forkIO" $
            [ testCase "wait"            $ test_wait            Thread.forkIO
            , testCase "isRunning"       $ test_isRunning       Thread.forkIO
            , testCase "blockedState"    $ test_blockedState    Thread.forkIO
            , testCase "unblockedState"  $ test_unblockedState  Thread.forkIO
            , testCase "sync exception"  $ test_sync_exception  Thread.forkIO
            , testCase "async exception" $ test_async_exception Thread.forkIO
            ]
          , testGroup "forkOS" $
            [ testCase "wait"            $ test_wait            Thread.forkOS
            , testCase "isRunning"       $ test_isRunning       Thread.forkOS
            , testCase "blockedState"    $ test_blockedState    Thread.forkOS
            , testCase "unblockedState"  $ test_unblockedState  Thread.forkOS
            , testCase "sync exception"  $ test_sync_exception  Thread.forkOS
            , testCase "async exception" $ test_async_exception Thread.forkOS
            ]
#ifdef __GLASGOW_HASKELL__
          , testGroup "forkOnIO 0" $
            [ testCase "wait"            $ test_wait            (Thread.forkOnIO 0)
            , testCase "isRunning"       $ test_isRunning       (Thread.forkOnIO 0)
            , testCase "blockedState"    $ test_blockedState    (Thread.forkOnIO 0)
            , testCase "unblockedState"  $ test_unblockedState  (Thread.forkOnIO 0)
            , testCase "sync exception"  $ test_sync_exception  (Thread.forkOnIO 0)
            , testCase "async exception" $ test_async_exception (Thread.forkOnIO 0)
            ]
#endif
          ]
        , testGroup "ThreadGroup" $
          [ testGroup "forkIO" $
            [ testCase "wait"            $ wrapIO test_wait
            , testCase "isRunning"       $ wrapIO test_isRunning
            , testCase "blockedState"    $ wrapIO test_blockedState
            , testCase "unblockedState"  $ wrapIO test_unblockedState
            , testCase "sync exception"  $ wrapIO test_sync_exception
            , testCase "async exception" $ wrapIO test_async_exception

            , testCase "group single wait"      $ test_group_single_wait      ThreadGroup.forkIO
            , testCase "group single isRunning" $ test_group_single_isRunning ThreadGroup.forkIO
            ]
          , testGroup "forkOS" $
            [ testCase "wait"            $ wrapOS test_wait
            , testCase "isRunning"       $ wrapOS test_isRunning
            , testCase "blockedState"    $ wrapOS test_blockedState
            , testCase "unblockedState"  $ wrapOS test_unblockedState
            , testCase "sync exception"  $ wrapOS test_sync_exception
            , testCase "async exception" $ wrapOS test_async_exception

            , testCase "group single wait"      $ test_group_single_wait      ThreadGroup.forkOS
            , testCase "group single isRunning" $ test_group_single_isRunning ThreadGroup.forkOS
            ]
#ifdef __GLASGOW_HASKELL__
          , testGroup "forkOnIO 0" $
            [ testCase "wait"            $ wrapOnIO_0 test_wait
            , testCase "isRunning"       $ wrapOnIO_0 test_isRunning
            , testCase "blockedState"    $ wrapOnIO_0 test_blockedState
            , testCase "unblockedState"  $ wrapOnIO_0 test_unblockedState
            , testCase "sync exception"  $ wrapOnIO_0 test_sync_exception
            , testCase "async exception" $ wrapOnIO_0 test_async_exception

            , testCase "group single wait"      $ test_group_single_wait      (ThreadGroup.forkOnIO 0)
            , testCase "group single isRunning" $ test_group_single_isRunning (ThreadGroup.forkOnIO 0)
            ]
#endif
          ]
        ]

-- Exactly 1 moment. Currently equal to 0.005 seconds.
a_moment ∷ Int
a_moment = 5000


--------------------------------------------------------------------------------
-- General properties
--------------------------------------------------------------------------------

type Fork α = IO α → IO (ThreadId, Result α)

test_wait ∷ Fork () → Assertion
test_wait fork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  r ← newIORef False
  (_, result) ← fork $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← Thread.wait result
  readIORef r

test_isRunning ∷ Fork () → Assertion
test_isRunning fork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  l ← Lock.newAcquired
  (_, result) ← fork $ Lock.acquire l
  r ← Thread.isRunning result
  Lock.release l
  return r

test_blockedState ∷ Fork Bool → Assertion
test_blockedState fork = do (_, result) ← block $ fork $ blocked
                            Thread.unsafeWait result >>= assert

test_unblockedState ∷ Fork Bool → Assertion
test_unblockedState fork = do (_, result) ← unblock $ fork $ not <$> blocked
                              Thread.unsafeWait result >>= assert

test_sync_exception ∷ Fork () → Assertion
test_sync_exception fork = assert $ do
  (_, result) ← fork $ throwIO MyException
  waitForException MyException result

waitForException ∷ (Exception e, Eq e) ⇒ e → Result α → IO Bool
waitForException e result = Thread.wait result <$$>
                              either (justEq e ∘ fromException)
                                     (const False)

test_async_exception ∷ Fork () → Assertion
test_async_exception fork = assert $ do
  l ← Lock.newAcquired
  (tid, result) ← fork $ Lock.acquire l
  throwTo tid MyException
  waitForException MyException result

data MyException = MyException deriving (Show, Eq, Typeable)
instance Exception MyException

test_killThread ∷ Fork () → Assertion
test_killThread fork = assert $ do
  l ← Lock.newAcquired
  (tid, result) ← fork $ Lock.acquire l
  killThread tid
  waitForException ThreadKilled result


--------------------------------------------------------------------------------
-- ThreadGroup
--------------------------------------------------------------------------------

wrapIO ∷ (Fork α → IO β) → IO β
wrapIO = wrap ThreadGroup.forkIO

wrapOS ∷ (Fork α → IO β) → IO β
wrapOS = wrap ThreadGroup.forkOS

#ifdef __GLASGOW_HASKELL__
wrapOnIO_0 ∷ (Fork α → IO β) → IO β
wrapOnIO_0 = wrap $ ThreadGroup.forkOnIO 0
#endif

wrap ∷ (ThreadGroup → Fork α) → (Fork α → IO β) → IO β
wrap doFork test = ThreadGroup.new >>= test ∘ doFork

test_group_single_wait ∷ (ThreadGroup → Fork ()) → Assertion
test_group_single_wait doFork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  tg ← ThreadGroup.new
  r ← newIORef False
  _ ← doFork tg $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← ThreadGroup.wait tg
  readIORef r

test_group_single_isRunning ∷ (ThreadGroup → Fork ()) → Assertion
test_group_single_isRunning doFork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  tg ← ThreadGroup.new
  l ← Lock.newAcquired
  _ ← doFork tg $ Lock.acquire l
  r ← ThreadGroup.isAnyRunning tg
  Lock.release l
  return r


-- The End ---------------------------------------------------------------------
