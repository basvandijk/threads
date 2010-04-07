{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module Main where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( threadDelay )
import Control.Exception  ( Exception, fromException
                          , AsyncException(ThreadKilled)
                          , throwIO
                          , unblock, block, blocked
                          )
import Control.Monad      ( return, (>>=), fail, (>>) )
import Data.Bool          ( Bool(False, True), not )
import Data.Eq            ( Eq )
import Data.Either        ( either )
import Data.Function      ( ($), id, const )
import Data.Functor       ( fmap  )
import Data.IORef         ( newIORef, readIORef, writeIORef )
import Data.Maybe         ( maybe )
import Data.Typeable      ( Typeable )
import Prelude            ( fromInteger )
import System.Timeout     ( timeout )
import System.IO          ( IO )
import Text.Show          ( Show )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )
import Data.Eq.Unicode       ( (≡) )
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock   as Lock

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )

-- from threads:
import qualified Control.Concurrent.Thread as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup

import TestUtils ( a_moment )
import Utils     ( (<$$>) )


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Thread" $
          [ testCase "wait"            $ test_wait            Thread.forkIO
          , testCase "isRunning"       $ test_isRunning       Thread.forkIO
          , testCase "blockedState"    $ test_blockedState    Thread.forkIO
          , testCase "unblockedState"  $ test_unblockedState  Thread.forkIO
          , testCase "sync exception"  $ test_sync_exception  Thread.forkIO
          , testCase "async exception" $ test_async_exception Thread.forkIO
          ]
        , testGroup "ThreadGroup" $
          [ testCase "wait"            $ wrap test_wait
          , testCase "isRunning"       $ wrap test_isRunning
          , testCase "blockedState"    $ wrap test_blockedState
          , testCase "unblockedState"  $ wrap test_unblockedState
          , testCase "sync exception"  $ wrap test_sync_exception
          , testCase "async exception" $ wrap test_async_exception

          , testCase "group single wait"      test_group_single_wait
          , testCase "group single isRunning" test_group_single_isRunning
          ]
        ]


-------------------------------------------------------------------------------
-- General properties
-------------------------------------------------------------------------------

type Fork α = IO α → IO (Thread.ThreadId α)

test_wait ∷ Fork () → Assertion
test_wait fork = assert
               $ fmap (maybe False id)
               $ timeout (10 ⋅ a_moment) $ do
  r ← newIORef False
  tid ← fork $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← Thread.wait tid
  readIORef r

test_isRunning ∷ Fork () → Assertion
test_isRunning fork = assert
                    $ fmap (maybe False id)
                    $ timeout (10 ⋅ a_moment) $ do
  l ← Lock.newAcquired
  tid ← fork $ Lock.acquire l
  r ← Thread.isRunning tid
  Lock.release l
  return r

test_blockedState ∷ Fork Bool → Assertion
test_blockedState fork = (block $ fork $ blocked) >>=
                         Thread.unsafeWait >>= assert

test_unblockedState ∷ Fork Bool → Assertion
test_unblockedState fork = (unblock $ fork $ fmap not $ blocked) >>=
                           Thread.unsafeWait >>= assert

test_sync_exception ∷ Fork () → Assertion
test_sync_exception fork = assert $
  fork (throwIO MyException) >>= waitForException MyException

waitForException ∷ (Exception e, Eq e) ⇒ e → Thread.ThreadId α → IO Bool
waitForException e tid = Thread.wait tid <$$>
                           either (maybe False (≡ e) ∘ fromException)
                                  (const False)

test_async_exception ∷ Fork () → Assertion
test_async_exception fork = assert $ do
  l ← Lock.newAcquired
  tid ← fork $ Lock.acquire l
  Thread.throwTo tid MyException
  waitForException MyException tid

data MyException = MyException deriving (Show, Eq, Typeable)
instance Exception MyException

test_killThread ∷ Fork () → Assertion
test_killThread fork = assert $ do
  l ← Lock.newAcquired
  tid ← fork $ Lock.acquire l
  Thread.killThread tid
  waitForException ThreadKilled tid


-------------------------------------------------------------------------------
-- ThreadGroup
-------------------------------------------------------------------------------

wrap ∷ (Fork α → IO β) → IO β
wrap test = ThreadGroup.new >>= test ∘ ThreadGroup.forkIO

test_group_single_wait ∷ Assertion
test_group_single_wait = assert
                       $ fmap (maybe False id)
                       $ timeout (10 ⋅ a_moment) $ do
  tg ← ThreadGroup.new
  r ← newIORef False
  _ ← ThreadGroup.forkIO tg $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← ThreadGroup.wait tg
  readIORef r


test_group_single_isRunning ∷ Assertion
test_group_single_isRunning = assert
                            $ fmap (maybe False id)
                            $ timeout (10 ⋅ a_moment) $ do
  tg ← ThreadGroup.new
  l ← Lock.newAcquired
  _ ← ThreadGroup.forkIO tg $ Lock.acquire l
  r ← ThreadGroup.isAnyRunning tg
  Lock.release l
  return r


-- The End ---------------------------------------------------------------------
