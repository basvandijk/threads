{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( threadDelay )
import Control.Exception  ( unblock, block, blocked )
import Control.Monad      ( return, (>>=), fail, (>>) )
import Data.Bool          ( Bool(False, True), not )
import Data.Function      ( ($), id )
import Data.Functor       ( fmap  )
import Data.IORef         ( newIORef, readIORef, writeIORef )
import Data.Maybe         ( maybe )
import Prelude            ( fromInteger )
import System.Timeout     ( timeout )
import System.IO ( IO )

-- from base-unicode-symbols:
import Prelude.Unicode       ( (⋅) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock   as Lock
import qualified Control.Concurrent.Thread as Thread

import TestUtils ( a_moment )

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework ( Test, defaultMain )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )


-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testCase "wait"           test_wait
        , testCase "isRunning"      test_isRunning
        , testCase "blockedState"   test_blockedState
        , testCase "unblockedState" test_unblockedState
        ]

test_wait ∷ Assertion
test_wait = assert $ fmap (maybe False id) $ timeout (10 ⋅ a_moment) $ do
  r ← newIORef False
  tid ← Thread.forkIO $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← Thread.wait tid
  readIORef r

test_isRunning ∷ Assertion
test_isRunning = assert $ fmap (maybe False id) $ timeout (10 ⋅ a_moment) $ do
  l ← Lock.newAcquired
  tid ← Thread.forkIO $ Lock.acquire l
  r ← Thread.isRunning tid
  Lock.release l
  return r

test_blockedState ∷ Assertion
test_blockedState = (block $ Thread.forkIO $ blocked) >>=
                    Thread.unsafeWait >>= assert

test_unblockedState ∷ Assertion
test_unblockedState = (unblock $ Thread.forkIO $ fmap not $ blocked) >>=
                      Thread.unsafeWait >>= assert


-- The End ---------------------------------------------------------------------
