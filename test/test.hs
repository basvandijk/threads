{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent ( ThreadId, threadDelay, throwTo, killThread )
import Control.Exception  ( Exception, fromException
                          , AsyncException(ThreadKilled)
                          , throwIO, mask_
                          , getMaskingState, MaskingState(MaskedInterruptible)
                          )
import Control.Monad      ( return, (>>=), replicateM_ )
import Data.Bool          ( Bool(False, True) )
import Data.Eq            ( Eq, (==) )
import Data.Either        ( either )
import Data.Function      ( ($), id, const, flip )
import Data.Functor       ( Functor(fmap), (<$>) )
import Data.Int           ( Int )
import Data.Maybe         ( Maybe, maybe )
import Data.IORef         ( newIORef, readIORef, writeIORef )
import Data.Typeable      ( Typeable )
import System.Timeout     ( timeout )
import System.IO          ( IO )
import Text.Show          ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode       ( (≡) )
import Prelude.Unicode       ( (⋅) )
import Data.Function.Unicode ( (∘) )

-- from concurrent-extra:
import qualified Control.Concurrent.Lock as Lock

-- from stm:
import Control.Concurrent.STM ( atomically )

-- from HUnit:
import Test.HUnit ( Assertion, assert )

-- from test-framework:
import Test.Framework ( Test, defaultMain, testGroup )

-- from test-framework-hunit:
import Test.Framework.Providers.HUnit ( testCase )

-- from threads:
import Control.Concurrent.Thread       ( Result, result )
import Control.Concurrent.Thread.Group ( ThreadGroup )

import qualified Control.Concurrent.Thread       as Thread
import qualified Control.Concurrent.Thread.Group as ThreadGroup


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Thread" $
          [ testGroup "forkIO" $
            [ testCase "wait"            $ test_wait            Thread.forkIO
            , testCase "maskingState"    $ test_maskingState    Thread.forkIO
            , testCase "sync exception"  $ test_sync_exception  Thread.forkIO
            , testCase "async exception" $ test_async_exception Thread.forkIO
            ]
          , testGroup "forkOS" $
            [ testCase "wait"            $ test_wait            Thread.forkOS
            , testCase "maskingState"    $ test_maskingState    Thread.forkOS
            , testCase "sync exception"  $ test_sync_exception  Thread.forkOS
            , testCase "async exception" $ test_async_exception Thread.forkOS
            ]
          , testGroup "forkOn 0" $
            [ testCase "wait"            $ test_wait            $ Thread.forkOn 0
            , testCase "maskingState"    $ test_maskingState    $ Thread.forkOn 0
            , testCase "sync exception"  $ test_sync_exception  $ Thread.forkOn 0
            , testCase "async exception" $ test_async_exception $ Thread.forkOn 0
            ]
          , testGroup "forkIOWithUnmask" $
            [ testCase "wait"            $ test_wait            $ wrapUnmask Thread.forkIOWithUnmask
            , testCase "sync exception"  $ test_sync_exception  $ wrapUnmask Thread.forkIOWithUnmask
            , testCase "async exception" $ test_async_exception $ wrapUnmask Thread.forkIOWithUnmask
            ]
          , testGroup "forkOnWithUnmask 0" $
            [ testCase "wait"            $ test_wait            $ wrapUnmask $ Thread.forkOnWithUnmask 0
            , testCase "sync exception"  $ test_sync_exception  $ wrapUnmask $ Thread.forkOnWithUnmask 0
            , testCase "async exception" $ test_async_exception $ wrapUnmask $ Thread.forkOnWithUnmask 0
            ]
          ]
        , testGroup "ThreadGroup" $
          [ testGroup "forkIO" $
            [ testCase "wait"              $ wrapIO test_wait
            , testCase "maskingState"      $ wrapIO test_maskingState
            , testCase "sync exception"    $ wrapIO test_sync_exception
            , testCase "async exception"   $ wrapIO test_async_exception

            , testCase "group single wait" $ test_group_single_wait ThreadGroup.forkIO
            , testCase "group nrOfRunning" $ test_group_nrOfRunning ThreadGroup.forkIO
            ]
          , testGroup "forkOS" $
            [ testCase "wait"              $ wrapOS test_wait
            , testCase "maskingState"      $ wrapOS test_maskingState
            , testCase "sync exception"    $ wrapOS test_sync_exception
            , testCase "async exception"   $ wrapOS test_async_exception

            , testCase "group single wait" $ test_group_single_wait ThreadGroup.forkOS
            , testCase "group nrOfRunning" $ test_group_nrOfRunning ThreadGroup.forkOS
            ]
          , testGroup "forkOn 0" $
            [ testCase "wait"              $ wrapOn_0 test_wait
            , testCase "maskingState"      $ wrapOn_0 test_maskingState
            , testCase "sync exception"    $ wrapOn_0 test_sync_exception
            , testCase "async exception"   $ wrapOn_0 test_async_exception

            , testCase "group single wait" $ test_group_single_wait $ ThreadGroup.forkOn 0
            , testCase "group nrOfRunning" $ test_group_nrOfRunning $ ThreadGroup.forkOn 0
            ]
          , testGroup "forkIOWithUnmask" $
            [ testCase "wait"              $ wrapIOWithUnmask test_wait
            , testCase "sync exception"    $ wrapIOWithUnmask test_sync_exception
            , testCase "async exception"   $ wrapIOWithUnmask test_async_exception

            , testCase "group single wait" $ test_group_single_wait $ wrapUnmask ∘ ThreadGroup.forkIOWithUnmask
            , testCase "group nrOfRunning" $ test_group_nrOfRunning $ wrapUnmask ∘ ThreadGroup.forkIOWithUnmask
            ]
          , testGroup "forkOnWithUnmask 0" $
            [ testCase "wait"              $ wrapOnWithUnmask test_wait
            , testCase "sync exception"    $ wrapOnWithUnmask test_sync_exception
            , testCase "async exception"   $ wrapOnWithUnmask test_async_exception

            , testCase "group single wait" $ test_group_single_wait $ wrapUnmask ∘ ThreadGroup.forkOnWithUnmask 0
            , testCase "group nrOfRunning" $ test_group_nrOfRunning $ wrapUnmask ∘ ThreadGroup.forkOnWithUnmask 0
            ]
          ]
        ]

-- Exactly 1 moment. Currently equal to 0.005 seconds.
a_moment ∷ Int
a_moment = 5000


--------------------------------------------------------------------------------
-- General properties
--------------------------------------------------------------------------------

type Fork α = IO α → IO (ThreadId, IO (Result α))

wrapUnmask ∷ ((β → α) → t) → α → t
wrapUnmask forkWithUnmask = \m -> forkWithUnmask $ const m

test_wait ∷ Fork () → Assertion
test_wait fork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  r ← newIORef False
  (_, wait) ← fork $ do
    threadDelay $ 2 ⋅ a_moment
    writeIORef r True
  _ ← wait
  readIORef r

test_maskingState ∷ Fork Bool → Assertion
test_maskingState fork = do (_, wait) ← mask_ $ fork $
                              (MaskedInterruptible ==) <$> getMaskingState
                            wait >>= result >>= assert

test_sync_exception ∷ Fork () → Assertion
test_sync_exception fork = assert $ do
  (_, wait) ← fork $ throwIO MyException
  waitForException MyException wait

waitForException ∷ (Exception e, Eq e) ⇒ e → IO (Result α) → IO Bool
waitForException e wait = wait <$$> either (justEq e ∘ fromException)
                                           (const False)

test_async_exception ∷ Fork () → Assertion
test_async_exception fork = assert $ do
  l ← Lock.newAcquired
  (tid, wait) ← fork $ Lock.acquire l
  throwTo tid MyException
  waitForException MyException wait

data MyException = MyException deriving (Show, Eq, Typeable)
instance Exception MyException

test_killThread ∷ Fork () → Assertion
test_killThread fork = assert $ do
  l ← Lock.newAcquired
  (tid, wait) ← fork $ Lock.acquire l
  killThread tid
  waitForException ThreadKilled wait


--------------------------------------------------------------------------------
-- ThreadGroup
--------------------------------------------------------------------------------

wrapIO ∷ (Fork α → IO β) → IO β
wrapIO = wrap ThreadGroup.forkIO

wrapOS ∷ (Fork α → IO β) → IO β
wrapOS = wrap ThreadGroup.forkOS

wrapOn_0 ∷ (Fork α → IO β) → IO β
wrapOn_0 = wrap $ ThreadGroup.forkOn 0

wrapIOWithUnmask ∷ (Fork α → IO β) → IO β
wrapIOWithUnmask = wrap $ \tg m -> ThreadGroup.forkIOWithUnmask tg $ const m

wrapOnWithUnmask ∷ (Fork α → IO β) → IO β
wrapOnWithUnmask = wrap $ \tg m -> ThreadGroup.forkOnWithUnmask 0 tg $ const m

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

test_group_nrOfRunning ∷ (ThreadGroup → Fork ()) → Assertion
test_group_nrOfRunning doFork = assert $ fmap isJustTrue $ timeout (10 ⋅ a_moment) $ do
  tg ← ThreadGroup.new
  l ← Lock.newAcquired
  replicateM_ n $ doFork tg $ Lock.acquire l
  true ← fmap (≡ n) $ (atomically $ ThreadGroup.nrOfRunning tg ∷ IO Int)
  Lock.release l
  return true
    where
      -- Don't set this number too big otherwise forkOS might throw an exception
      -- indicating that too many OS threads have been created:
      n ∷ Int
      n = 100


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Check if the given value equals 'Just' 'True'.
isJustTrue ∷ Maybe Bool → Bool
isJustTrue = maybe False id

-- | Check if the given value in the 'Maybe' equals the given reference value.
justEq ∷ Eq α ⇒ α → Maybe α → Bool
justEq = maybe False ∘ (≡)

-- | A flipped '<$>'.
(<$$>) ∷ Functor f ⇒ f α → (α → β) → f β
(<$$>) = flip (<$>)
