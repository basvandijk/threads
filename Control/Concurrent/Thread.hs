{-# LANGUAGE CPP, NoImplicitPrelude, RankNTypes, ImpredicativeTypes #-}

#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module     : Control.Concurrent.Thread
-- Copyright  : (c) 2010-2012 Bas van Dijk & Roel van Dijk
-- License    : BSD3 (see the file LICENSE)
-- Maintainer : Bas van Dijk <v.dijk.bas@gmail.com>
--            , Roel van Dijk <vandijk.roel@gmail.com>
--
-- Standard threads extended with the ability to /wait/ for their return value.
--
-- This module exports equivalently named functions from @Control.Concurrent@
-- (and @GHC.Conc@). Avoid ambiguities by importing this module qualified. May
-- we suggest:
--
-- @
-- import qualified Control.Concurrent.Thread as Thread ( ... )
-- @
--
-- The following is an example how to use this module:
--
-- @
--
-- import qualified Control.Concurrent.Thread as Thread ( 'forkIO', 'result' )
--
-- main = do (tid, wait) <- Thread.'forkIO' $ do x <- someExpensiveComputation
--                                            return x
--          doSomethingElse
--          x <- Thread.'result' =<< 'wait'
--          doSomethingWithResult x
-- @
--
--------------------------------------------------------------------------------

module Control.Concurrent.Thread
  ( -- * Forking threads
    forkIO
  , forkOS
  , forkOn
  , forkIOWithUnmask
  , forkOnWithUnmask

    -- * Results
  , Result
  , result
  ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import qualified Control.Concurrent ( forkOS
                                    , forkIOWithUnmask
                                    , forkOnWithUnmask
                                    )
import Control.Concurrent           ( ThreadId )
import Control.Concurrent.MVar      ( newEmptyMVar, putMVar, readMVar )
import Control.Exception            ( SomeException, try, throwIO, mask )
import Control.Monad                ( return, (>>=) )
import Data.Either                  ( Either(..), either )
import Data.Function                ( (.), ($) )
import Data.Int                     ( Int )
import System.IO                    ( IO )

-- from threads:
import Control.Concurrent.Raw       ( rawForkIO, rawForkOn )


--------------------------------------------------------------------------------
-- * Forking threads
--------------------------------------------------------------------------------

-- | Like @Control.Concurrent.'Control.Concurrent.forkIO'@ but returns
-- a computation that when executed blocks until the thread terminates
-- then returns the final value of the thread.
forkIO :: IO a -> IO (ThreadId, IO (Result a))
forkIO = fork rawForkIO

-- | Like @Control.Concurrent.'Control.Concurrent.forkOS'@ but returns
-- a computation that when executed blocks until the thread terminates
-- then returns the final value of the thread.
forkOS :: IO a -> IO (ThreadId, IO (Result a))
forkOS = fork Control.Concurrent.forkOS

-- | Like @Control.Concurrent.'Control.Concurrent.forkOn'@ but returns
-- a computation that when executed blocks until the thread terminates
-- then returns the final value of the thread.
forkOn :: Int -> IO a -> IO (ThreadId, IO (Result a))
forkOn = fork . rawForkOn

-- | Like @Control.Concurrent.'Control.Concurrent.forkIOWithUnmask'@ but returns
-- a computation that when executed blocks until the thread terminates
-- then returns the final value of the thread.
forkIOWithUnmask
    :: ((forall b. IO b -> IO b) -> IO a) -> IO (ThreadId, IO (Result a))
forkIOWithUnmask = forkWithUnmask Control.Concurrent.forkIOWithUnmask

-- | Like @Control.Concurrent.'Control.Concurrent.forkOnWithUnmask'@ but returns
-- a computation that when executed blocks until the thread terminates
-- then returns the final value of the thread.
forkOnWithUnmask
    :: Int -> ((forall b. IO b -> IO b) -> IO a) -> IO (ThreadId, IO (Result a))
forkOnWithUnmask = forkWithUnmask . Control.Concurrent.forkOnWithUnmask


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

fork :: (IO () -> IO ThreadId) -> (IO a -> IO (ThreadId, IO (Result a)))
fork doFork = \a -> do
  res <- newEmptyMVar
  tid <- mask $ \restore -> doFork $ try (restore a) >>= putMVar res
  return (tid, readMVar res)

forkWithUnmask
    :: (((forall b. IO b -> IO b) -> IO ()) -> IO ThreadId)
    ->  ((forall b. IO b -> IO b) -> IO a)  -> IO (ThreadId, IO (Result a))
forkWithUnmask doForkWithUnmask = \f -> do
  res <- newEmptyMVar
  tid <- mask $ \restore ->
           doForkWithUnmask $ \unmask ->
             try (restore $ f unmask) >>= putMVar res
  return (tid, readMVar res)


--------------------------------------------------------------------------------
-- Results
--------------------------------------------------------------------------------

-- | A result of a thread is either some exception that was thrown in the thread
-- and wasn't catched or the actual value that was returned by the thread.
type Result a = Either SomeException a

-- | Retrieve the actual value from the result.
--
-- When the result is 'SomeException' the exception is thrown.
result :: Result a -> IO a
result = either throwIO return
