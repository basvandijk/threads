module Main where

import Control.Monad ( replicateM_ )

import System.Environment ( getArgs )

import qualified Control.Concurrent.Thread.Group as ThreadGroup

main = do
  [sn] <- getArgs
  tg <- ThreadGroup.new
  replicateM_ (read sn) $
    ThreadGroup.forkIO tg $
      return ()
