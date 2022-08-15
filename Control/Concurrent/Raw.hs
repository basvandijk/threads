{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}

module Control.Concurrent.Raw ( rawForkIO, rawForkOn ) where

import Data.Function ( ($) )
import GHC.IO        ( IO(IO) )
import GHC.Exts      ( Int(I#), fork#, forkOn# )
import GHC.Conc      ( ThreadId(ThreadId) )

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
#if MIN_VERSION_base(4,17,0)
rawForkIO (IO action) = IO $ \s ->
#else
rawForkIO action = IO $ \s ->
#endif
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE rawForkOn #-}
rawForkOn :: Int -> IO () -> IO ThreadId
#if MIN_VERSION_base(4,17,0)
rawForkOn (I# cpu) (IO action) = IO $ \s ->
#else
rawForkOn (I# cpu) action = IO $ \s ->
#endif
   case (forkOn# cpu action s) of (# s1, tid #) -> (# s1, ThreadId tid #)
