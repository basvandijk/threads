{-# LANGUAGE CPP, NoImplicitPrelude, UnicodeSyntax #-}

module Mask ( mask ) where

#if MIN_VERSION_base(4,3,0)
import Control.Exception ( mask )
#else
import Control.Monad     ( (>>=) )
import Control.Exception ( blocked, block, unblock )
import Data.Function     ( ($), id )
import System.IO         ( IO )

mask ∷ ((IO α → IO α) → IO β) → IO β
mask io = blocked >>= \b → if b then io id else block $ io unblock
#endif
