{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

module Control.Concurrent.Thread.Result ( Result(Result, unResult) ) where

-- from base:
import Control.Exception ( SomeException )
import Data.Either       ( Either )
import Data.Typeable     ( Typeable )

#ifdef __HADDOCK__
import System.IO ( IO )
#endif

-- from stm:
import Control.Concurrent.STM.TMVar ( TMVar )

{-|
A @'Result' &#x3B1;@ is an abstract type representing the result of a thread
that is executing or has executed a computation of type @'IO' &#x3B1;@.
-}
newtype Result α = Result { unResult ∷ TMVar (Either SomeException α) }
     deriving Typeable
