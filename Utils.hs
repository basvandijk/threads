{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception            ( SomeException(SomeException), throwIO )
import Control.Monad                ( Monad, return, (>>=), (>>), fail )
import Data.Bool                    ( Bool )
import Data.Function                ( ($), flip )
import Data.Functor                 ( Functor, (<$>), (<$) )
import Data.Maybe                   ( Maybe(Nothing, Just) )
import System.IO                    ( IO )

-- from stm:
import Control.Concurrent.STM       ( atomically )
import Control.Concurrent.STM.TMVar ( TMVar, tryTakeTMVar, putTMVar )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

(<$$>) ∷ Functor f ⇒ f α → (α → β) → f β
(<$$>) = flip (<$>)

void ∷ Functor f ⇒ f α → f ()
void = (() <$)

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

throwInner ∷ SomeException → IO α
throwInner (SomeException e) = throwIO e

tryReadTMVar ∷ TMVar α → IO (Maybe α)
tryReadTMVar mv = atomically $ do
                    mx ← tryTakeTMVar mv
                    case mx of
                      Nothing → return mx
                      Just x  → putTMVar mv x >> return mx


-- The End ---------------------------------------------------------------------
