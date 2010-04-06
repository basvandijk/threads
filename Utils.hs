{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, putMVar, tryTakeMVar )
import Control.Exception       ( SomeException(SomeException)
                               , block, throwIO
                               )
import Control.Monad           ( Monad, return, (>>=), (>>), fail )
import Data.Bool               ( Bool )
import Data.Function           ( ($) )
import Data.Functor            ( Functor, (<$) )
import Data.Maybe              ( Maybe(Nothing, Just) )
import System.IO               ( IO )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

void ∷ Functor f ⇒ f α → f ()
void = (() <$)

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

whenThen ∷ Monad m ⇒ Bool → m () → m α → m α
whenThen b t a = if b then t >> a else a

throwInner ∷ SomeException → IO α
throwInner (SomeException e) = throwIO e

tryRead ∷ MVar α → IO (Maybe α)
tryRead mv = block $ do mx ← tryTakeMVar mv
                        case mx of
                          Nothing → return mx
                          Just x  → putMVar mv x >> return mx


-- The End ---------------------------------------------------------------------
