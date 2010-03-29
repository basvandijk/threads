{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar ( MVar, takeMVar, putMVar, tryTakeMVar )
import Control.Exception       ( SomeException(SomeException)
                               , block, throwIO
                               )
import Control.Monad           ( Monad, return, (>>=), (>>), fail )
import Data.Bool               ( Bool(False, True), otherwise )
import Data.Function           ( ($) )
import Data.Functor            ( Functor, (<$) )
import Data.Maybe              ( Maybe(Nothing, Just) )
import Prelude                 ( ($!) )
import System.IO               ( IO )

-- from base-unicode-symbols:
import Data.Function.Unicode   ( (∘) )


--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Strict function composition.
(∘!) ∷ (β → γ) → (α → β) → (α → γ)
f ∘! g = (f $!) ∘ g

void ∷ Functor f ⇒ f α → f ()
void = (() <$)

ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

anyM ∷ Monad m ⇒ (α → m Bool) → [α] → m Bool
anyM p = anyM_p
    where
      anyM_p []     = return False
      anyM_p (x:xs) = ifM (p x) (return True) (anyM_p xs)

throwInner ∷ SomeException → IO α
throwInner (SomeException e) = throwIO e

purelyModifyMVar ∷ MVar α → (α → α) → IO ()
purelyModifyMVar mv f = block $ takeMVar mv >>= putMVar mv ∘! f

tryRead ∷ MVar α → IO (Maybe α)
tryRead mv = block $ do mx ← tryTakeMVar mv
                        case mx of
                          Nothing → return mx
                          Just x  → putMVar mv x >> return mx
{-|
/Strictly/ delete the first element of the list which satisfies the predicate.

This function strictly constructs the list up until the point of the deleted element.
-}
deleteFirstWhich' ∷ (α → Bool) → [α] → [α]
deleteFirstWhich' p = deleteFirstWhich'_p
    where
      deleteFirstWhich'_p []      = []
      deleteFirstWhich'_p (x:xs)
                      | p x       = xs
                      | otherwise = (x:) $! deleteFirstWhich'_p xs


-- The End ---------------------------------------------------------------------
