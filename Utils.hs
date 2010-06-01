{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Utils where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Exception            ( SomeException(SomeException), throwIO )
import Control.Monad                ( Monad, return, (>>=), (>>), fail )
import Data.Bool                    ( Bool(..) )
import Data.Eq                      ( Eq )
import Data.Function                ( flip, id )
import Data.Functor                 ( Functor, (<$>), (<$) )
import Data.Maybe                   ( Maybe(Nothing, Just), maybe )
import Prelude                      ( ($!) )
import System.IO                    ( IO )

-- from base-unicode-symbols:
import Data.Eq.Unicode              ( (≡) )
import Data.Function.Unicode        ( (∘) )

-- from stm:
import Control.Concurrent.STM       ( STM )
import Control.Concurrent.STM.TMVar ( TMVar, tryTakeTMVar, putTMVar )
import Control.Concurrent.STM.TVar  ( TVar, readTVar, writeTVar )


--------------------------------------------------------------------------------
-- Utility functions
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

-- | Ignore the value.
void ∷ Functor f ⇒ f α → f ()
void = (() <$)

-- | Monadic @if then else@
ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM c t e = c >>= \b → if b then t else e

-- | Throw the exception stored inside the 'SomeException'.
throwInner ∷ SomeException → IO α
throwInner (SomeException e) = throwIO e

-- | Non retrying 'takeMVar'.
tryReadTMVar ∷ TMVar α → STM (Maybe α)
tryReadTMVar mv = do mx ← tryTakeTMVar mv
                     case mx of
                       Nothing → return mx
                       Just x  → putTMVar mv x >> return mx

-- | Strictly modify the contents of a 'TVar'.
modifyTVar ∷ TVar α → (α → α) → STM ()
modifyTVar tv f = readTVar tv >>= writeTVar tv ∘! f

-- | Strict function composition
(∘!) ∷ (β → γ) → (α → β) → (α → γ)
f ∘! g = \x → f $! g x


-- The End ---------------------------------------------------------------------
