{-# LANGUAGE NoImplicitPrelude
           , ScopedTypeVariables
           , UnicodeSyntax  
  #-}

module TestUtils where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative ( (<$>) )
import Control.Concurrent  ( threadDelay )
import Control.Exception   ( try, SomeException )
import Control.Monad       ( (>>=), return, fail )
import Data.Bool           ( Bool, not )
import Data.Char           ( String )
import Data.Either         ( Either(Left, Right) )
import Data.Int            ( Int )
import Data.Maybe          ( isJust )
import Prelude             ( fromInteger )
import System.IO           ( IO )
import System.Timeout      ( timeout )

-- from HUnit:
import Test.HUnit          ( Assertion, assertFailure )


-------------------------------------------------------------------------------
-- Utilities for testing
-------------------------------------------------------------------------------

-- Exactly 1 moment. Currently equal to 0.005 seconds.
a_moment ∷ Int
a_moment = 5000

wait_a_moment ∷ IO ()
wait_a_moment = threadDelay a_moment

-- True if the action 'a' evaluates within 't' μs.
within ∷ Int → IO α → IO Bool
within t a = isJust <$> timeout t a

notWithin ∷ Int → IO α → IO Bool
notWithin t a = not <$> within t a

assertException ∷ String → IO α → Assertion
assertException errMsg a = do e ← try a
                              case e of
                                Left (_ ∷ SomeException ) → return ()
                                Right _ → assertFailure errMsg


-- The End ---------------------------------------------------------------------
