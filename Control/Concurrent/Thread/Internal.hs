{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

module Control.Concurrent.Thread.Internal
    ( ThreadId( ThreadId )
    , result, threadId
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar           ( MVar )
import Control.Exception                 ( SomeException )
import Data.Eq                           ( Eq, (==) )
import Data.Either                       ( Either )
import Data.Function                     ( on )
import Data.Ord                          ( Ord, compare )
import Data.Typeable                     ( Typeable )
import Text.Show                         ( Show, show )
import qualified Control.Concurrent as C ( ThreadId )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )


-------------------------------------------------------------------------------
-- ThreadIds
-------------------------------------------------------------------------------

{-|
A @'ThreadId' &#x3B1;@ is an abstract type representing a handle to a thread
that is executing or has executed a computation of type @'IO' &#x3B1;@.
-}
data ThreadId α = ThreadId
    { result   ∷ MVar (Either SomeException α)
    , threadId ∷ C.ThreadId -- ^ Extract the underlying
                            -- @Control.Concurrent.'C.ThreadId'@.
    } deriving Typeable

instance Eq (ThreadId α) where
    (==) = (==) `on` threadId

-- | The @Ord@ instance implements an arbitrary total ordering over 'ThreadId's.
instance Ord (ThreadId α) where
    compare = compare `on` threadId

{-|
The @Show@ instance lets you convert an arbitrary-valued 'ThreadId' to string
form; showing a 'ThreadId' value is occasionally useful when debugging or
diagnosing the behaviour of a concurrent program.
-}
instance Show (ThreadId α) where
    show = show ∘ threadId


-- The End ---------------------------------------------------------------------
