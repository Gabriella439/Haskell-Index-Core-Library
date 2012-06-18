{-|
    This module is the indexed equivalent to @Control.Monad.Trans.Class@ from
    the @transformers@ package.
-}

{-# LANGUAGE TypeOperators #-}

module Control.IMonad.Trans (
    -- * Monad Transformers
    -- $transform
    IMonadTrans(..),
    liftU
    ) where

import Control.IMonad.Core
import Control.IMonad.Restrict

{- $transform
    Indexed monad transformers transform computations in a base /indexed/ monad
    into a higher /indexed/ monad layered on top of the base monad.

    Note that this does not lift ordinary monads.  To do that you must first
    convert the ordinary monad to a restricted monad using the 'u' function from
    "Control.IMonad.Restrict" and then 'lift' it like so:

> -- Using indexed do notation provided by Control.IMonad.Do
> do x <- indexedActionInHigherMonad
>    lift $ u $ ordinaryActionInBaseMonad x
-}

{-|
    An indexed monad transformer.

    All instances must satisfy the monad transformer laws:

> lift . skip = skip
> lift . (f >?> g) = (lift . f) >?> (lift . g)
-}
class IMonadTrans t where
    lift :: (IMonad m) => m a :-> t m a

{-|
    Use this to lift ordinary monads for indexed monad transformers

> liftU = lift . u
-}
liftU :: (Monad m, IMonadTrans t) => m a -> R (t (U m)) i i a
liftU = lift . u
