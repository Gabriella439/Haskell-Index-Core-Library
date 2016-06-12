{-|
    This module is the indexed equivalent to @Control.Monad.Trans.Class@ from
    the @transformers@ package.
-}

{-# LANGUAGE TypeOperators, CPP #-}
#if MIN_VERSION_base(4,6,0)
{-# LANGUAGE PolyKinds #-}
#endif

module Control.IMonad.Trans (
    -- * Monad Transformers
    -- $transform
    IMonadTrans(..),
    liftU
    ) where

import Control.Category.Index
import Control.IMonad.Core
import Control.IMonad.Restrict

{- $transform
    Indexed monad transformers transform computations in a base /indexed/ monad
    into a higher /indexed/ monad layered on top of the base monad.

    Note that this does not lift ordinary monads.  To do that you must first
    convert the ordinary monad to a restricted monad using the 'u' function from
    "Control.IMonad.Restrict" and then use 'liftI' like so:

> -- Using indexed do notation provided by Control.IMonad.Do
> do x <- indexedActionInHigherMonad
>    liftI $ u $ ordinaryActionInBaseMonad x
-}

{-|
    An indexed monad transformer.

    All instances must satisfy the monad transformer laws:

> liftI . returnI = returnI
>
> liftI . (f >?> g) = (liftI . f) >?> (liftI . g)
-}
class IMonadTrans t where
    liftI :: (IMonad m) => m a :-> t m a

{-|
    Lifts ordinary monads for restricted monad transformers

> liftU = liftI . u
-}
liftU :: (Monad m, IMonadTrans t) => m a -> t (U m) (a := i) i
liftU = liftI . u
