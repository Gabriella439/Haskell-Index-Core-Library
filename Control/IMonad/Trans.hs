{-# LANGUAGE TypeOperators #-}

module Control.IMonad.Trans (IMonadTrans(..)) where

import Control.IMonad

{-|
    An indexed monad transformer.

    All instances must satisfy the monad transformer laws:

> lift . skip = skip
> lift . (f >?> g) = lift . f >?> lift . g
-}
class IMonadTrans t where
    lift :: (IMonad m) => m a :-> t m a
