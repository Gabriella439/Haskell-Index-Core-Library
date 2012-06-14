-- | Use this module in conjunction with the @RebindableSyntax@ extension to
--   rebind @do@ notation to work with indexed monads.  This module re-exports
--   "Control.IMonad", so it only requires the following minimum file header:
--
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Control.IMonad.Do
-- > import Prelude hiding (Monad(..))

{-# LANGUAGE GADTs, Rank2Types, TypeOperators #-}

module Control.IMonad.Do (
    module Control.IMonad,
    return,
    (>>=),
    (>>),
    fail
    ) where

import Control.IMonad

import Prelude hiding (Monad(..))

-- | 'return' replaces @return@ from @Control.Monad@.
return :: (IMonad m) => a -> m (a := i) i
return = skip . V

-- | ('>>=') replaces (@>>=@) from @Control.Monad@.
(>>=) :: (IMonad m) => m (a := j) i -> (a -> m b j) -> m b i
m >>= f = bind (\(V a) -> f a) m

-- | ('>>') replaces (@>>@) from @Control.Monad@.
(>>) :: (IMonad m) => m (a := j) i -> m b j -> m b i
m1 >> m2 = m1 >>= \_ -> m2

-- | 'fail' replaces @fail@ from @Control.Monad@
fail :: String -> m a i
fail = error
