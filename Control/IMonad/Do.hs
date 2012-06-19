-- | This module rebinds @do@ notation to work with restricted monads in
--   conjunction with the @RebindableSyntax@ extension.  This module re-exports
--   "Control.IMonad", so it only requires the following minimum file header:
--
-- > {-# LANGUAGE RebindableSyntax #-}
-- >
-- > import Control.IMonad.Do
-- > import Prelude hiding (Monad(..))
--
-- The Prelude is reimported since @RebindableSyntax@ also includes the
-- @NoImplicitPrelude@ extension, otherwise the Prelude's @Monad@ bindings would
-- conflict with these bindings.

{-# LANGUAGE GADTs, Rank2Types, TypeOperators #-}

module Control.IMonad.Do (
    -- * Modules
    module Control.IMonad,
    -- * Rebindings
    return,
    (>>=),
    (>>),
    fail
    ) where

import Control.IMonad

import Prelude hiding (Monad(..))

-- | 'return' replaces @return@ from @Control.Monad@.
return :: (IMonad m) => a -> m (a := i) i
return = skipR

-- | ('>>=') replaces (@>>=@) from @Control.Monad@.
(>>=) :: (IMonad m) => m (a := j) i -> (a -> m (b := k) j) -> m (b := k) i
(>>=) = (!>=)

-- | ('>>') replaces (@>>@) from @Control.Monad@.
(>>) :: (IMonad m) => m (a := j) i -> m (b := k) j -> m (b := k) i
(>>) = (!>)

-- | 'fail' replaces @fail@ from @Control.Monad@
fail :: String -> m (a := j) i
fail = error
