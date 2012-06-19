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
return :: (IMonad m) => a -> R m i i a
return = skipR

-- | ('>>=') replaces (@>>=@) from @Control.Monad@.
(>>=) :: (IMonad m) => R m i j a -> (a -> R m j k b) -> R m i k b
(>>=) = (!>=)

-- | ('>>') replaces (@>>@) from @Control.Monad@.
(>>) :: (IMonad m) => R m i j a -> R m j k b -> R m i k b
(>>) = (!>)

-- | 'fail' replaces @fail@ from @Control.Monad@
fail :: String -> m a i
fail = error
