{-|
    This module provides the 'IFunctor' and 'IMonad' classes which are the
    indexed counterparts to 'Functor' and 'Monad' from @Control.Monad@.
-}

{-# LANGUAGE Rank2Types, TypeOperators #-}

module Control.IMonad.Core (
    -- * Indexed Monads
    -- $imonads
    IFunctor(..),
    IMonad(..),
    -- * Functions
    -- $functions
    (?>=),
    (=<?),
    (>?>),
    (<?<)
    ) where

import Data.Index

infixr 1 =<?, <?<, >?>
infixl 1 ?>=

{- $imonads
    Indexed monads generalize the traditional approach to parametrizing the
    initial and final states of ordinary monads.  The 'IMonad' class does not
    require specifying a concrete index of kind @*@ for the intermediate or
    final state of the 'bind' operation, permitting operations which may end in
    multiple possible states.
-}

{-|
    An endofunctor within the category of index-preserving functions

    All instances must satisfy the functor laws:

> imap id == id
>
> imap (f . g) == imap f . imap g
-}
class IFunctor f where imap :: (a :-> b) -> (f a :-> f b)

{-|
    An indexed monad

    All instances must satisfy the monad laws:

> bind skip == id
>
> bind f (skip x) == f x
>
> bind g (bind f m) == bind (\x -> bind g (f x)) m
-}
class (IFunctor m) => IMonad m where
    skip ::  a :-> m a
    bind :: (a :-> m b) -> (m a :-> m b)

{- $functions
    Functions derived from 'skip' and 'bind'
-}

-- | An infix 'bind'
(=<?) :: (IMonad m) => (a :-> m b) -> (m a :-> m b)
(=<?) = bind

-- | An infix 'bind' with arguments flipped
(?>=) :: (IMonad m) => m a i -> (a :-> m b) -> m b i
(?>=) = flip bind

-- | Composition of indexed Kleisli arrows (equivalent to @>>>@)
(>?>) :: (IMonad m) => (a :-> m b) -> (b :-> m c) -> (a :-> m c)
f >?> g = \x -> f x ?>= g

-- | Composition of indexed Kleisli arrows (equivalent to @<<<@)
(<?<) :: (IMonad m) => (b :-> m c) -> (a :-> m b) -> (a :-> m c)
f <?< g = \x -> f =<? g x
