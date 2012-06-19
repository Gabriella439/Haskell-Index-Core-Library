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

import Control.Category ((<<<), (>>>))
import Control.Category.Index

infixr 1 =<?, <?<, >?>
infixl 1 ?>=

{- $imonads
    I deviate from Conor's terminology, referring to his monads on indexed types
    as \"indexed monads\" and referring to his indexed monads as \"restricted
    monads\".  This module provides \"indexed monads\".

    Indexed monads generalize the traditional approach to parametrizing the
    initial and final states of ordinary monads.  The 'IMonad' class does not
    require specifying a concrete index of kind @*@ for the intermediate or
    final state of the 'bindI' operation, permitting operations which may end in
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

> returnI >?> f = f
>
> f >?> returnI = f
>
> (f >?> g) >?> h = f >?> (g >?> h)
-}
class (IFunctor m) => IMonad m where
    returnI ::  a :-> m a
    bindI :: (a :-> m b) -> (m a :-> m b)

{- $functions
    Functions derived from 'returnI' and 'bindI'
-}

-- | An infix 'bindI'
(=<?) :: (IMonad m) => (a :-> m b) -> (m a :-> m b)
(=<?) = bindI

-- | An infix 'bindI' with arguments flipped
(?>=) :: (IMonad m) => m a i -> (a :-> m b) -> m b i
(?>=) = flip bindI

{-|
    Composition of indexed Kleisli arrows

    This is equivalent to ('>>>') from @Control.Category@.
-}
(>?>) :: (IMonad m) => (a :-> m b) -> (b :-> m c) -> (a :-> m c)
f >?> g = \x -> f x ?>= g

{-|
    Composition of indexed Kleisli arrows

    This is equivalent to ('<<<') from @Control.Category@.
-}
(<?<) :: (IMonad m) => (b :-> m c) -> (a :-> m b) -> (a :-> m c)
f <?< g = \x -> f =<? g x
