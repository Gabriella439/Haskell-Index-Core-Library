{-|
    Restricted monads are a subset of indexed monads where the final state is
    restricted to a single index.  They build on top of 'IMonad' using the
    (':=') type constructor which restricts the final state of the return value.
-}

{-# LANGUAGE TypeOperators, GADTs, Rank2Types #-}

module Control.IMonad.Restrict (
    -- * Restriction
    -- $restrict
    (:=)(..),
    R,
    rskip,
    (!>=),
    -- * Functions
    -- $functions
    (=<!),
    (!>),
    (>!>),
    (<!<),
    join,
    forever,
    -- * Interoperability
    -- $interop
    U(..),
    u,
    D(..)
    ) where

import Control.IMonad.Core
import Control.Monad (liftM)

-- Just copying the fixities from Control.Monad
infixr 1 =<!, <!<, >!>
infixl 1 !>, !>=

{- $restrict
    The (':=') type constructor restricts the final index that the return value
    inhabits.

    'rskip' and ('!>=') provide the restricted operations corresponding to
    'skip' and ('?>=').

    Type type synonym 'R' rearranges the type variables of the restricted monad
    to match conventional notation.
-}

{-|
    @(a := i)@ represents a locked value of type @a@ that you can only access 
    at the index @i@.

    'V' seals values of type @a@, restricting them to a single index @i@.
-}
data (a := i) j where V :: a -> (a := i) i

-- | An indexed monad where the final index is \'R\'estricted
type R m i j a = m (a := j) i

-- | A 'skip' that restricts the final index
rskip :: (IMonad m) => a -> R m i i a
rskip = skip . V

-- | A flipped 'bind' that restricts the intermediate and final index
(!>=) :: (IMonad m) => R m i j a -> (a -> R m j k b) -> R m i k b
m !>= f = bind (\(V a) -> f a) m

{- $functions
    Functions derived from 'rskip' and ('!>=')
-}

-- | A 'bind' that restricts the intermediate and final index
(=<!) :: (IMonad m) => (a -> R m j k b) -> R m i j a -> R m i k b
(=<!) = flip (!>=)

-- | Sequence two indexed monads
(!>) :: (IMonad m) => R m i j a -> R m j k b -> R m i k b
m1 !> m2 = m1 !>= \_ -> m2

-- | Composition of restricted Kleisli arrows (equivalent to @>>>@)
(>!>) :: (IMonad m) => (a -> R m i j b) -> (b -> R m j k c) -> (a -> R m i k c)
f >!> g = \x -> f x !>= g

-- | Composition of restricted Kleisli arrows (equivalent to @<<<@)
(<!<) :: (IMonad m) => (b -> R m j k c) -> (a -> R m i j b) -> (a -> R m i k c)
f <!< g = \x -> f =<! g x

-- | 'join' joins two monad layers into one
join :: (IMonad m) => R m i j (R m j k a) -> R m i k a
join m = m !>= id

-- | 'forever' repeats the action indefinitely
forever :: (IMonad m) => R m i i a -> R m i j b
forever m = m !> forever m

{- $interop
    The following types and functions convert between ordinary monads and
    restricted monads.

    Use 'u' to convert an ordinary monad to a restricted monad so that it can be
    used within an indexed @do@ block like so:

> -- Both do blocks are indexed, using syntax rebinding from Control.IMonad.Do
> do x <- indexedAction
>    lift $ do
>        y <- u $ ordinaryAction1 x
>        u $ ordinaryAction2 x y

    Use 'D' to convert an index-preserving restricted monad into an ordinary
    monad so that it can be used within a normal @do@ block.

> -- An ordinary do block (i.e. without syntax rebinding from Control.IMonad.Do)
> do x <- D $ indexPreservingAction
>    D $ anotherIndexPreservingAction x
-}

-- | The 'U' type \'U\'pgrades ordinary monads to restricted monads
data U m a i where
    U :: { unU :: m (a i) } -> U m a i

instance (Monad m) => IFunctor (U m) where
    imap f m = m ?>= (skip . f)

instance (Monad m) => IMonad (U m) where
    skip = U . return
    bind f (U m) = U (m >>= (unU . f))

-- | 'u' transforms a functor into a restricted functor
u :: (Monad m) => m a -> R (U m) i i a
u x = U (liftM V x)

{-|
    The 'D' type \'D\'owngrades index-preserving restricted monads to ordinary
    monads
-}
data D i m r = D { unD :: R m i i r }

instance (IMonad m) => Monad (D i m) where
    return = D . rskip
    (D m) >>= f = D (m !>= (unD . f))
