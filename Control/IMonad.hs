{-# LANGUAGE GADTs, Rank2Types, TypeOperators #-}

module Control.IMonad (
    -- * Indexed monads
    -- $imonad
    (:->),
    IFunctor(..),
    IMonad(..),
    (?>=),
    (=<?),
    (>?>),
    (<?<),
    -- * Restricted indexed monads
    -- $restrict
    (:=)(..),
    rskip,
    (!>=),
    -- * Interoperability
    -- $interop
    F(..),
    f
    ) where

import Control.Monad (liftM)

{- $imonad
    Indexed monads generalize the traditional approach to parametrizing the
    initial and final states of ordinary monads.  The 'IMonad' class does not
    require specifying concrete indices of kind @*@ for either the initial or
    intermediate index of the 'bind' operation, permitting operations which may
    end in multiple possible states.
-}

-- | An index-preserving function from @a@ to @b@
type a :-> b = forall i . a i -> b i

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

{- $restrict
    You can recapitulate traditional indexed monads where the starting index is
    fixed by using the (':=') type operator, which restricts a value to a single
    index.

    I call this variation of indexed monad a \"restricted indexed monad\", to
    distinguish it from 'IMonad', which I refer to as just an \"indexed monad\".

    'rskip' and ('!>=') provide the restricted operations corresponding to
    'skip' and ('?>=').  If 'skip' and ('?>=') satisfy the monad laws, then so
    do 'rskip' and ('!>=').
-}

{-|
    @(a := i)@ represents a locked value of type @a@ that can only access 
    at the index @i@.

    'V' seals values of type @a@, restricting them to a single index.
-}
data (a := i) j where V :: a -> (a := i) i

-- | A 'skip' that restricts the initial index
rskip :: (IMonad m) => a -> m (a := i) i
rskip = skip . V

-- | A 'bind' that restricts the initial index
(!>=) :: (IMonad m) => m (a := j) i -> (a -> m b j) -> m b i
m !>= f = bind (\(V a) -> f a) m

{- $interop
    The following types and functions transform ordinary types into indexed
    types.
-}

-- | 'F' bridges ordinary functors and indexed functors
data F f a i where
    F :: { unF :: f (a i) } -> F f a i

instance (Functor f) => IFunctor (F f) where
    imap f (F x) = F (fmap f x)

instance (Functor m, Monad m) => IMonad (F m) where
    skip = F . return
    bind f (F m) = F (m >>= unF . f)

-- | 'f' transforms a functor into a restricted indexed functor
f :: (Functor f) => f a -> F f (a := i) i
f x = F (fmap V x)
