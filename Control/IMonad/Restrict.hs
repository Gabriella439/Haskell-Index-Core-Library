{-|
    Restricted monads are a subset of indexed monads where the return value is
    restricted to a single index.  They build on top of 'IMonad' using the
    (':=') type constructor which restricts the index of the return value.
-}

{-# LANGUAGE TypeOperators, GADTs, Rank2Types #-}

module Control.IMonad.Restrict (
    -- * Restriction
    -- $restrict
    (:=)(..),
    R,
    returnR,
    (!>=),
    -- * Functions
    -- $functions
    fmapR,
    (<!>),
    (<.>),
    (=<!),
    (!>),
    (>!>),
    (<!<),
    joinR,
    voidR,
    foreverR,
    mapMR,
    mapMR_,
    forMR,
    forMR_,
    replicateMR,
    replicateMR_,
    sequenceR,
    sequenceR_,
    whenR,
    unlessR,
    -- * Interoperability
    -- $interop
    U(..),
    u,
    D(..)
    ) where

import Control.Category ((<<<), (>>>))
import Control.IMonad.Core
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative (..))

-- Just copying the fixities from Control.Monad
infixr 1 =<!, <!<, >!>
infixl 1 !>, !>=

{- $restrict
    The (':=') type constructor restricts the index that the return value
    inhabits.

    'returnR' and ('!>=') provide the restricted operations corresponding to
    'returnI' and ('?>=').  If 'returnI' and ('?>=') satisfy the monad laws,
    then so will 'returnR' and ('!>='):

> returnR >!> f = f
>
> f >!> returnR = f
>
> (f >!> g) >!> h = f >!> (g >!> h)

    The type synonym 'R' rearranges the type variables of the restricted monad
    to match conventional notation.
-}

{-|
    @(a := i)@ represents a locked value of type @a@ that you can only access 
    at the index @i@.

    'V' seals values of type @a@, restricting them to a single index @i@.
-}
data (a := i) j where V :: a -> (a := i) i

-- | An indexed monad where the final index, @j@, is \'R\'estricted
type R m i j a = m (a := j) i

-- | A 'returnI' that restricts the final index
returnR :: (IMonad m) => a -> m (a := i) i
returnR = returnI . V

-- | A flipped 'bindI' that restricts the intermediate and final index
(!>=) :: (IMonad m) => m (a := j) i -> (a -> m (b := k) j) -> m (b := k) i
m !>= f = bindI (\(V a) -> f a) m

{- $functions
    Functions derived from 'returnR' and ('!>=')
-}

-- | All restricted monads are ordinary functors
fmapR :: (IMonad m) => (a -> b) -> m (a := j) i -> m (b := j) i
fmapR f m = m !>= returnR . f

-- | Infix 'fmapR'
(<!>) :: (IMonad m) => (a -> b) -> m (a := j) i -> m (b := j) i
(<!>) = fmapR

-- | All restricted monads are restricted applicatives
(<.>) :: (IMonad m) => m ((a -> b) := j) i -> m (a := k) j -> m (b := k) i
mf <.> mx = mf !>= \f -> f <!> mx

-- | A 'bindI' that restricts the intermediate and final index
(=<!) :: (IMonad m) => (a -> m (b := k) j) -> m (a := j) i -> m (b := k) i
(=<!) = flip (!>=)

-- | Sequence two indexed monads
(!>) :: (IMonad m) => m (a := j) i -> m (b := k) j -> m (b := k) i
m1 !> m2 = m1 !>= \_ -> m2

{-|
    Composition of restricted Kleisli arrows

    This is equivalent to ('>>>') from @Control.Category@.
-}
(>!>) :: (IMonad m) =>
    (a -> m (b:= j) i) -> (b -> m (c := k) j) -> (a -> m (c := k) i)
f >!> g = \x -> f x !>= g

{-|
    Composition of restricted Kleisli arrows

    This is equivalent to ('<<<') from @Control.Category@.
-}
(<!<) :: (IMonad m) =>
    (b -> m (c := k) j) -> (a -> m (b := j) i) -> (a -> m (c := k) i)
f <!< g = \x -> f =<! g x

-- | 'joinR' joins two monad layers into one
joinR :: (IMonad m) => m ((m (a := k) j) := j) i -> m (a := k) i
joinR m = m !>= id

-- | Discard the result of evaluation
voidR :: (IMonad m) => m (a := i) i -> m (() := i) i
voidR m = m !> returnR ()

-- | 'foreverR' repeats the action indefinitely
foreverR :: (IMonad m) => m (a := i) i -> m (b := j) i
foreverR m = let r = m !> r in r

-- | \"@mapMR f@\" is equivalent to \"@sequenceR . map f@\"
mapMR :: (IMonad m) => (a -> m (b := i) i) -> [a] -> m ([b] := i) i
{-# INLINE mapMR #-}
mapMR f as = sequenceR (map f as)

-- | \"@mapMR_ f@\" is equivalent to \"@sequenceR_ . map f@\"
mapMR_ :: (IMonad m) => (a -> m (b := i) i) -> [a] -> m (() := i) i
{-# INLINE mapMR_ #-}
mapMR_ f as = sequenceR_ (map f as)

-- | 'mapMR' with its arguments flipped
forMR :: (IMonad m) => [a] -> (a -> m (b := i) i) -> m ([b] := i) i
{-# INLINE forMR #-}
forMR = flip mapMR

-- | 'mapMR_' with its arguments flipped
forMR_ :: (IMonad m) => [a] -> (a -> m (b := i) i) -> m (() := i) i
{-# INLINE forMR_ #-}
forMR_ = flip mapMR_

-- | \"@replicateMR n m@\" performs @m@ @n@ times and collects the results
replicateMR :: (IMonad m) => Int -> m (a := i) i -> m ([a] := i) i
replicateMR n x = sequenceR (replicate n x)

-- | \"@replicateMR_ n m@\" performs @m@ @n@ times and ignores the results
replicateMR_ :: (IMonad m) => Int -> m (a := i) i -> m (() := i) i
replicateMR_ n x = sequenceR_ (replicate n x)

-- | Evaluate each action from left to right and collect the results
sequenceR :: (IMonad m) => [m (a := i) i] -> m ([a] := i) i
{-# INLINE sequenceR #-}
sequenceR ms = foldr k (returnR []) ms where
    k m m' = m  !>= \x  ->
             m' !>= \xs ->
             returnR (x:xs)

-- | Evaluate each action from left to right and ignore the results
sequenceR_ :: (IMonad m) => [m (a := i) i] -> m (() := i) i
{-# INLINE sequenceR_ #-}
sequenceR_ ms = foldr (!>) (returnR ()) ms

-- | \"@whenR p m@\" executes @m@ if @p@ is 'True'
whenR :: (IMonad m) => Bool -> m (() := i) i -> m (() := i) i
whenR p s = if p then s else returnR ()

-- | \"@unlessR p m@\" executes @m@ if @p@ is 'False'
unlessR :: (IMonad m) => Bool -> m (() := i) i -> m (() := i) i
unlessR p s = if p then returnR () else s

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
    fmapI f m = m ?>= (returnI . f)

instance (Monad m) => IMonad (U m) where
    returnI = U . return
    bindI f (U m) = U (m >>= (unU . f))

-- | 'u' transforms an ordinary monad into a restricted monad
u :: (Monad m) => m a -> (U m) (a := i) i
u x = U (liftM V x)

{-|
    The 'D' type \'D\'owngrades index-preserving restricted monads to ordinary
    monads
-}
data D i m r = D { unD :: m (r := i) i }

instance (IMonad m) => Monad (D i m) where
    return = D . returnR
    (D m) >>= f = D (m !>= (unD . f))

instance (IMonad m) => Applicative (D i m) where
    pure = return
    (<*>) = ap

instance (IMonad m) => Functor (D i m) where
    fmap f (D m) = D (fmapR f m)
