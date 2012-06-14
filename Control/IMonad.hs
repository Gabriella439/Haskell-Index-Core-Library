{-# LANGUAGE GADTs, Rank2Types, TypeOperators #-}

module Control.IMonad (
    (:->),
    IFunctor(..),
    IMonad(..),
    (?>=),
    (:=)(..)
    ) where

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

    Traditional indexed monads do not permit the full-range of indexed monads
    because they fix both the initial and final index.  The following class
    generalizes those approaches by permitting actions that may begin or
    terminate in multiple indices.

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

{-|
    An infix 'bind' with arguments flipped

    Unfortunately, you cannot use ('?>=') as a substitute for (@>>=@) from
    @Control.Monad@ when using the @RebindableSyntax@ extension, for some reason
    that is not yet clear to me.  For example, the following code type-checks:

> (>>=) = (?>=)
> return = skip
>
> return 1 >>= \x -> return x

    ... but if you rebind @do@ to express the above code, it fails:

> do x <- return 1 -- This fails
>    return x

    Also, even if it did work, it would preclude the use of ('>>') completely,
    since that cannot preserve indices, so I reserve @do@ syntax for indexed
    monads where the initial index is fixed.
-}
(?>=) :: (IMonad m) => m a i -> (a :-> m b) -> m b i
(?>=) = flip bind

{-|
    @(a := i)@ represents a locked value of type @a@ that you can only access 
    at the index @i@.

    'V' seals values of type @a@, restricting them to a single index.
-}
data (a := i) j where V :: a -> (a := i) i
