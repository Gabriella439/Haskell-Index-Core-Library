-- | This module provides the core (':->') type operator, which links the world
--   of indexed types and the world of unindexed types.
--
--   You can use this type operator with the following extension:
--
-- > {-# LANGUAGE TypeOperators #-}
--
--   Sometimes you may also need the @Rank2Types@ extension.

{-# LANGUAGE TypeOperators, Rank2Types #-}

module Control.Category.Index (
    -- * Index-Preserving Functions
    -- $index
    (:->)
    ) where

import Control.Category

{- $index
    (':->') defines an indexed 'Category'.  This 'Category' permits almost
    mechanical translations of ordinary types to indexed types where you
    selectively replace certain (@->@) arrows with (':->') arrows.

    Index-preserving functions share the same composition ('.') and identity
    ('id') as ordinary functions.  If (':->') were a distinct type instead of a
    synonym, you would define:

> instance Category (:->) where
>     id x = x            -- the same definition for the (->) Category
>     (f . g) x = f (g x) -- the same definition for the (->) Category

    Fortunately, that's not necessary since it will correctly use the 'Category'
    instance for (@->@).
-}

-- | An index-preserving function from @a@ to @b@
type a :-> b = forall i . a i -> b i
