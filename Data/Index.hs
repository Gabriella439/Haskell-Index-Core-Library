-- | This module provides the core (':->') type operator which demonstrates the
--   parallel between the world of indexed types and the world of unindexed
--   types.
--
--   You can use this type operator with the following extensions:
--
-- > {-# LANGUAGE TypeOperators, Rank2Types #-}
--
--   However, even if you don't use it, you may still need @Rank2Types@ anyway
--   to express index-preserving functions.

{-# LANGUAGE TypeOperators, Rank2Types #-}

module Data.Index (
    -- * Index-Preserving Functions
    -- $index
    (:->)
    ) where

{- $index
    (':->') defines an indexed category.  This category permits almost
    mechanical translations of ordinary types to indexed types where you
    selectively replace certain (@->@) arrows with (':->') arrows.

    Index-preserving functions share the same composition ('.') and identity
    ('id') as ordinary functions.
-}

-- | An index-preserving function from @a@ to @b@
type a :-> b = forall i . a i -> b i
