{-|
    This module provides a common interface to both indexed monads and
    restricted monads.  I split them into two separate modules for
    organizational purposes and to simplify the presentation of each type of
    indexed monad.
-}
module Control.IMonad (
    -- * Modules
    module Control.IMonad.Core,
    module Control.IMonad.Restrict
    ) where

import Control.IMonad.Core
import Control.IMonad.Restrict
