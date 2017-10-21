{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
--
-- This module provides the 'TextSet' container for storing sets of text strings.
--
-- This module is intended to be imported @qualified@, e.g.
--
-- > import           Data.TextSet.Unboxed (TextSet)
-- > import qualified Data.TextSet.Unboxed as TextSet
--
module Data.TextSet.Unboxed
    ( TextSet
    , Key

      -- * Querying & lookup
    , size
    , null
    , member
    , lookupMin
    , lookupMax
    , lookupLE
    , lookupGE

    , (!?)
    , lookupIndex

      -- * Construction
    , empty
    , singleton
    , fromList
    , fromDistinctAscList
    , fromSet

      -- * Deconstruction
    , toList
    , toArray
    , toSet

      -- * Set operations
    , union
    , intersection
    , difference
    , sdifference

    , isSubsetOf
    , isProperSubsetOf

    ) where

import           Data.TextSet.Unboxed.Internal
import           Prelude                       ()
