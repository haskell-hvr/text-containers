{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
--
-- This module provides the 'TextArray' container for storing arrays of text strings.
--
-- This module is intended to be imported @qualified@, e.g.
--
-- > import           Data.TextArray.Unboxed (TextArr)
-- > import qualified Data.TextArray.Unboxed as TextArr
--
module Data.TextArray.Unboxed
    ( TextArray
      -- * Querying & lookup
    , null
    , length
    , elem

    , elemIndices
    -- , findIndicesOrd
    -- , findAllOrd
    , (!?)
      -- * Construction
    , empty
    , singleton
    , fromList
      -- * Deconstruction
    , toList
    ) where

import           Prelude                         ()

import           Data.TextArray.Unboxed.Internal
