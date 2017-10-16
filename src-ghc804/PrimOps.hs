{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Unsafe    #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
module PrimOps (compareByteArrays#) where

import           GHC.Prim (compareByteArrays#)
