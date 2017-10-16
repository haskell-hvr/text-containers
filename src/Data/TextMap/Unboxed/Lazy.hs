{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE Trustworthy  #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
--
-- This module provides the 'TextMap' container for storing maps of text keys to non-strict values.
--
-- This module is intended to be imported @qualified@, e.g.
--
-- > import           Data.TextMap.Unboxed.Lazy (TextMap)
-- > import qualified Data.TextMap.Unboxed.Lazy as TextMap
--
-- The API of this module provides value-lazy operations.
module Data.TextMap.Unboxed.Lazy
    ( TextMap
    , Key
      -- * Querying & lookup
    , size
    , null
    , member
    , lookup
    , (!?)
    , findWithDefault
    , lookupMin
    , lookupMax
    , lookupLE
    , lookupGE

      -- * Construction
    , empty
    , singleton
    , fromList
    , fromDistinctAscList
    , fromMap

      -- * Deconstruction
    , toList
    , toMap

      -- * Traversals
    , map

    ) where

import           Control.DeepSeq
import qualified Data.Foldable        as F
import qualified Data.Map.Lazy        as Map
import qualified GHC.Exts             as GHC (IsList (..))
import           Prelude              hiding (lookup, map, null)

import           Data.TextSet.Unboxed (Key, TextSet)
import qualified Data.TextSet.Unboxed as TS
import           Internal

-- | A map of (unboxed) 'ShortText' string keys to values.
--
-- The memory footprint of this data-structure is expressed in words
--
-- \[
--  8 + 2n + \left\lceil \frac{1}{w} \sum_{i=0}^{n-1} len(k_i) \right\rceil + \sum_{i=0}^{n-1} size(v_i)
-- \]
--
-- where
--
-- * the word-size \(w\) is either \(w = 4\) or \(w = 8\) bytes,
-- * \(len(k_i)\) denotes the UTF-8 size in bytes of the \(i\)-th key string, and
-- * \(size(v_i)\) denotes the allocated heap size of the \(i\)-th value in words.
--
-- __NOTE__: One word can be saved by unpacking 'TextMap' (i.e. via @{-\# UNPACK \#-}@) into another constructor.
data TextMap v = TM !TextSet !(A v)
               deriving (Eq)

instance Ord v => Ord (TextMap v) where
  compare a b = compare (toList a) (toList b)

instance Show v => Show (TextMap v) where
  showsPrec p m = showsPrec p (toList m)
  show m        = show (toList m)

instance Read v => Read (TextMap v) where
  readsPrec p = fmap (\(x,s) -> (fromList x,s)) . readsPrec p

instance GHC.IsList (TextMap v) where
  type Item (TextMap v) = (Key,v)
  fromList = fromList
  toList   = toList

instance NFData v => NFData (TextMap v) where
  rnf (TM ks vs) = ks `deepseq` vs `deepseq` ()

instance Hashable v => Hashable (TextMap v) where
  hashWithSalt s (TM ks vs) = s `hashWithSalt` ks `hashWithSalt` vs

instance Functor TextMap where
  fmap = map
  v <$ (TM ks vs) = TM ks (v <$ vs)

instance Foldable TextMap where
  foldMap f = F.foldMap f . elems
  foldr f g = F.foldr f g . elems
  toList    = elems
  null      = null
  length    = size

instance Traversable TextMap where
  traverse f (TM ks vs) = TM ks <$> arrayTraverse f vs

-- | \(\mathcal{O}(\log n)\). Lookup value for given key.
--
-- >>> fromList [("bb",False),("cc",True)] !? "cc"
-- Just True
--
-- This is a 'flip'ped infix alias of 'lookup'.
(!?) :: TextMap v -> Key -> Maybe v
(!?) = flip lookup

-- | \(\mathcal{O}(\log n)\). Lookup value for given key.
--
-- >>> lookup "a" (fromList [("bb",False),("cc",True)])
-- Nothing
--
-- >>> lookup "c" (fromList [("bb",False),("cc",True)])
-- Nothing
--
-- >>> lookup "cc" (fromList [("bb",False),("cc",True)])
-- Just True
--
-- >>> lookup "z" (fromList [("bb",False),("cc",True)])
-- Nothing
--
lookup :: Key -> TextMap v -> Maybe v
lookup k (TM ks vs) = indexArray vs <$> TS.lookupIndex k ks

-- | \(\mathcal{O}(1)\). Extract minimal key and its associated value.
--
-- >>> lookupMin empty
-- Nothing
--
-- >>> lookupMin (fromList [("a",EQ),("b",GT),("c",LT)])
-- Just ("a",EQ)
--
lookupMin :: TextMap v -> Maybe (Key,v)
lookupMin (TM ks vs) = case TS.lookupMin ks of
                         Nothing -> Nothing
                         Just k  -> Just (k,indexArray vs 0)

-- | \(\mathcal{O}(1)\). Extract maximal key and its associated value.
--
-- >>> lookupMax empty
-- Nothing
--
-- >>> lookupMax (fromList [("a",EQ),("b",GT),("c",LT)])
-- Just ("c",LT)
--
lookupMax :: TextMap v -> Maybe (Key,v)
lookupMax (TM ks vs) = case TS.lookupMax ks of
                         Nothing -> Nothing
                         Just k  -> Just (k,indexArray vs (sizeofArray vs - 1))

-- | \(\mathcal{O}(\log n)\). Lookup \"greatest\" key in map less or equal to given key and return key/value pair.
--
-- >>> lookupLE "a" (fromList [("bb",False),("cc",True)])
-- Nothing
--
-- >>> lookupLE "c" (fromList [("bb",False),("cc",True)])
-- Just ("bb",False)
--
-- >>> lookupLE "cc" (fromList [("bb",False),("cc",True)])
-- Just ("cc",True)
--
-- >>> lookupLE "z" (fromList [("bb",False),("cc",True)])
-- Just ("cc",True)
--
lookupLE :: Key -> TextMap v -> Maybe (Key,v)
lookupLE k0 (TM ks vs) = case TS.lookupLE k0 ks of
                          Nothing    -> Nothing
                          Just (i,k) -> Just (k,indexArray vs i)

-- | \(\mathcal{O}(\log n)\). Lookup \"least\" key in map less or equal to given key and return key/value pair.
--
-- >>> lookupGE "a" (fromList [("bb",False),("cc",True)])
-- Just ("bb",False)
--
-- >>> lookupGE "c" (fromList [("bb",False),("cc",True)])
-- Just ("cc",True)
--
-- >>> lookupGE "cc" (fromList [("bb",False),("cc",True)])
-- Just ("cc",True)
--
-- >>> lookupGE "z" (fromList [("bb",False),("cc",True)])
-- Nothing
--
lookupGE :: Key -> TextMap v -> Maybe (Key,v)
lookupGE k0 (TM ks vs) = case TS.lookupGE k0 ks of
                          Nothing    -> Nothing
                          Just (i,k) -> Just (k,indexArray vs i)

-- | \(\mathcal{O}(\log n)\). Lookup value for key in map or return default value if key not contained in map.
--
-- >>> findWithDefault True "z" (fromList [("bb",False),("cc",True)])
-- True
--
-- >>> findWithDefault True "bb" (fromList [("bb",False),("cc",True)])
-- False
--
-- See also 'lookup' or '!?'.
findWithDefault :: v -> Key -> TextMap v -> v
findWithDefault def k (TM ks vs) = maybe def (indexArray vs) (TS.lookupIndex k ks)

-- | \(\mathcal{O}(n)\). Convert 'Map' to 'TextMap'.
fromMap :: Map.Map Key v -> TextMap v
fromMap m0 = TM ks vs
  where
    ks = TS.fromSet (Map.keysSet m0)
    vs = arrayFromListN (TS.size ks) (Map.elems m0)

-- | \(\mathcal{O}(n)\). Convert 'TextMap' to 'Map'.
toMap :: TextMap v -> Map.Map Key v
toMap m = Map.fromDistinctAscList (toList m)

-- | \(\mathcal{O}(1)\). Extract set of all keys.
keysTextSet :: TextMap v -> TextSet
keysTextSet (TM ks _) = ks

-- | \(\mathcal{O}(\log n)\). Test whether key is present in map.
--
-- >>> member "empty" empty
-- False
--
-- >>> member "a" (fromList [("a",EQ),("b",GT),("c",LT)])
-- True
--
-- >>> member "d" (fromList [("a",EQ),("b",GT),("c",LT)])
-- False
--
member :: Key -> TextMap v -> Bool
member k m = TS.member k (keysTextSet m)

-- | \(\mathcal{O}(n)\). list all keys in ascending order.
keys :: TextMap v -> [Key]
keys m = TS.toList (keysTextSet m)

-- | \(\mathcal{O}(n)\). list all values of the map in ascending order of their keys.
elems :: TextMap v -> [v]
elems (TM _ vs) = arrayToList vs

-- | \(\mathcal{O}(n)\). list all key/value pairs of the map in ascending order of their keys.
--
-- >>> toList (fromList [("Hey",GT),("Jude",EQ),("Hey",LT),("Law",EQ),("Hey",EQ),("",EQ)])
-- [("",EQ),("Hey",EQ),("Jude",EQ),("Law",EQ)]
--
toList :: TextMap v -> [(Key,v)]
toList m = zip (keys m) (elems m)

-- | \(\mathcal{O}(n \log n)\). Construct map from list of key/value pairs.
--
-- >>> toList (fromList [("Hey",GT),("Jude",EQ),("Hey",LT),("Law",EQ),("Hey",EQ),("",EQ)])
-- [("",EQ),("Hey",EQ),("Jude",EQ),("Law",EQ)]
--
fromList :: [(Key,v)] -> TextMap v
fromList []      = empty
fromList [(k,v)] = singleton k v
fromList kvs     = fromMap (Map.fromList kvs)

-- | \(\mathcal{O}(n)\). Construct map from list with distinct keys in ascending order.
--
-- __NOTE__: If the input list is not strictly ascending, an 'error' is thrown.
fromDistinctAscList :: [(Key,v)] -> TextMap v
fromDistinctAscList as = TM ks vs
  where
    (ks0,vs0) = unzip as
    ks = TS.fromDistinctAscList      ks0
    vs = arrayFromListN (TS.size ks) vs0

-- | \(\mathcal{O}(1)\). An empty 'TextMap'.
empty :: TextMap v
empty = TM TS.empty emptyA

-- | \(\mathcal{O}(1)\). Construct map containing a single entry.
--
-- >>> toList (singleton "sa" LT)
-- [("sa",LT)]
--
singleton :: Key -> v -> TextMap v
singleton k v = TM (TS.singleton k) (arraySingleton v)

-- | \(\mathcal{O}(1)\). Report number of entries in map.
--
-- >>> size empty
-- 0
--
-- >>> size (singleton "sa" LT)
-- 1
--
-- >>> size (fromList [("a",True),("b",False),("a",False)])
-- 2
--
size :: TextMap v -> Int
size (TM _ vs) = sizeofArray vs

-- | \(\mathcal{O}(1)\). Test whether map is 'empty'.
--
-- >>> null empty
-- True
--
-- >>> null (singleton "" ())
-- False
--
null :: TextMap v -> Bool
null m = size m == 0

-- | \(\mathcal{O}(n)\). Apply function to values in map.
--
-- >>> toList (map not (fromList [("a",True),("b",False),("a",False)]))
-- [("a",True),("b",True)]
--
-- This is a specialised version of 'fmap'.
map :: (v->w) -> TextMap v -> TextMap w
map f (TM ks vs) = TM ks (arrayMap f vs)

-- $setup
-- >>> :set -XOverloadedStrings
