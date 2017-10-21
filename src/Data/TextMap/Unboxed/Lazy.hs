{-# LANGUAGE BangPatterns #-}
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
    , fromTextSet

      -- * Deconstruction
    , toList
    , toMap
    , keysTextSet

      -- * Set operations
    , unionWith
    , unionWith'
    , intersectionWith
    , difference
    , sdifference
    , sdifferenceWith

    , restrictKeys
    , withoutKeys

      -- * Traversals
    , map

    ) where

import           Control.DeepSeq
import qualified Data.Foldable                 as F
import qualified Data.Map.Lazy                 as Map
import qualified GHC.Exts                      as GHC (IsList (..))
import           Prelude                       hiding (lookup, map, null)

import           Data.TextSet.Unboxed          (Key, TextSet)
import           Data.TextSet.Unboxed.Internal (LR (..))
import qualified Data.TextSet.Unboxed.Internal as TS

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

-- | Combines 'TextMap's via @'unionWith' ('<>')@
--
-- @since 0.1.1.0
instance Semigroup v => Semigroup (TextMap v) where
  (<>) = unionWith (<>)

-- | @since 0.1.1.0
instance Semigroup v => Monoid (TextMap v) where
  mempty  = empty
  mappend = (<>)

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
--
-- @since 0.1.1.0
keysTextSet :: TextMap v -> TextSet
keysTextSet (TM ks _) = ks

-- | \(\mathcal{O}(n)\). Build a 'TextMap' from a 'TextSet' of keys.
--
-- @since 0.1.1.0
fromTextSet :: (Key -> v) -> TextSet -> TextMap v
fromTextSet f ks = TM ks vs
  where
    vs = arrayFromListN (TS.size ks) (fmap f (TS.toList ks))

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

-- | \(\mathcal{O}(m+n)\). Union of two 'TextMap's with combining function for common keys.
--
-- See also the 'Semigroup' instance of 'TextMap' which uses @'unionWith' ('<>')@.
--
-- You can use @'unionWith' 'const'@ or @'unionWith' ('flip' 'const')@ to get a left or right biased merge respectively.
--
-- @since 0.1.1.0
unionWith :: (a -> a -> a) -> TextMap a -> TextMap a -> TextMap a
unionWith _ l0 r0
  | null l0 = r0
  | null r0 = l0
unionWith f l0 r0 = unionWith' id id f l0 r0

-- | \(\mathcal{O}(m+n)\). Union of two 'TextMap's with combining functions.
--
-- This is a more general version of 'unionWith' (which is
-- semantically equivalent to @'unionWith'' 'id' 'id'@) allowing for
-- computing the union of 'TextMap's with different value types.
--
-- @since 0.1.1.0
unionWith' :: (a -> c) -> (b -> c) -> (a -> b -> c) -> TextMap a -> TextMap b -> TextMap c
unionWith' _  fr _ l0             r0   | null l0 = fmap fr r0
unionWith' fl _  _ l0             r0   | null r0 = fmap fl l0
unionWith' fl fr f (TM l0ks l0vs) (TM r0ks r0vs) = TM ks' vs'
  where
    ks' = TS.fromOfsLen res_cnt res_sz (goK (TS.lraToList lrs0) (TS.listOfsLen l0ks) (TS.listOfsLen r0ks))

    vs' = arrayFromListN res_cnt (goV (TS.lraToList lrs0) 0 0)

    -- TODO: verify (lraToList lrs0) isn't CSE'd

    goK []       []             []             = []
    goK (L:lrs)  (l_ofs:l_ofss) r_ofss         = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (LR:lrs) (l_ofs:l_ofss) (_:r_ofss)     = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (R:lrs)  l_ofss         (r_ofs:r_ofss) = (r0ks,r_ofs) : goK lrs l_ofss r_ofss
    goK _        _              _              = error "TextMap.unionWith: the impossible happened"


    goV []       !li !ri = assert (li == (l_cnt+lr_cnt) && ri == (r_cnt+lr_cnt)) []
    goV (L:lrs)  !li !ri = fl (indexArray l0vs li)                   : goV lrs (li+1)  ri
    goV (LR:lrs) !li !ri = indexArray l0vs li `f` indexArray r0vs ri : goV lrs (li+1) (ri+1)
    goV (R:lrs)  !li !ri = fr (indexArray r0vs ri)                   : goV lrs  li    (ri+1)

    TS.SetOpRes lrs0 (CS l_cnt l_sz) (CS lr_cnt lr_sz) (CS r_cnt r_sz) = TS.genSetOp l0ks r0ks

    res_cnt = l_cnt + lr_cnt + r_cnt
    res_sz  = l_sz  + lr_sz  + r_sz

-- | \(\mathcal{O}(m+n)\). Intersection of two 'TextMap's with a combining function for common keys.
--
-- Commonly used combining functions:
--
-- [@('<>')@] for combining values via their 'Semigroup' operation
--
-- ['const'] for a left-biased merge
--
-- [@('flip' 'const')@] for a right-biased merge
--
-- @since 0.1.1.0
intersectionWith :: (a -> b -> c) -> TextMap a -> TextMap b -> TextMap c
intersectionWith _ l0 r0
  | null l0 = empty
  | null r0 = empty
intersectionWith f (TM l0ks l0vs) (TM r0ks r0vs) = TM ks' vs'
  where
    ks' = TS.fromOfsLen res_cnt res_sz (goK (TS.lraToList lrs0) (TS.listOfsLen l0ks) (TS.listOfsLen r0ks))

    vs' = arrayFromListN res_cnt (goV (TS.lraToList lrs0) 0 0)

    -- TODO: verify (lraToList lrs0) isn't CSE'd

    goK []       []             []             = []
    goK (L:lrs)  (_:l_ofss)     r_ofss         =                goK lrs l_ofss r_ofss
    goK (LR:lrs) (l_ofs:l_ofss) (_:r_ofss)     = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (R:lrs)  l_ofss         (_:r_ofss)     =                goK lrs l_ofss r_ofss
    goK _        _              _              = error "TextMap.intersectionWith: the impossible happened"


    goV []       !li !ri = assert (li == (l_cnt+lr_cnt) && ri == (r_cnt+lr_cnt)) []
    goV (L:lrs)  !li !ri =                                             goV lrs (li+1)  ri
    goV (LR:lrs) !li !ri = indexArray l0vs li `f` indexArray r0vs ri : goV lrs (li+1) (ri+1)
    goV (R:lrs)  !li !ri =                                             goV lrs  li    (ri+1)

    TS.SetOpRes lrs0 (CS l_cnt _) (CS lr_cnt lr_sz) (CS r_cnt _) = TS.genSetOp l0ks r0ks

    res_cnt = lr_cnt
    res_sz  = lr_sz

-- | \(\mathcal{O}(m+n)\). Difference of two 'TextMap's.
--
-- @'difference' l r = 'withoutKeys' l ('keysTextSet' r)@
--
-- @since 0.1.1.0
difference :: TextMap a -> TextMap b -> TextMap a
difference l r = restrictKeys l (keysTextSet r)

-- | \(\mathcal{O}(m+n)\). Symmetric difference of two 'TextMap's.
--
-- @'sdifference' l r = 'withoutKeys' l ('keysTextSet' l '<>' 'keysTextSet' r)@
--
-- @since 0.1.1.0
sdifference :: TextMap a -> TextMap a -> TextMap a
sdifference l0 r0
  | null l0 = r0
  | null r0 = l0
sdifference l0 r0 = sdifferenceWith id id l0 r0

-- | \(\mathcal{O}(m+n)\). Symmetric difference of two 'TextMap's.
--
-- This is a more general version of 'sdifference' supporting
-- combining maps with different value types.
--
-- @since 0.1.1.0
sdifferenceWith :: (a -> c) -> (b -> c) -> TextMap a -> TextMap b -> TextMap c
sdifferenceWith _  fr l0             r0   | null l0 = fmap fr r0
sdifferenceWith fl _  l0             r0   | null r0 = fmap fl l0
sdifferenceWith fl fr (TM l0ks l0vs) (TM r0ks r0vs) = TM ks' vs'
  where
    ks' = TS.fromOfsLen res_cnt res_sz (goK (TS.lraToList lrs0) (TS.listOfsLen l0ks) (TS.listOfsLen r0ks))

    vs' = arrayFromListN res_cnt (goV (TS.lraToList lrs0) 0 0)

    -- TODO: verify (lraToList lrs0) isn't CSE'd

    goK []       []             []             = []
    goK (L:lrs)  (l_ofs:l_ofss) r_ofss         = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (LR:lrs) (_ :l_ofss)    (_:r_ofss)     =                goK lrs l_ofss r_ofss
    goK (R:lrs)  l_ofss         (r_ofs:r_ofss) = (r0ks,r_ofs) : goK lrs l_ofss r_ofss
    goK _        _              _              = error "TextMap.sdifferenceWith: the impossible happened"


    goV []       !li !ri = assert (li == (l_cnt+lr_cnt) && ri == (r_cnt+lr_cnt)) []
    goV (L:lrs)  !li !ri = fl (indexArray l0vs li)                   : goV lrs (li+1)  ri
    goV (LR:lrs) !li !ri =                                             goV lrs (li+1) (ri+1)
    goV (R:lrs)  !li !ri = fr (indexArray r0vs ri)                   : goV lrs  li    (ri+1)

    TS.SetOpRes lrs0 (CS l_cnt l_sz) (CS lr_cnt _) (CS r_cnt r_sz) = TS.genSetOp l0ks r0ks

    res_cnt = l_cnt + r_cnt
    res_sz  = l_sz  + r_sz

-- | \(\mathcal{O}(m+n)\). Restrict 'TextMap' to keys contained in 'TestSet'.
--
-- @since 0.1.1.0
restrictKeys :: TextMap a -> TextSet -> TextMap a
restrictKeys l0 !r0
  | null l0    = empty
  | TS.null r0 = empty
restrictKeys (TM l0ks l0vs) r0ks = TM ks' vs'
  where
    ks' = TS.fromOfsLen res_cnt res_sz (goK (TS.lraToList lrs0) (TS.listOfsLen l0ks) (TS.listOfsLen r0ks))

    vs' = arrayFromListN res_cnt (goV (TS.lraToList lrs0) 0)

    -- TODO: verify (lraToList lrs0) isn't CSE'd

    goK []       []             []             = []
    goK (L:lrs)  (_:l_ofss)     r_ofss         =                goK lrs l_ofss r_ofss
    goK (LR:lrs) (l_ofs:l_ofss) (_:r_ofss)     = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (R:lrs)  l_ofss         (_:r_ofss)     =                goK lrs l_ofss r_ofss
    goK _        _              _              = error "TextMap.restrictKeys: the impossible happened"


    goV []       !li = assert (li == (l_cnt+lr_cnt)) []
    goV (L:lrs)  !li =                      goV lrs (li+1)
    goV (LR:lrs) !li = indexArray l0vs li : goV lrs (li+1)
    goV (R:lrs)  !li =                      goV lrs  li

    TS.SetOpRes lrs0 (CS l_cnt _) (CS lr_cnt lr_sz) _ = TS.genSetOp l0ks r0ks

    res_cnt = lr_cnt
    res_sz  = lr_sz


-- | \(\mathcal{O}(m+n)\). Remove keys from 'TextMap' which are contained in 'TextSet'.
--
-- @since 0.1.1.0
withoutKeys :: TextMap a -> TextSet -> TextMap a
withoutKeys l0 !r0
  | null l0    = empty
  | TS.null r0 = l0
withoutKeys (TM l0ks l0vs) r0ks = TM ks' vs'
  where
    ks' = TS.fromOfsLen res_cnt res_sz (goK (TS.lraToList lrs0) (TS.listOfsLen l0ks) (TS.listOfsLen r0ks))

    vs' = arrayFromListN res_cnt (goV (TS.lraToList lrs0) 0)

    -- TODO: verify (lraToList lrs0) isn't CSE'd

    goK []       []             []             = []
    goK (L:lrs)  (l_ofs:l_ofss)     r_ofss     = (l0ks,l_ofs) : goK lrs l_ofss r_ofss
    goK (LR:lrs) (_:l_ofss)     (_:r_ofss)     =                goK lrs l_ofss r_ofss
    goK (R:lrs)  l_ofss         (_:r_ofss)     =                goK lrs l_ofss r_ofss
    goK _        _              _              = error "TextMap.restrictKeys: the impossible happened"


    goV []       !li = assert (li == (l_cnt+lr_cnt)) []
    goV (L:lrs)  !li = indexArray l0vs li : goV lrs (li+1)
    goV (LR:lrs) !li =                      goV lrs (li+1)
    goV (R:lrs)  !li =                      goV lrs  li

    TS.SetOpRes lrs0 (CS l_cnt l_sz) (CS lr_cnt _) _ = TS.genSetOp l0ks r0ks

    res_cnt = l_cnt
    res_sz  = l_sz


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
