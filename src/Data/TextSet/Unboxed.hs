{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeFamilies               #-}

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

import           Control.DeepSeq
import           Data.Bits
import qualified Data.List                       as List
import qualified Data.Set                        as Set
import qualified GHC.Exts                        as GHC
import           Prelude                         hiding (null)

import           Data.TextArray.Unboxed          (TextArray)
import qualified Data.TextArray.Unboxed.Internal as TA
import           Internal

-- | A set of unboxed 'ShortText' strings
--
-- The memory footprint of this data-structure is a single heap object (an unlifted 'ByteArray#') with the size expressed in words
--
-- \[
--  3 + n + \left\lceil \frac{1}{w} \sum_{i=0}^{n-1} len(s_i) \right\rceil
-- \]
--
-- where the word-size \(w\) is either \(w = 4\) or \(w = 8\) bytes; and where \(len(s_i)\) denotes the UTF-8 size in bytes of the \(i\)-th text string.
--
-- NOTE: Depending on whether you @UNPACK@ the 'TextSet' wrapper, you need at least one additional word for the pointer to the internal 'ByteArray#' heap object.
newtype TextSet = TS TextArray
                deriving (Eq,Ord,NFData,Hashable)

type Key = ShortText

instance Show TextSet where
  showsPrec p (TS ta) = showsPrec p ta
  show (TS ta)        = show ta

instance Read TextSet where
  readsPrec p = map (\(x,s) -> (TS x,s)) . readsPrec p

instance GHC.IsList TextSet where
  type Item TextSet = Key
  fromList = fromList
  toList   = toList

-- | 'Semigroup' over 'union' operation.
instance Semigroup TextSet where
  (<>) = union

-- | 'Monoid' over 'union' operation.
instance Monoid TextSet where
  mempty  = empty
  mappend = (<>)

-- | \(\mathcal{O}(1)\). An empty 'TextSet'.
empty :: TextSet
empty = TS TA.empty

-- | \(\mathcal{O}(1)\). Report number of elements in 'TextSet'.
--
-- >>> size empty
-- 0
--
-- >>> size (singleton "")
-- 1
--
-- >>> size (fromList ["Hey","Hey","Jude"])
-- 2
--
size :: TextSet -> Int
size (TS ta) = TA.length ta

-- | \(\mathcal{O}(1)\). Check for 'empty' set.
--
-- >>> null empty
-- True
--
-- >>> null (singleton "")
-- False
--
null :: TextSet -> Bool
null (TS ta) = TA.null ta


-- | \(\mathcal{O}(1)\). Construct set containing one element.
--
-- >>> toList (singleton "alone")
-- ["alone"]
--
singleton :: Key -> TextSet
singleton = TS . TA.singleton

-- | \(\mathcal{O}(n \log n)\). Construct set from list of elements.
--
-- >>> toList (fromList ["Hey","Jude","Hey","Law","Hey",""])
-- ["","Hey","Jude","Law"]
--
fromList :: [Key] -> TextSet
fromList []  = empty
fromList [x] = singleton x
fromList xs  = fromSet (Set.fromList xs)

-- | \(\mathcal{O}(n)\). Construct set from list of distinct elements in ascending order.
--
-- __NOTE__: If the input list is not strictly ascending, an 'error' is thrown.
fromDistinctAscList :: [Key] -> TextSet
fromDistinctAscList [] = empty
fromDistinctAscList xs
  | isMonotonic xs = TS (TA.fromList xs)
  | otherwise      = error "fromDistinctAscList: invalid argument"

isMonotonic :: [Key] -> Bool
isMonotonic [] = True
isMonotonic [_] = True
isMonotonic (x:xs) = go x xs
  where
    go _ [] = True
    go y (y':ys)
      | y < y'    = go y' ys
      | otherwise = False

-- | \(\mathcal{O}(n)\). Convert 'Set' to 'TextSet'.
fromSet :: Set.Set Key -> TextSet
fromSet = TS . TA.fromList . Set.toAscList

-- | \(\mathcal{O}(n)\). Convert 'TextSet' to 'Set'.
toSet :: TextSet -> Set.Set Key
toSet = Set.fromDistinctAscList . toList

-- | \(\mathcal{O}(n)\). Convert 'TextSet' to list of 'ShortText' in ascending order.
toList :: TextSet -> [Key]
toList (TS ta) = TA.toList ta

-- | \(\mathcal{O}(1)\). Convert 'TextSet' to 'TextArray' of 'ShortText' in ascending order.
--
-- >>> toList (fromList ["Hey","Jude","Hey","Law","Hey",""])
-- ["","Hey","Jude","Law"]
--
toArray :: TextSet -> TextArray
toArray (TS ta) = ta

-- | \(\mathcal{O}(1)\). Extract minimal element from set.
--
-- >>> lookupMin empty
-- Nothing
--
-- >>> lookupMin (fromList ["a","b","c"])
-- Just "a"
--
lookupMin :: TextSet -> Maybe Key
lookupMin (TS ta) = ta TA.!? 0

-- | \(\mathcal{O}(1)\). Extract maximal element from set.
--
-- >>> lookupMax empty
-- Nothing
--
-- >>> lookupMax (fromList ["a","b","c"])
-- Just "c"
--
lookupMax :: TextSet -> Maybe Key
lookupMax (TS ta) = ta TA.!? (TA.length ta - 1)


----------------------------------------------------------------------------
-- lookup

-- | \(\mathcal{O}(\log n)\). Test whether set contains a string.
--
-- >>> member "empty" empty
-- False
--
-- >>> member "a" (fromList ["a","b","c"])
-- True
--
-- >>> member "d" (fromList ["a","b","c"])
-- False
--
member :: Key -> TextSet -> Bool
member t ts = case lookupIndexNear LBelow t ts of
                LResExact _ -> True
                _           -> False


-- | \(\mathcal{O}(1)\). Retrieve \(i\)-th element in the sorted sequence of elements.
--
-- >>> fromList ["Hey","","Jude"] !? 0
-- Just ""
--
-- >>> fromList ["Hey","","Jude"] !? 1
-- Just "Hey"
--
-- >>> fromList ["Hey","","Jude"] !? 3
-- Nothing
--
-- See also 'lookupIndex'.
(!?) :: TextSet -> Int -> Maybe Key
(TS ta) !? i = ta TA.!? i

-- | \(\mathcal{O}(\log n)\). Look up element in set and report its zero-based index in the sorted sequence elements.
--
-- >>> lookupIndex "" (fromList ["Hey","","Jude"])
-- Just 0
--
-- >>> lookupIndex "Hey" (fromList ["Hey","","Jude"])
-- Just 1
--
-- >>> lookupIndex "Law" (fromList ["Hey","","Jude"])
-- Nothing
--
-- See also '!?'.
lookupIndex :: Key -> TextSet -> Maybe Int
lookupIndex t ts = case lookupIndexNear LBelow t ts of
                     LResExact (IdxOfsLen i _ _) -> Just i
                     _                           -> Nothing

mkist :: TextSet -> IdxOfsLen -> (Int, Key)
mkist (TS ta) (IdxOfsLen i ofs n) = (i, ba2st (sliceBA (TA.ta2ba ta) ofs n))

-- TODO: lookupLT lookupGT

-- | \(\mathcal{O}(\log n)\). Look up \"greatest\" string (together with its index) in set less or equal to given string.
--
-- >>> lookupLE "a" (fromList ["bb","cc"])
-- Nothing
--
-- >>> lookupLE "c" (fromList ["bb","cc"])
-- Just (0,"bb")
--
-- >>> lookupLE "cc" (fromList ["bb","cc"])
-- Just (1,"cc")
--
-- >>> lookupLE "z" (fromList ["bb","cc"])
-- Just (1,"cc")
--
lookupLE :: Key -> TextSet -> Maybe (Int,Key)
lookupLE t ts = case lookupIndexNear LAbove t ts of
                  LResAbove res -> Just (mkist ts res)
                  LResExact res -> Just (mkist ts res)
                  _             -> Nothing

-- | \(\mathcal{O}(\log n)\). Look up \"least\" string (together with its index) in set greater or equal to given string.
--
-- >>> lookupGE "a" (fromList ["bb","cc"])
-- Just (0,"bb")
--
-- >>> lookupGE "c" (fromList ["bb","cc"])
-- Just (1,"cc")
--
-- >>> lookupGE "cc" (fromList ["bb","cc"])
-- Just (1,"cc")
--
-- >>> lookupGE "z" (fromList ["bb","cc"])
-- Nothing
--
lookupGE :: Key -> TextSet -> Maybe (Int,Key)
lookupGE t ts = case lookupIndexNear LBelow t ts of
                  LResBelow res -> Just (mkist ts res)
                  LResExact res -> Just (mkist ts res)
                  _             -> Nothing

data LRes = LResEmpty              -- empty set
          | LResExact   !IdxOfsLen -- exact match
          | LResBelow   !IdxOfsLen -- inexact match, needle is /below/ IdxOfsLen entry
          | LResAbove   !IdxOfsLen -- inexact match, needle is /above/ IdxOfsLen entry
          deriving Show

-- | associativity of 'lookupIndexNear'
data LMode = LBelow -- if between two entries, return 'LResBelow'
           | LAbove -- if between two entries, return 'LResAbove'

-- | Find near entry
--
-- NB: the implementation of 'lookupIndex{L,R}' differs only in two places, marked with (I) and (II) comments
lookupIndexNear :: LMode -> Key -> TextSet -> LRes
lookupIndexNear mode x (TS ta)
  | ui0 <   0 = LResEmpty
  | otherwise = case cmpBA lv0 of
      LT -> LResBelow lv0
      EQ -> LResExact lv0
      GT -> case cmpBA uv0 of
              GT -> LResAbove uv0
              EQ -> LResExact uv0
              LT -> case mode of
                      LAbove -> goL li0 ui0 -- invariant: lv0 < v < uv0   && li < ui
                      LBelow -> goR li0 ui0 -- invariant: lv0 < v < uv0   && li < ui
  where
    li0 = 0
    ui0 = TA.length ta - 1

    lv0, uv0 :: IdxOfsLen
    lv0 = TA.indexOfsLen' ta li0
    uv0 = TA.indexOfsLen' ta ui0

    cmpBA = cmpBA2OfsLen (st2ba x) (TA.ta2ba ta)

    -----------------------------------------------

    goL :: Int -> Int -> LRes
    goL li ui
      | assert (li < ui) False = undefined -- invariant: li < ui
      | mi == li  = LResAbove mv -- closest match -- (I)
      | otherwise = case cmpBA mv of
                      LT -> goL li mi -- go left
                      EQ -> LResExact mv -- exact match
                      GT -> goL mi ui -- go right
      where
        mi = mid li ui
        mv = TA.indexOfsLen' ta mi

        mid :: Int -> Int -> Int
        mid li' ui' = li' + quot (ui'-li') 2 -- (II)

    goR :: Int -> Int -> LRes
    goR li ui
      | assert (li < ui) False = undefined -- invariant: li < ui
      | mi == ui  = LResBelow mv -- closest match -- (I)
      | otherwise = case cmpBA mv of
                      LT -> goR li mi    -- go left
                      EQ -> LResExact mv -- exact match
                      GT -> goR mi ui    -- go right
      where
        mi = mid li ui
        mv = TA.indexOfsLen' ta mi

        mid :: Int -> Int -> Int
        mid li' ui' = ui' - quot (ui'-li') 2 -- (II)

-- | See 'SetOpRes'
genSetOp :: TextSet -> TextSet -> SetOpRes
genSetOp l r = lraFromListN (size l + size r) (computeLRs' l r)

-- | \(\mathcal{O}(m+n)\). Intersection ( \(\cap\) ) of two 'TextSet's.
intersection :: TextSet -> TextSet -> TextSet
intersection l0 r0
  | null l0 = empty
  | null r0 = empty
  | size l0 > size r0 = intersection r0 l0
intersection l0@(TS l0ta) r0 = TS $ TA.fromOfsLens res_cnt res_sz (go (lraToList lrs0) l0_ofss)
  where
    go []       []              = []
    go (L:lrs)  (_:l_ofss)      =                go lrs l_ofss
    go (LR:lrs) (l_ofs:l_ofss)  = (l0ba,l_ofs) : go lrs l_ofss
    go (R:lrs)  l_ofss          =                go lrs l_ofss
    go _        _               = error "TextSet.intersection: the impossible happened"

    SetOpRes lrs0 _ (CS lr_cnt lr_sz) _ = genSetOp l0 r0

    res_cnt = lr_cnt
    res_sz  = lr_sz

    l0_ofss = TA.listOfsLen l0ta

    l0ba = TA.ta2ba l0ta

-- | \(\mathcal{O}(m+n)\). Set-difference ( \(\setminus\) ) of two 'TextSet's.
difference :: TextSet -> TextSet -> TextSet
difference l0 r0
  | null l0 = empty
  | null r0 = l0
difference l0@(TS l0ta) r0 = TS $ TA.fromOfsLens res_cnt res_sz (go (lraToList lrs0) l0_ofss)
  where
    go []       []              = []
    go (L:lrs)  (l_ofs:l_ofss)  = (l0ba,l_ofs) : go lrs l_ofss
    go (LR:lrs) (_:l_ofss)      =                go lrs l_ofss
    go (R:lrs)  l_ofss          =                go lrs l_ofss
    go _        _               = error "TextSet.difference: the impossible happened"

    SetOpRes lrs0 (CS l_cnt l_sz) _ _ = genSetOp l0 r0

    res_cnt = l_cnt
    res_sz  = l_sz

    l0_ofss = TA.listOfsLen l0ta

    l0ba = TA.ta2ba l0ta

-- | \(\mathcal{O}(m+n)\). Union ( \(\cup\) ) of two 'TextSet's.
union :: TextSet -> TextSet -> TextSet
union l0 r0
  | null l0 = r0
  | null r0 = l0
union l0@(TS l0ta) r0@(TS r0ta) = TS $ TA.fromOfsLens res_cnt res_sz (go (lraToList lrs0) l0_ofss r0_ofss)
  where
    go []       []             []             = []
    go (L:lrs)  (l_ofs:l_ofss) r_ofss         = (l0ba,l_ofs) : go lrs l_ofss r_ofss
    go (LR:lrs) (l_ofs:l_ofss) (_:r_ofss)     = (l0ba,l_ofs) : go lrs l_ofss r_ofss
    go (R:lrs)  l_ofss         (r_ofs:r_ofss) = (r0ba,r_ofs) : go lrs l_ofss r_ofss
    go _        _              _              = error "TextSet.union: the impossible happened"

    SetOpRes lrs0 (CS l_cnt l_sz) (CS lr_cnt lr_sz) (CS r_cnt r_sz) = genSetOp l0 r0

    res_cnt = l_cnt + lr_cnt + r_cnt
    res_sz  = l_sz  + lr_sz  + r_sz

    l0_ofss = TA.listOfsLen l0ta
    r0_ofss = TA.listOfsLen r0ta

    l0ba = TA.ta2ba l0ta
    r0ba = TA.ta2ba r0ta

-- | \(\mathcal{O}(m+n)\). Symmetric difference ( \(\triangle\) ) (aka /disjunctive union/) of two 'TextSet's, i.e.
-- \( A \triangle B = (A \setminus B) \cup (B \setminus A) \).
--
sdifference :: TextSet -> TextSet -> TextSet
sdifference l0 r0
  | null l0 = r0
  | null r0 = l0
sdifference l0@(TS l0ta) r0@(TS r0ta) = TS $ TA.fromOfsLens res_cnt res_sz (go (lraToList lrs0) l0_ofss r0_ofss)
  where
    go []       []             []             = []
    go (L:lrs)  (l_ofs:l_ofss) r_ofss         = (l0ba,l_ofs) : go lrs l_ofss r_ofss
    go (LR:lrs) (_:l_ofss)     (_:r_ofss)     =                go lrs l_ofss r_ofss
    go (R:lrs)  l_ofss         (r_ofs:r_ofss) = (r0ba,r_ofs) : go lrs l_ofss r_ofss
    go _        _              _              = error "TextSet.sdifference: the impossible happened"

    SetOpRes lrs0 (CS l_cnt l_sz) _ (CS r_cnt r_sz) = genSetOp l0 r0

    res_cnt = l_cnt + r_cnt
    res_sz  = l_sz  + r_sz

    l0_ofss = TA.listOfsLen l0ta
    r0_ofss = TA.listOfsLen r0ta

    l0ba = TA.ta2ba l0ta
    r0ba = TA.ta2ba r0ta


-- | Encodes an intermediate set-delta (into 3 disjoint sets) from
-- which the usual set operations (union, intersection, difference,
-- sym. difference) can easily be computed
data SetOpRes = SetOpRes !BA !CntSize !CntSize !CntSize

-- | Represent whether an element is contained in "left" set 'L', "right" set 'R', or both, 'LR'.
--
-- This gets encoded as 0b10 0b11 and 0b01 respectively.
data LR = L | LR | R
        deriving (Eq,Show)

-- | \(\mathcal{O}(m+n)\). Proper-subset ( \( \subset \) ) predicate.
isProperSubsetOf :: TextSet -> TextSet -> Bool
isProperSubsetOf l r = (size l < size r) && l `isSubsetOf` r

-- | \(\mathcal{O}(m+n)\). Subset ( \( \subseteq \) ) predicate.
isSubsetOf :: TextSet -> TextSet -> Bool
isSubsetOf l0@(TS tal) r0@(TS tar) = (size l0 <= size r0) && go iolL iolR
  where
    go []     _  = True
    go (_:_)  [] = False
    go (l:ls) (r:rs) = case cmp l r of
                         LT -> False
                         EQ -> go ls rs
                         GT -> go (l:ls) rs

    iolL = TA.listOfsLen tal
    iolR = TA.listOfsLen tar

    cmp x y = cmpOfsLens (TA.ta2ba tal) x (TA.ta2ba tar) y

----------------------------------------------------------------------------
-- helpers

computeLRs' :: TextSet -> TextSet -> [(Int,LR)]
computeLRs' (TS tal) (TS tar) = go iolL iolR
  where
    go [] []     = []
    go [] (r:rs) = (len r,R) : go [] rs
    go (l:ls) [] = (len l,L) : go ls []

    go (l:ls) (r:rs) = case cmp l r of
                         LT -> (len l,L)  : go ls (r:rs)
                         EQ -> (len l,LR) : go ls rs
                         GT -> (len r,R)  : go (l:ls) rs

    iolL = TA.listOfsLen tal
    iolR = TA.listOfsLen tar

    cmp x y = cmpOfsLens (TA.ta2ba tal) x (TA.ta2ba tar) y

    len (IdxOfsLen _ _ n) = n

lr2bits :: Int -> LR -> Word8
lr2bits i lr
  = case i of
      0 -> case lr of
             L  -> 0b10
             LR -> 0b11
             R  -> 0b01
      1 -> case lr of
             L  -> 0b1000
             LR -> 0b1100
             R  -> 0b0100
      2 -> case lr of
             L  -> 0b100000
             LR -> 0b110000
             R  -> 0b010000
      3 -> case lr of
             L  -> 0b10000000
             LR -> 0b11000000
             R  -> 0b01000000
      _ -> undefined -- impossible

bits2lr :: Int -> Word8 -> Maybe LR
bits2lr i w = assert (0 <= i && i < 4) $
  case (w `shiftR` (i*2)) .&. 0b11 of
                0b10 -> Just L
                0b11 -> Just LR
                0b01 -> Just R
                0b00 -> Nothing
                _    -> undefined

lraFromListN :: Int-> [(Int,LR)] -> SetOpRes
lraFromListN sz lrs = mkRes $ createBA' bsz $ \mba -> do
    let -- go :: Int -> Word8 -> [LR] -> ST s ()
        go !j !lenL !lenLR !lenR !_   []     = assert (j <= sz) $ do
          let bsz' = 1+((j-1) `div` 4)
          unless (bsz' == bsz) $
            assert (bsz' < bsz) $ shrinkMutableByteArray mba bsz'

          pure (lenL,lenLR,lenR)
        go !j !lenL !lenLR !lenR !acc ((len,x):xs) = assert (j <  sz) $ do
          let bidx  = j `unsafeShiftR` 2
          let bidx2 = j .&. 0b11
          let acc' | bidx2 == 0  =         lr2bits bidx2 x
                   | otherwise   = acc .|. lr2bits bidx2 x

          when ((bidx2 == 3) || List.null xs) $
            assert (bidx < bsz) $ writeWord8Array mba bidx acc'

          case x of
            L  -> go (j+1) (lenL^+len) lenLR       lenR        acc' xs
            LR -> go (j+1) lenL       (lenLR^+len) lenR        acc' xs
            R  -> go (j+1) lenL        lenLR       (lenR^+len) acc' xs
    go 0 cs0 cs0 cs0 0 lrs
  where
    bsz = 1+((sz-1) `div` 4)

    cs0 = CS 0 0

    mkRes (ba,(l,lr,r)) = SetOpRes ba l lr r

lraToList :: BA -> [LR]
lraToList ba = go 0
  where
    go j | bidx >= sz   = []
         | otherwise = case bits2lr bidx2 (indexWord8Array ba bidx) of
                         Nothing -> []
                         Just lr -> lr : go (j+1)
      where
        bidx  = j `unsafeShiftR` 2
        bidx2 = j .&. 0b11

    sz = sizeOfByteArray ba

-- $setup
-- >>> :set -XOverloadedStrings
