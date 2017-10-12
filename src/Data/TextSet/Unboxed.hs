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

    ) where

import           Control.DeepSeq
import           Data.Hashable                   (Hashable)
import qualified Data.Set                        as Set
import           Data.Text.Short                 (ShortText)
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


-- TODO: isProperSubsetOf isSubsetOf union intersection difference

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

-- Find near entry
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
    goL li ui -- invariant: li < ui
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
    goR li ui -- invariant: li < ui
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



-- $setup
-- >>> :set -XOverloadedStrings
