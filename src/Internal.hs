{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE Unsafe           #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
module Internal
    ( module Internal
    , module Data.Word
    , Data.Semigroup.Semigroup((<>))
    , ST
    , ShortText
    , assert
    , unless
    , when
    , Hashable(..)
    ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString.Short          as SBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Hashable                  (Hashable (..),
                                                 hashByteArrayWithSalt)
import           Data.Semigroup
import           Data.Text.Short                (ShortText)
import qualified Data.Text.Short                as TS
import qualified Data.Text.Short.Unsafe         as TS
import           Data.Word
import           GHC.Exts                       (Int (..))
import           GHC.Prim
import           GHC.ST                         (ST (ST))
import           GHC.Word                       (Word8 (..))
import           Numeric                        (showHex)
import           Prelude                        hiding (elem, length, null)

import           PrimOps                        (compareByteArrays#)

----------------------------------------------------------------------------
-- GHC (Mutable)ByteArray# helpers

-- | Wordsize in bytes
wordSize :: Int
wordSize = case finiteBitSize (0 :: Int) of
             32 -> 4
             64 -> 8
             _  -> error "unsupported wordSize"

data BA    = BA#   ByteArray#
data MBA s = MBA# (MutableByteArray# s)

ba2sbs :: BA -> SBS.ShortByteString
ba2sbs (BA# x) = SBS.SBS x

ba2st :: BA -> ShortText
ba2st = TS.fromShortByteStringUnsafe . ba2sbs

st2ba :: ShortText -> BA
st2ba x = case TS.toShortByteString x of
            SBS.SBS y -> BA# y

st2sz :: ShortText -> Int
st2sz = sizeOfByteArray . st2ba

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# srcOfs#) (MBA# dest#) (I# destOfs#) (I# n#)
  = ST $ \s -> case copyByteArray# src# srcOfs# dest# destOfs# n# s of
                 s' -> (# s', () #)

indexIntArray :: BA -> Int -> Int
indexIntArray (BA# ba#) (I# i#)
  = I# (indexIntArray# ba# i#)

indexWord8Array :: BA -> Int -> Word8
indexWord8Array (BA# ba#) (I# i#)
  = W8# (indexWord8Array# ba# i#)

sizeOfByteArray :: BA -> Int
sizeOfByteArray (BA# ba#) = I# (sizeofByteArray# ba#)

writeIntArray :: MBA s -> Int -> Int -> ST s ()
writeIntArray (MBA# mba#) (I# i#) (I# j#)
  = ST $ \s -> case writeIntArray# mba# i# j# s of
                 s' -> (# s', () #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# j#)
  = ST $ \s -> case writeWord8Array# mba# i# j# s of
                 s' -> (# s', () #)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#)
  = ST $ \s -> case newByteArray# n# s of
                 (# s', mba# #) -> (# s', MBA# mba# #)

shrinkMutableByteArray :: MBA s -> Int -> ST s ()
shrinkMutableByteArray (MBA# mba#) (I# n#)
  = ST $ \s -> case shrinkMutableByteArray# mba# n# s of
                 s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#)
  = ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s', ba# #) -> (# s', BA# ba# #)

createBA :: Int -> (forall s. MBA s -> ST s ()) -> BA
createBA n go = runST $ do
  mba <- newByteArray n
  go mba
  unsafeFreezeByteArray mba

createBA' :: Int -> (forall s. MBA s -> ST s x) -> (BA,x)
createBA' n go = runST $ do
  mba <- newByteArray n
  x <- go mba
  ba <- unsafeFreezeByteArray mba
  pure (ba,x)

sliceBA :: BA -> Int -> Int -> BA
sliceBA orig ofs0 n = assert (ofs0 + n <= sizeOfByteArray orig) $
                      createBA n $ \mba -> copyByteArray orig ofs0 mba 0 n

instance Hashable BA where
  hashWithSalt salt ba@(BA# ba#) = hashByteArrayWithSalt ba# 0 (sizeOfByteArray ba) salt

instance Show BA where
  show = hexdumpBA

----------------------------------------------------------------------------

data A e = A# (Array# e)
data MA s e = MA# (MutableArray# s e)

sizeofArray :: A e -> Int
sizeofArray (A# a#) = I# (sizeofArray# a#)

indexArray :: A e -> Int -> e
indexArray (A# a#) (I# i#)
  = case indexArray# a# i# of (# e #) -> e

writeArray :: MA s e -> Int -> e -> ST s ()
writeArray (MA# ma#) (I# i#) e
  = ST $ \s -> case writeArray# ma# i# e s of
                 s' -> (# s', () #)

newArray :: Int -> e -> ST s (MA s e)
newArray (I# n#) e
  = ST $ \s -> case newArray# n# e s of
                 (# s', ma# #) -> (# s', MA# ma# #)

unsafeFreezeArray :: MA s e -> ST s (A e)
unsafeFreezeArray (MA# ma#)
  = ST $ \s -> case unsafeFreezeArray# ma# s of
                 (# s', a# #) -> (# s', A# a# #)

createA :: Int -> e -> (forall s. MA s e -> ST s ()) -> A e
createA n e go = runST $ do
  ma <- newArray n e
  go ma
  unsafeFreezeArray ma

{-# NOINLINE emptyA #-}
emptyA :: A e
emptyA = runST $ do
  ma <- newArray 0 undefined
  unsafeFreezeArray ma

arraySingleton :: e -> A e
arraySingleton x = runST $ do
  ma <- newArray 1 x
  unsafeFreezeArray ma

arrayFromListN :: Int -> [e] -> A e
arrayFromListN n xs0 = createA n undefined $ \ma -> do
    let go !i [] = assert (i==n) $ pure ()
        go !i (x:xs) = do
          writeArray ma i x
          go (i+1) xs
    go 0 xs0

arrayMap :: (a->b) -> A a -> A b
arrayMap f arr = createA sz undefined $ \ma -> do
    let go i | i < sz = do
                 writeArray ma i (f (indexArray arr i))
                 go (i+1)
             | otherwise = pure ()
    go 0
  where
    !sz = sizeofArray arr

arrayTraverse :: Applicative f => (a -> f b) -> A a -> f (A b)
arrayTraverse act arr = arrayFromListN (sizeofArray arr) <$> traverse act (arrayToList arr)

arrayToList :: A e -> [e]
arrayToList arr = go 0
  where
    !sz = sizeofArray arr
    go i | i < sz    = indexArray arr i : go (i+1)
         | otherwise = []

instance NFData e => NFData (A e) where
  rnf arr = rnf (arrayToList arr)

instance Show e => Show (A e) where
  showsPrec p = showsPrec p . arrayToList

instance Eq e => Eq (A e) where
  xs == ys  = arrayToList xs == arrayToList ys

instance Ord e => Ord (A e) where
  compare xs ys = compare (arrayToList xs) (arrayToList ys)

instance Functor A where
  fmap = arrayMap
  v <$ arr0 = runST $ do
    ma <- newArray (sizeofArray arr0) v
    unsafeFreezeArray ma

instance Hashable e => Hashable (A e) where
  hashWithSalt salt = hashWithSalt salt . arrayToList

----------------------------------------------------------------------------

equalByteArray :: BA -> Int -> Int -> BA -> Int -> Int -> Bool
equalByteArray (BA# ba1#) !(I# ofs1#) !n1@(I# n1#) (BA# ba2#) !(I# ofs2#) !n2
  | n1 /= n2  = False
  | n1 == 0   = True
  | otherwise = I# (compareByteArrays# ba1# ofs1# ba2# ofs2# n1#) == 0

compareByteArray :: BA -> Int -> Int -> BA -> Int -> Int -> Ordering
compareByteArray (BA# ba1#) !(I# ofs1#) !n1 (BA# ba2#) !(I# ofs2#) !n2
  | n == 0 = compare n1 n2
  | otherwise = case compareByteArrays# ba1# ofs1# ba2# ofs2# n# of
      r# | I# r# < 0 -> LT
         | I# r# > 0 -> GT
         | n1 < n2   -> LT
         | n1 > n2   -> GT
         | otherwise -> EQ
  where
    !n@(I# n#) = n1 `min` n2

data IdxOfsLen = IdxOfsLen !Int !Int !Int
               deriving Show

cmpBA2OfsLen :: BA -> BA -> IdxOfsLen -> Ordering
cmpBA2OfsLen !ba !ba2 = \(IdxOfsLen _ ofs n) -> compareByteArray ba 0 (sizeOfByteArray ba) ba2 ofs n

cmpOfsLens :: BA -> IdxOfsLen -> BA -> IdxOfsLen -> Ordering
cmpOfsLens !ba !(IdxOfsLen _ ofs n) !ba2 !(IdxOfsLen _ ofs2 n2) = compareByteArray ba ofs n ba2 ofs2 n2

hexdumpBA :: BA -> String
hexdumpBA ba = "0x" ++ go 0
  where
    go j | j < sz    = h (indexWord8Array ba j) (spacer (go (j+1)))
         | otherwise = ""
      where
        spacer = id
        -- spacer = if j `rem` 8 == 7 then ('_':) else id

    sz = sizeOfByteArray ba

    h :: Word8 -> ShowS
    h x | x < 0x10  = ('0':) . showHex x
        | otherwise = showHex x

----------------------------------------------------------------------------
-- helper

-- | Encodes a count and size pair
data CntSize = CS !Int !Int deriving Show

-- | Add size to 'CntSize'
(^+) :: CntSize -> Int -> CntSize
(CS c s) ^+ s1 = CS (c+1) (s+s1)

sumCntSize :: [Int] -> CntSize
sumCntSize = go 0 0
  where
    go !s !l []     = CS l s
    go !s !l (x:xs) = go (s+x) (l+1) xs
