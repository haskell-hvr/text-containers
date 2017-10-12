{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE Unsafe           #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
module Data.TextArray.Unboxed.Internal where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString.Short as SBS
import           Data.Hashable         (Hashable (..))
import qualified Data.List             as List
import           Data.Semigroup
import           Data.Text.Short       (ShortText)
import qualified Data.Text.Short       as TS
import qualified GHC.Exts              as GHC
import           Prelude               hiding (elem, length, null)

import           Internal

-- | An array of unboxed 'ShortText' strings
--
-- The memory footprint of this data-structure is a single heap object (an unlifted 'ByteArray#') with the size expressed in words
--
-- \[
--  3 + n + \left\lceil \frac{1}{w} \sum_{i=0}^{n-1} len(s_i) \right\rceil
-- \]
--
-- where the word-size \(w\) is either \(w = 4\) or \(w = 8\) bytes; and where \(len(s_i)\) denotes the UTF-8 size in bytes of the \(i\)-th text string.
--
-- NOTE: Depending on whether you @UNPACK@ the 'TextArray' wrapper, you need at least one additional word for the pointer to the internal 'ByteArray#' heap object.
newtype TextArray = TA# BA

ta2ba :: TextArray -> BA
ta2ba (TA# x) = x

instance Eq TextArray where
  (TA# x) == (TA# y) = ba2sbs x == ba2sbs y

-- TODO: optimise
instance Ord TextArray where
  compare x y = compare (toList x) (toList y)

instance Show TextArray where
  showsPrec p ta = showsPrec p (toList ta)
  show ta        = show (toList ta)

instance Read TextArray where
  readsPrec p = map (\(x,s) -> (fromList x,s)) . readsPrec p

instance GHC.IsList TextArray where
  type Item TextArray = ShortText
  fromList  = fromList
  toList    = toList

instance NFData TextArray where
  rnf (TA# (BA# !_)) = ()

-- | \(\mathcal{O}(n+m)\). Concatenate two 'TextArray's.
instance Semigroup TextArray where
  (<>) = append

instance Monoid TextArray where
  mempty  = empty
  mappend = (<>)

instance Hashable TextArray where
  hashWithSalt salt (TA# ba) = hashWithSalt salt ba

-- | \(\mathcal{O}(1)\). An empty 'TextArray'
empty :: TextArray
empty = TA# $ createBA wordSize $ \mba -> writeIntArray mba 0 0

-- | \(\mathcal{O}(1)\). Construct 'TextArray' with single element
singleton :: ShortText -> TextArray
singleton t = TA# $ createBA totalSize $ \mba -> do
    writeIntArray mba 0 1
    writeIntArray mba 1 dataOfs
    copyByteArray (st2ba t) 0 mba dataOfs dataSize
  where
    dataOfs = wordSize * 2
    dataSize = sizeOfByteArray (st2ba t)
    totalSize = dataOfs + dataSize

-- | \(\mathcal{O}(1)\). Test whether 'TestArray' is empty.
null :: TextArray -> Bool
null = (==0) . length

-- | \(\mathcal{O}(1)\). Return number of strings contained in 'TestArray'.
length :: TextArray -> Int
length (TA# ba) = indexIntArray ba 0

-- | \(\mathcal{O}(1)\). Array indexing (0-based).
(!?) :: TextArray -> Int -> Maybe ShortText
ta@(TA# ba) !? i = case indexOfsLen ta i of
                     Just (IdxOfsLen _ ofs n) -> Just $! ba2st (sliceBA ba ofs n)
                     Nothing                  -> Nothing

-- | \(\mathcal{O}(n)\). Construct 'TextArray' from list of strings.
fromList :: [ShortText] -> TextArray
fromList [] = empty
fromList xs = TA# $ createBA totalSize $ \mba -> do
    writeIntArray mba 0 n
    writeOfsTab mba 1 dataOfs xs
    pure ()
  where
    writeOfsTab :: MBA s -> Int -> Int -> [ShortText] -> ST s ()
    writeOfsTab _   !_ !_   []     = pure ()
    writeOfsTab mba !j !ofs (t:ts) = do
        let y = SBS.length (TS.toShortByteString t)
        writeIntArray mba  j ofs
        copyByteArray (st2ba t) 0 mba ofs (sizeOfByteArray (st2ba t))
        writeOfsTab mba (j+1) (ofs+y) ts

    -- Memory layout of 'ByteArray#' payload:
    --
    -- - `N :: Int#` (number of strings in array)
    -- - byte-offset[j] (for 0 <= j < N) as 'Int#'
    -- - data-area: concatenated UTF8-encoded strings
    --
    -- byte-offset[0] points to start of 'data-area'
    --
    (dataSize,n) = sumLen $ map (SBS.length . TS.toShortByteString) xs
    dataOfs      = wordSize * (1+n)
    totalSize    = dataOfs + dataSize

-- | \(\mathcal{O}(n+m)\). Concatenate two 'TextArray's
append :: TextArray -> TextArray -> TextArray
append a b
  | null a  = b
  | null b  = a
append ta1@(TA# ba1) ta2@(TA# ba2) = TA# $ createBA totalSize $ \mba -> do
    writeIntArray mba 0 n
    forM_ [1..n1] $ \j -> writeIntArray mba j      (ofs1fixup + indexIntArray ba1 j)
    forM_ [1..n2] $ \j -> writeIntArray mba (n1+j) (ofs2fixup + indexIntArray ba2 j)
    copyByteArray ba1 dataOfs1 mba dataOfs1' dataSz1
    copyByteArray ba2 dataOfs2 mba dataOfs2' dataSz2
    pure ()
  where
    n         = n1+n2
    n1        = length ta1
    n2        = length ta2
    ofs1fixup = n2 * wordSize
    ofs2fixup = sz1 - wordSize
    sz1       = sizeOfByteArray ba1
    sz2       = sizeOfByteArray ba2
    dataOfs1  = (1+n1) * wordSize
    dataOfs2  = (1+n2) * wordSize
    dataSz1   = sz1 - dataOfs1
    dataSz2   = sz2 - dataOfs2
    dataOfs1' = (1+n) * wordSize
    dataOfs2' = dataOfs1'+dataSz1
    totalSize = sz1+sz2-wordSize

-- | \(\mathcal{O}(n)\). Deconstruct 'TextArray' into list of strings.
toList :: TextArray -> [ShortText]
toList ta = map (\(IdxOfsLen _ ofs n) -> ba2st (sliceBA (ta2ba ta) ofs n)) (listOfsLen ta)

-- | \(\mathcal{O}(n)\). Test whether 'TestArray' contains a specific string.
elem :: ShortText -> TextArray -> Bool
elem needle ta = not (List.null (elemIndices needle ta))

-- | \(\mathcal{O}(n)\). Find occurences of given string in 'TextArray' and report list of indices (in ascending order).
elemIndices :: ShortText -> TextArray -> [Int]
elemIndices needle ta = [ i | IdxOfsLen i ofs n <- listOfsLen ta, cmp ofs n ]
  where
    !ba2   = st2ba needle
    !ba2sz = sizeOfByteArray ba2

    cmp !ofs1 !n = equalByteArray ba2 0 ba2sz (ta2ba ta) ofs1 n

findAllOrd :: (Ordering -> Bool) -> ShortText -> TextArray -> [ShortText]
findAllOrd ordFilter needle ta = [ mkst ofs n | IdxOfsLen _ ofs n <- listOfsLen ta, ordFilter (cmp ofs n) ]
  where
    !ba2   = st2ba needle
    !ba2sz = sizeOfByteArray ba2

    mkst !ofs1 !n = ba2st (sliceBA (ta2ba ta) ofs1 n)
    cmp !ofs1 !n = compareByteArray ba2 0 ba2sz (ta2ba ta) ofs1 n

findIndicesOrd :: (Ordering -> Bool) -> ShortText -> TextArray -> [Int]
findIndicesOrd ordFilter needle ta = [ i | IdxOfsLen i ofs n <- listOfsLen ta, ordFilter (cmp ofs n) ]
  where
    !ba2   = st2ba needle
    !ba2sz = sizeOfByteArray ba2

    cmp !ofs1 !n = compareByteArray ba2 0 ba2sz (ta2ba ta) ofs1 n

----------------------------------------------------------------------------
-- internal helpers

indexOfsLen :: TextArray -> Int -> Maybe IdxOfsLen
indexOfsLen ta@(TA# ba) i
  | i >= sz   = Nothing
  | i < 0     = Nothing
  | otherwise
      = let i'    = i+1
            ofs  = indexIntArray ba i'
            ofs' | i'  == sz  = sizeOfByteArray ba
                 | otherwise  = indexIntArray ba (i'+1)
            !e    = IdxOfsLen i ofs (ofs'-ofs)
        in Just e
  where
    sz = length ta

indexOfsLen' :: TextArray -> Int -> IdxOfsLen
indexOfsLen' ta@(TA# ba) i
  | i >= sz   = IdxOfsLen 0 0 0
  | i < 0     = IdxOfsLen 0 0 0
  | otherwise
      = let i'    = i+1
            ofs  = indexIntArray ba i'
            ofs' | i'  == sz  = sizeOfByteArray ba
                 | otherwise  = indexIntArray ba (i'+1)
        in IdxOfsLen i ofs (ofs'-ofs)
  where
    sz = length ta

listOfsLen :: TextArray -> [IdxOfsLen]
listOfsLen ta@(TA# ba)
  | sz == 0 = []
  | otherwise = go 1 (indexIntArray ba 1)
  where
    sz = length ta

    go !i !ofs | i > sz     = []
               | otherwise  = e : go (i+1) ofs'
      where
        !e   = IdxOfsLen (i-1) ofs n
        n    = ofs'-ofs
        ofs' | i == sz   = sizeOfByteArray ba
             | otherwise = indexIntArray ba (i+1)

sumLen :: [Int] -> (Int,Int)
sumLen = go 0 0
  where
    go !s !l []     = (s,l)
    go !s !l (x:xs) = go (s+x) (l+1) xs
