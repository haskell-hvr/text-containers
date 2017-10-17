-- repro-helper for https://ghc.haskell.org/trac/ghc/ticket/14361

-- The code below works with GHC 8.3
-- If both, the `-fno-strictness` as well as the `NOINLINE` pragma are active.
-- Remove one, and the test-suite starts failing.

{-# OPTIONS_GHC -fno-strictness #-}

module Internal2
    ( Internal2.cmpBA2OfsLen
    ) where

import           Internal hiding (cmpBA2OfsLen)

{-# NOINLINE cmpBA2OfsLen #-}
cmpBA2OfsLen :: BA -> BA -> IdxOfsLen -> Ordering
-- cmpBA2OfsLen !ba !ba2 = \(IdxOfsLen _ ofs n) -> compareByteArray ba 0 (sizeOfByteArray ba) ba2 ofs n
cmpBA2OfsLen ba ba2 (IdxOfsLen _ ofs n) = compareByteArray ba 0 (sizeOfByteArray ba) ba2 ofs n
