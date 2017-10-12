{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Data.List
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import qualified Data.Text.Short           as ST

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC

import qualified Data.TextArray.Unboxed    as TA
import qualified Data.TextMap.Unboxed.Lazy as TM
import qualified Data.TextSet.Unboxed      as TS

main :: IO ()
main = defaultMain tests

-- compat for containers<0.5.9
setLookupMin :: Set.Set a -> Maybe a
setLookupMin s = if Set.null s then Nothing else Just (Set.findMin s)
setLookupMax :: Set.Set a -> Maybe a
setLookupMax s = if Set.null s then Nothing else Just (Set.findMax s)

instance Arbitrary ST.ShortText where
  arbitrary = ST.fromText <$> arbitrary

instance Arbitrary TA.TextArray where
  arbitrary = TA.fromList <$> arbitrary

tests :: TestTree
tests = testGroup "Tests" [testsMapLazy, testsSet, testsArray]
  where
    testsArray = testGroup "TextArray"
      [ QC.testProperty "toList.fromList" $
          \xs -> (TA.toList . TA.fromList) xs == xs

      , QC.testProperty "singleton" $
          \x -> (TA.toList . TA.singleton) x == [x]

      , QC.testProperty "length" $
          \xs -> length xs == TA.length (TA.fromList xs)

      , QC.testProperty "null" $
          \xs -> null xs == TA.null (TA.fromList xs)

      , QC.testProperty "elem" $
          \xs -> all (`TA.elem` (TA.fromList xs)) xs

      , QC.testProperty "elemIndices" $
          \t0 t1 t2 t3 t4 t5 -> let xs = [t1,t0,t2,t0,t3,t0,t4,t5]
                                    ta = TA.fromList xs
                                in all (\z -> elemIndices z xs == TA.elemIndices z ta) xs

      , QC.testProperty "elemIndices#2" $
          \xs -> let ta = TA.fromList xs
                 in all (\z -> elemIndices z xs == TA.elemIndices z ta) (ST.fromString "Shakespeare":xs)

      , QC.testProperty "(!?)" $
          \xs -> let ta = TA.fromList xs
                 in mapM (ta TA.!?) [0.. (TA.length ta)-1] == Just xs

      , QC.testProperty "append" $
          \xs ys -> TA.fromList (xs++ys) == TA.fromList xs `mappend` TA.fromList ys

      -- , QC.testProperty "findAllOrd EQ" $
      --     \xs -> let ta = TA.fromList xs
      --            in all (\z -> filter (==z) xs == TA.findAllOrd (==EQ) z ta) (ST.fromString "Shakespeare":xs)

      -- , QC.testProperty "findIndicesOrd EQ" $
      --     \xs -> let ta = TA.fromList xs
      --            in all (\z -> findIndices (==z) xs == TA.findIndicesOrd (==EQ) z ta) (ST.fromString "Shakespeare":xs)
      ]

    testsSet = testGroup "TextSet"
      [ QC.testProperty "toList.fromList" $
          \xs -> (TS.toList . TS.fromList) xs == (Set.toList . Set.fromList) xs

      , QC.testProperty "singleton" $
          \x -> (TS.toList . TS.singleton) x == [x]

      , QC.testProperty "toList.fromSet" $
          \xs -> (TS.toList . TS.fromSet) xs == Set.toList xs

      , QC.testProperty "toList.toSet" $
          \xs -> (TS.toSet . TS.fromList) xs == Set.fromList xs

      , QC.testProperty "size" $
          \xs -> Set.size (Set.fromList xs) == TS.size (TS.fromList xs)

      , QC.testProperty "null" $
          \xs -> null xs == TS.null (TS.fromList xs)

      , QC.testProperty "member" $
          \xs -> all (`TS.member` (TS.fromList xs)) xs

      , QC.testProperty "(!?)" $
          \xs -> let s1 = TS.fromList xs
                     s2 = Set.fromList xs
                 in all (\j -> s1 TS.!? j == Just (Set.elemAt j s2)) [0 .. Set.size s2 - 1]

      , QC.testProperty "lookupMin" $
          \xs -> (TS.lookupMin . TS.fromList) xs == (setLookupMin . Set.fromList) xs

      , QC.testProperty "lookupMax" $
          \xs -> (TS.lookupMax . TS.fromList) xs == (setLookupMax . Set.fromList) xs

      , QC.testProperty "lookupIndex" $
          \xs -> let s1 = TS.fromList xs
                     s2 = Set.fromList xs
                 in all (\k -> TS.lookupIndex k s1 == Set.lookupIndex k s2) (someStrs ++ xs)

      , QC.testProperty "lookupLE" $
          \xs -> let s1 = TS.fromList xs
                     s2 = Set.fromList xs
                 in all (\k -> fmap snd (TS.lookupLE k s1) == Set.lookupLE k s2) (someStrs ++ xs)

      , QC.testProperty "lookupGE" $
          \xs -> let s1 = TS.fromList xs
                     s2 = Set.fromList xs
                 in all (\k -> fmap snd (TS.lookupGE k s1) == Set.lookupGE k s2) (someStrs ++ xs)

      ]

    testsMapLazy = testGroup "TextMap(Lazy)"
      [ QC.testProperty "toList.fromList" $
          \(Ps xs) -> (TM.toList . TM.fromList) xs == (Map.toList . Map.fromList) xs

      , QC.testProperty "singleton" $
          \x y -> TM.toList (TM.singleton x y) == [(x,y::Int)]

      , QC.testProperty "toList.fromMap" $
          \xs -> (TM.toList . TM.fromMap) xs == Map.toList (xs :: Map.Map ST.ShortText Int)

      , QC.testProperty "toList.toMap" $
          \(Ps xs) -> (TM.toMap . TM.fromList) xs == Map.fromList xs

      , QC.testProperty "size" $
          \(Ps xs) -> TM.size (TM.fromList xs) == Map.size (Map.fromList xs)

      , QC.testProperty "null" $
          \(Ps xs) -> TM.null (TM.fromList xs) == null xs

      , QC.testProperty "member" $
          \(Ps xs) -> all (`TM.member` (TM.fromList xs)) (map fst xs)

      , QC.testProperty "(!?)" $
          \(Ps xs) -> let s1 = TM.fromList xs
                          s2 = Map.fromList xs
                      in all (\k -> (s1 TM.!? k) == (Map.lookup k s2)) (someStrs ++ map fst xs)
      ]


someStrs :: [ST.ShortText]
someStrs = map ST.fromString ["","\x00","a","Z","\x10ffff"]

newtype Ps = Ps [(ST.ShortText,Int)]
           deriving (Arbitrary,Show)
