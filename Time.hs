{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Criterion.Types
import qualified Data.BloomFilter.Easy
import qualified Data.DAWG.Packed
import qualified Data.HashSet
import qualified Data.IntSet
import qualified Data.Set
import           System.Directory
import           System.Random

data InsertInt = forall f. NFData f => InsertInt String (Int -> f)

data FromListS =
  forall f. NFData f =>
            FromListS String
                     ([String] -> f)

data Intersection = forall f. NFData f =>
     Intersection String ([Int] -> f) (f -> f -> f)

data Member =
  forall f. (NFData f) =>
            Member String
                   ([Int] -> f)
                   (Int -> f ->  Bool)

data MemberS =
  forall f. (NFData f) =>
            MemberS String
                   ([String] -> f)
                   (String -> f ->  Bool)

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    [ bgroup
        "Insert Int (Randomized)"
        (insertInts
           [ InsertInt "Data.Set" insertSet
           , InsertInt "Data.HashSet" insertHashSet
           , InsertInt "Data.IntSet" insertIntSet
           ])
    , bgroup
        "Intersection (Randomized)"
        (intersection
           [ Intersection
               "Data.Set"
               Data.Set.fromList
               Data.Set.intersection
           , Intersection
               "Data.HashSet"
               Data.HashSet.fromList
               Data.HashSet.intersection
           , Intersection
               "Data.IntSet"
               Data.IntSet.fromList
               Data.IntSet.intersection
           ])
    , bgroup
        "Member Int (Randomized)"
        (memberRandomized
           [ Member
               "Data.Set"
               Data.Set.fromList
               Data.Set.member
           , Member
               "Data.HashSet"
               Data.HashSet.fromList
               Data.HashSet.member
           , Member
               "Data.IntSet"
               Data.IntSet.fromList
               Data.IntSet.member
           ])
    , bgroup
        "Member Int (Randomized, false positive rate 0.1)"
        (memberRandomized
           [ Member
               "Data.Set"
               Data.Set.fromList
               Data.Set.member
           , Member
               "Data.HashSet"
               Data.HashSet.fromList
               Data.HashSet.member
           , Member
               "Data.IntSet"
               Data.IntSet.fromList
               Data.IntSet.member
           , Member
               "Data.BloomFilter"
               (Data.BloomFilter.Easy.easyList 0.1)
               Data.BloomFilter.Easy.elem
           ])
    , bgroup
        "Member String (Randomized)"
        (memberRandomizedS
           [ MemberS
               "Data.Set"
               Data.Set.fromList
               Data.Set.member
           , MemberS
               "Data.HashSet"
               Data.HashSet.fromList
               Data.HashSet.member
           , MemberS
               "Data.DAWG.Packed"
               Data.DAWG.Packed.fromList
               Data.DAWG.Packed.member
           ])
    , bgroup
        "Member String (Randomized, false positive rate 0.1)"
        (memberRandomizedS
           [ MemberS
               "Data.Set"
               Data.Set.fromList
               Data.Set.member
           , MemberS
               "Data.HashSet"
               Data.HashSet.fromList
               Data.HashSet.member
           , MemberS
               "Data.DAWG.Packed"
               Data.DAWG.Packed.fromList
               Data.DAWG.Packed.member
           , MemberS
               "Data.BloomFilter"
               (Data.BloomFilter.Easy.easyList 0.1)
               Data.BloomFilter.Easy.elem
           ])
    , bgroup
        "FromList String (Monotonic)"
        (insertSMonotonic
           [ FromListS "Data.Set" Data.Set.fromList
           , FromListS "Data.HashSet" Data.HashSet.fromList
           , FromListS "Data.DAWG.Packed" Data.DAWG.Packed.fromList
           ])
    , bgroup
        "FromList String (Randomized)"
        (insertSRandomized
           [ FromListS "Data.Set" Data.Set.fromList
           , FromListS "Data.HashSet" Data.HashSet.fromList
           , FromListS "Data.DAWG.Packed" Data.DAWG.Packed.fromList
           ])
    ]
  where
    insertInts funcs =
      [ env
        (let !elems =
               force (take i (randoms (mkStdGen 0) :: [Int]))
         in pure elems)
        (\_ -> bench (title ++ ":" ++ show i) $ nf func i)
      | i <- [10, 100, 1000, 10000]
      , InsertInt title func <- funcs
      ]
    intersection funcs =
      [ env
        (let !args =
               force
                 ( build (take i (randoms (mkStdGen 0) :: [Int]))
                 , build (take i (randoms (mkStdGen 1) :: [Int])))
         in pure args)
        (bench (title ++ ":" ++ show i) . nf (uncurry intersect))
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , Intersection title build intersect <- funcs
      ]
    insertSRandomized funcs =
      [ env
        (let !elems =
               force
                 (map
                    show
                    (take i (randoms (mkStdGen 0) :: [Int])))
         in pure elems)
        (bench (title ++ ":" ++ show i) . nf func)
      | i <- [10, 100, 1000, 10000]
      , FromListS title func <- funcs
      ]
    insertSMonotonic funcs =
      [ env
        (let !elems =
               force
                 (map
                    show
                    [1 :: Int .. i])
         in pure elems)
        (bench (title ++ ":" ++ show i) . nf func)
      | i <- [10000]
      , FromListS title func <- funcs
      ]
    memberRandomized funcs =
      [ env
        (let list = take i (randoms (mkStdGen 0) :: [Int])
             !key = list !! div i 2
             !elems = force (fromList list)
         in pure (elems, key))
        (\(~(elems, key)) ->
           bench (title ++ ":" ++ show i) $ nf (`func` elems) key)
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , Member title fromList func <- funcs
      ]
    memberRandomizedS funcs =
      [ env
        (let list = take i (map show (randoms (mkStdGen 0) :: [Int]))
             !key = list !! div i 2
             !elems = force (fromList list)
         in pure (elems, key))
        (\(~(elems, key)) ->
           bench (title ++ ":" ++ show i) $ nf (`func` elems) key)
      | i <- [10, 100, 1000, 10000, 100000, 1000000]
      , MemberS title fromList func <- funcs
      ]

--------------------------------------------------------------------------------
-- Insert Int

insertSet :: Int -> Data.Set.Set Int
insertSet n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.Set.insert n acc)

insertHashSet :: Int -> Data.HashSet.HashSet Int
insertHashSet n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.HashSet.insert n acc)

insertIntSet :: Int -> Data.IntSet.IntSet
insertIntSet n0 = go n0 mempty
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (Data.IntSet.insert n acc)
