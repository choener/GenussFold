
module Main where

import Data.List as L
import Data.Map.Strict as M
import Data.Tuple (swap)
import Data.Vector as V
import Debug.Trace
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import Data.Paired.Vector as DPV
import Data.Paired.Foldable as DPF
import Math.TriangularNumbers



-- * Data.Paired.Vector

-- |

prop_vector_upperTri_On :: NonNegative Int -> Bool
prop_vector_upperTri_On (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG OnDiag v
        ls = [ (a,b)
             | as@(a:_) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_vector_upperTri_No :: NonNegative Int -> Bool
prop_vector_upperTri_No (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG NoDiag v
        ls = [ (a,b)
             | (a:as) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_vector_rectangular :: NonNegative Int -> NonNegative Int -> Bool
prop_vector_rectangular (NonNegative k) (NonNegative l) = V.toList vs == ls
  where vs = snd $ rectangularVG as bs
        ls = [ (a,b)
             | a <- V.toList as
             , b <- V.toList bs
             ]
        as = V.enumFromTo 0 k
        bs = V.enumFromTo 0 l



-- * Data.Paired.Foldable

-- | Generalized upper triangular elements. We want to enumerate all
-- elements, including those on the main diagonal.

prop_foldable_upperTri_On_All :: (NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_On_All (NonNegative n, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) OnDiag All xs
        ls = [ ((a,b),(a,b))
             | as@(a:_) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

-- | Only a subset of elements, starting at @k@ (counting from 0) and
-- taking @s@ elements.

prop_foldable_upperTri_On_FromN :: (NonNegative Int, NonNegative Int, NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_On_FromN (NonNegative n, NonNegative k, NonNegative s, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) OnDiag (FromN k s) xs
        ls = L.take s
           . L.drop k
           $ [ ((a,b),(a,b))
             | as@(a:_) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

prop_foldable_upperTri_No_All :: (NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_No_All (NonNegative n, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) NoDiag All xs
        ls = [ ((a,b),(a,b))
             | (a:as) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

prop_foldable_upperTri_No_FromN :: (NonNegative Int, NonNegative Int, NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_No_FromN (NonNegative n, NonNegative k, NonNegative s, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) NoDiag (FromN k s) xs
        ls = L.take s
           . L.drop k
           $ [ ((a,b),(a,b))
             | (a:as) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls



-- * Math.TriangularNumbers

-- | Test that each index pair @(i,j)@ is assigned a unique linear index
-- @k@ given @0 <= i <= j <= n@.

prop_uniqueLinear :: NonNegative Int -> Bool
prop_uniqueLinear (NonNegative n) = M.null $ M.filter ((/=1) . L.length) mp
  where mp = M.fromListWith (L.++) [ (toLinear n (i,j), [(i,j)]) | i <- [0..n], j <- [i..n] ]

-- | Back and forth translation between paired and linear indices is
-- unique.

prop_BackForth :: NonNegative Int -> Bool
prop_BackForth (NonNegative n) = L.and xs
  where mb = M.fromList ls
        mf = M.fromList $ L.map swap ls
        ls = [ (toLinear n (i,j), (i,j)) | i <- [0..n], j <- [i..n] ]
        xs = [ (mb M.! k == (i,j)) && (mf M.! (i,j) == k) && fromLinear n k == (i,j)
             | (k,(i,j)) <- ls ]

--

main :: IO ()
main = $(defaultMainGenerator)

