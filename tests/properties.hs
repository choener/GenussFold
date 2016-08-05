
module Main where

import Data.List as L
import Data.Map.Strict as M
import Data.Tuple (swap)
import Data.Vector as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import Data.Combined.Vector
import Math.TriangularNumbers



-- |

prop_upperTri_On :: NonNegative Int -> Bool
prop_upperTri_On (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG OnDiag v
        ls = [ (a,b)
             | as@(a:_) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_upperTri_No :: NonNegative Int -> Bool
prop_upperTri_No (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG NoDiag v
        ls = [ (a,b)
             | (a:as) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_rectangular :: NonNegative Int -> NonNegative Int -> Bool
prop_rectangular (NonNegative k) (NonNegative l) = V.toList vs == ls
  where vs = snd $ rectangularVG as bs
        ls = [ (a,b)
             | a <- V.toList as
             , b <- V.toList bs
             ]
        as = V.enumFromTo 0 k
        bs = V.enumFromTo 0 l

-- | Test that each index pair @(i,j)@ is assigned a unique linear index
-- @k@ given @0 <= i <= j <= n@.

prop_uniqueLinear :: NonNegative Int -> Bool
prop_uniqueLinear (NonNegative n) = M.null $ M.filter ((/=1) . L.length) mp
  where mp = M.fromListWith (L.++) [ (toLinear (0,n) (i,j), [(i,j)]) | i <- [0..n], j <- [i..n] ]

-- | Back and forth translation between paired and linear indices is
-- unique.

prop_BackForth :: NonNegative Int -> Bool
prop_BackForth (NonNegative n) = L.and xs
  where mb = M.fromList ls
        mf = M.fromList $ L.map swap ls
        ls = [ (toLinear (0,n) (i,j), (i,j)) | i <- [0..n], j <- [i..n] ]
        xs = [ (mb M.! k == (i,j)) && (mf M.! (i,j) == k) && fromLinear n k == (i,j)
             | (k,(i,j)) <- ls ]

--

main :: IO ()
main = $(defaultMainGenerator)

