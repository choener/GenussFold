
module Main where

import Data.List as L
import Data.Vector as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import Data.Vector.Combined



prop_upperTri_On :: Positive Int -> Bool
prop_upperTri_On (Positive k) = V.toList vs == ls
  where vs = upperTriVG OnDiag v
        ls = [ (a,b)
             | as@(a:_) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

prop_upperTri_No :: Positive Int -> Bool
prop_upperTri_No (Positive k) = V.toList vs == ls
  where vs = upperTriVG NoDiag v
        ls = [ (a,b)
             | (a:as) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

main :: IO ()
main = $(defaultMainGenerator)

