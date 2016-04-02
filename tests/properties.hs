
module Main where

import Data.List as L
import Data.Vector as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import Data.Vector.Combined



prop_upperTri_On :: NonNegative Int -> Bool
prop_upperTri_On (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG OnDiag v
        ls = [ (a,b)
             | as@(a:_) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

prop_upperTri_No :: NonNegative Int -> Bool
prop_upperTri_No (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG NoDiag v
        ls = [ (a,b)
             | (a:as) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

prop_rectangular :: NonNegative Int -> NonNegative Int -> Bool
prop_rectangular (NonNegative k) (NonNegative l) = V.toList vs == ls
  where vs = snd $ rectangularVG as bs
        ls = [ (a,b)
             | a <- V.toList as
             , b <- V.toList bs
             ]
        as = V.enumFromTo 0 k
        bs = V.enumFromTo 0 l

main :: IO ()
main = $(defaultMainGenerator)

