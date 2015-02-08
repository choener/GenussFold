
{-# Language ScopedTypeVariables #-}

-- | Check a number of properties for popcount-ordered elements.

module Data.Bits.Ordered.QuickCheck where

import           Test.QuickCheck
import           Data.Int (Int16(..))
import           Data.Bits
import qualified Data.Vector.Unboxed as VU
import           Data.List (groupBy,sort)
import           Data.Function (on)
import           Data.Maybe (isJust)
import           Control.Monad (join)
import           Debug.Trace

import           Data.Bits.Ordered



prop_PopCountSet (NonZero (n :: Int16)) = memo == enum
  where b    = popCount n
        memo = memoSorted b
        enum = enumSorted b

memoSorted, enumSorted :: Int -> [[Int]]

memoSorted b = map sort . groupBy ((==) `on` popCount) $ VU.toList $ popCntMemoInt b
enumSorted b = map sort                                $ [0] : [ roll (succPopulation b) (Just $ 2^k-1) | k <- [1..b] ]
  where roll f (Just k) = k : roll f (f k)
        roll _ Nothing  = []

