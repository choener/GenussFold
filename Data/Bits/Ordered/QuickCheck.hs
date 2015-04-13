
{-# Options_GHC -O0 #-}

-- | Check a number of properties for popcount-ordered elements.
--
-- $setup
--
-- >>> :set -XScopedTypeVariables
--

module Data.Bits.Ordered.QuickCheck where

import           Test.QuickCheck
import           Data.Int (Int16(..))
import           Data.Bits
import qualified Data.Vector.Unboxed as VU
import           Data.List (groupBy,sort,permutations,nub)
import           Data.Function (on)
import           Data.Maybe (isJust)
import           Control.Monad (join)
import           Debug.Trace

import           Data.Bits.Ordered



-- | Check if both the memoized version and the population enumeration
-- produce the same multisets, but maybe in different order.
--
-- prop> \(n :: Int16) -> let b = popCount n in memoSorted b == enumSorted b
--

prop_PopCountSet (NonZero (n :: Int16)) = memo == enum
  where b    = popCount n
        memo = memoSorted b
        enum = enumSorted b

memoSorted, enumSorted :: Int -> [[Int]]

memoSorted b = map sort . groupBy ((==) `on` popCount) $ VU.toList $ popCntMemoInt b
enumSorted b = map sort                                $ [0] : [ roll (popPermutation b) (Just $ 2^k-1) | k <- [1..b] ]
  where roll f (Just k) = k : roll f (f k)
        roll _ Nothing  = []

prop_lsb_Int (x :: Int) = lsbZ x == maybe (-1) id (maybeLsb x)

prop_lsb_Word (x :: Word) = lsbZ x == maybe (-1) id (maybeLsb x)

prop_OneBits_Int (x :: Int) = popCount x == length abl && and [ testBit x k | k <- abl ]
  where abl = activeBitsL x

-- Tests if we actually generate all permutations.

prop_allPermutations (a :: Int , b :: Int) = and $ zipWith cmp (sort qs) (sort $ nub ps)
  where nbs = min a' b' -- number of 1 bits in set
        sts = max a' b' -- set size
        a' = a `mod` 8 -- finiteBitSize a
        b' = b `mod` 8 -- finiteBitSize b
        ps = permutations $ replicate (sts - nbs) False ++ replicate nbs True
        qs = go (Just $ 2 ^ nbs - 1)
        go :: Maybe Int -> [Int]
        go Nothing  = []
        go (Just k) = k : go (popPermutation sts k)
        cmp k as = and [ if a then testBit k c else (not $ testBit k c) | (a,c) <- zip (reverse as) [0 .. ] ]

-- TODO popComplement

-- TODO popMove

