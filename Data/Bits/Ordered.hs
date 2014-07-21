
-- | Efficiently enumerate the bits in data types in order of population
-- count. This yields, say, @000, 001, 010, 100, 011, 101, 110, 111@ (or
-- @0, 1, 2, 4, 3, 5, 6, 7@). Another view is of looking at the bits as
-- a bitset, first enumerating the empty set, then all 1-element sets, all
-- 2-element sets, up to the set size.
--
-- The enumerator can be inlined with @unfoldr@ (of the @vector@ package)
-- and is a good producer.

module Data.Bits.Ordered 
  ( popCntSorted
  , popCntMemo
  ) where

import           Data.Bits
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Algorithms.Intro as AI
import           Data.Ord (comparing)
import           Control.Arrow



-- | The /slow/ default implementation. We sort the vector, not the list,
-- as sorting will walk the whole data structure anyway, and the vector
-- requires not as much memory.
--
-- Uses a stable sort so that @a,b@ with equal pop count still has @a<b@ in
-- the order via @comparing (popCount &&& id)@.

popCntSorted :: (Unbox n, Integral n, Bits n) => Int -> VU.Vector n
popCntSorted n = VU.modify (AI.sortBy (comparing (popCount &&& id))) $ VU.enumFromN 0 (2^n)
{-# INLINE popCntSorted #-}

-- | Memoized version of 'popCntSorted'.
--
-- NOTE Since this uses @popCntSorted@ for now it will still require a lot
-- of memory for sorting the vector!

popCntMemo
  :: Int    -- ^ memoize how many bits? Can only go up once a certain number of bits have been memoized
  -> Int    -- ^ size of the set we want. If larger than memo limit, will just call 'popCntSorted'
  -> VU.Vector Int
popCntMemo m n
  | n>m       = popCntSorted n
  | m>limit   = error $ "for safety reasons, memoization is only performed for popcounts up to " ++ show limit ++ " bits, memoize manually!"
  | otherwise = _popCntMemo !! n
  where limit = 31
{-# INLINE popCntMemo #-}

-- | Memoizes popcount arrays. The limit to memoization is enforced by
-- popCntMemo, not here.

_popCntMemo = map popCntSorted [0..]
{-# NOINLINE _popCntMemo #-}

