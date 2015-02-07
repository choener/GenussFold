
{-# Language CPP #-}
{-# Language BangPatterns #-}

-- | Efficiently enumerate the bits in data types in order of population
-- count. This yields, say, @000, 001, 010, 100, 011, 101, 110, 111@ (or
-- @0, 1, 2, 4, 3, 5, 6, 7@). Another view is of looking at the bits as
-- a bitset, first enumerating the empty set, then all 1-element sets, all
-- 2-element sets, up to the set size.
--
-- The enumerator can be inlined with @unfoldr@ (of the @vector@ package)
-- and is a good producer.
--
-- A memo-table is available, since @popCntSorted@ is still waiting for an
-- efficient @popCntEnumerated@ that does not require sorting.

module Data.Bits.Ordered 
  -- bitset operations
  ( lsbActive
  , nextActive
  -- stream ever larger population counts
  , popCntSorted
  , popCntMemoInt
  , popCntMemoWord
  , succPopulation
  , popComplement
  ) where

import           Data.Bits
import           Data.Bits.Extras
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Algorithms.Intro as AI
import           Data.Ord (comparing)
import           Control.Arrow
import           Data.Word(Word(..))
import           Debug.Trace



-- * Move from one active bit to the next one

-- | Capture the no-bit-set case

captureNull :: Ranked t => t -> (t -> Int) -> Int
captureNull t f = if t==0 then -1 else f t
{-# INLINE captureNull #-}

-- | Get the lowest active bit. Returns @-1@ if no bit is set.

lsbActive :: Ranked t => t -> Int
lsbActive t = captureNull t lsb
{-# INLINE lsbActive #-}

-- | Given the set @t@ and the currently active bit @k@, get the next
-- active bit. Return @-1@ if there is no next active bit.

nextActive :: Ranked t => Int -> t -> Int
nextActive k t = lsbActive $ (t `shiftR` (k+1)) `shiftL` (k+1)
{-# INLINE nextActive #-}

-- * Population count methods

-- | The /slow/ default implementation. We sort the vector, not the list,
-- as sorting will walk the whole data structure anyway, and the vector
-- requires not as much memory.
--
-- Uses a stable sort so that @a,b@ with equal pop count still has @a<b@ in
-- the order via @comparing (popCount &&& id)@.

popCntSorted :: (Unbox n, Integral n, Bits n) => Int -> VU.Vector n
popCntSorted n = VU.modify (AI.sortBy (comparing (popCount &&& id))) $ VU.enumFromN 0 (2^n)
{-# INLINE popCntSorted #-}

-- | Memoized version of 'popCntSorted' for @Int@s.
--
-- NOTE Since this uses @popCntSorted@ for now it will still require a lot
-- of memory for sorting the vector!

popCntMemoInt
  :: Int    -- ^ size of the set we want. If larger than memo limit, will just call 'popCntSorted'
  -> VU.Vector Int
popCntMemoInt n
  | n>limit   = error $ "for safety reasons, memoization is only performed for popcounts up to " ++ show limit ++ " bits, memoize manually!"
  | otherwise = _popCntMemoInt !! n
  where limit = 28
{-# INLINE popCntMemoInt #-}

-- | Memoizes popcount arrays. The limit to memoization is enforced by
-- popCntMemo, not here.

_popCntMemoInt = map popCntSorted [0..]
{-# NOINLINE _popCntMemoInt #-}

-- | Memoized version of 'popCntSorted' for @Word@s.
--
-- NOTE Since this uses @popCntSorted@ for now it will still require a lot
-- of memory for sorting the vector!

popCntMemoWord
  :: Int    -- ^ size of the set we want. If larger than memo limit, will just call 'popCntSorted'
  -> VU.Vector Word
popCntMemoWord n
  | n>limit   = error $ "for safety reasons, memoization is only performed for popcounts up to " ++ show limit ++ " bits, memoize manually!"
  | otherwise = _popCntMemoWord !! n
  where limit = 28
{-# INLINE popCntMemoWord #-}

-- | Memoizes popcount arrays. The limit to memoization is enforced by
-- popCntMemo, not here.

_popCntMemoWord = map popCntSorted [0..]
{-# NOINLINE _popCntMemoWord #-}

-- | Enumerate all sets with the same population count. Given a population
-- @i@, this returns @Just j@ with @j>i@ (but same number of set bits) or
-- @Nothing@. For a population count of @k@, start with @2^(k+1) -1@.

succPopulation
  :: Ranked t
  => Int        -- zero-based index of the highest bit in the population
  -> t          -- current population
  -> Maybe t    -- Just the new population, or nothing if now higher-ordered population exists.
succPopulation !h' !s' = go h' s' where
  go !h !s
    | s == 0    = Nothing
    | m == h    = fmap (`setBit` h) (go (h-1) (clearBit s h))
    | otherwise = Just $ setBit (clearBit s m) (m+1)
    where !m = msb s
{-# INLINE succPopulation #-}

-- | Given a population, get the complement.

popComplement
  :: Ranked t
  => Int        -- zero-based index of the highest bit in the population
  -> t          -- current population
  -> t          -- complement of the population. All bits higher than the highest bit are kept zero.
popComplement !h !s = mask .&. complement s
  where mask = (2^(h+1) -1)
{-# INLINE popComplement #-}



-- WARNING: Conditional compilation based on architecture!

instance Ranked Int where
#if x86_64_HOST_ARCH
  lsb  = lsb  . w64
  rank = rank . w64
  nlz  = nlz  . w64
#endif
#if i386_HOST_ARCH
  lsb  = lsb  . w32
  rank = rank . w32
  nlz  = nlz  . w32
#endif
  {-# INLINE lsb  #-}
  {-# INLINE rank #-}
  {-# INLINE nlz  #-}

instance Ranked Word where
#if x86_64_HOST_ARCH
  lsb  = lsb  . w64
  rank = rank . w64
  nlz  = nlz  . w64
#endif
#if i386_HOST_ARCH
  lsb  = lsb  . w32
  rank = rank . w32
  nlz  = nlz  . w32
#endif
  {-# INLINE lsb  #-}
  {-# INLINE rank #-}
  {-# INLINE nlz  #-}

