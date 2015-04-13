
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
  ( lsbZ
  , nextActiveZ
  , maybeNextActive
  , maybeLsb
  -- population operations
  , popPermutation
  , popComplement
  -- stream ever larger population counts
  , popCntSorted
  , popCntMemoInt
  , popCntMemoWord
  , popMove
  -- structures with active bits
  , activeBitsL
  , activeBitsS
  , activeBitsV
  ) where

import           Control.Arrow
import           Data.Bits
import           Data.Bits.Extras
import           Data.Ord (comparing)
import           Data.Vector.Unboxed (Unbox)
import           Data.Word(Word(..))
import           Debug.Trace
import qualified Data.Vector.Algorithms.Intro as AI
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU



-- * Move from one active bit to the next one

-- | Capture the no-bit-set case

captureNull :: Ranked t => t -> (t -> Int) -> Int
captureNull t f = if t==0 then -1 else f t
{-# INLINE captureNull #-}

-- | Get the lowest active bit. Returns @-1@ if no bit is set.

lsbZ :: Ranked t => t -> Int
lsbZ t = captureNull t lsb
{-# INLINE lsbZ #-}

-- | Given the set @t@ and the currently active bit @k@, get the next
-- active bit. Return @-1@ if there is no next active bit.

nextActiveZ :: Ranked t => Int -> t -> Int
nextActiveZ k t = lsbZ $ (t `shiftR` (k+1)) `shiftL` (k+1)
{-# INLINE nextActiveZ #-}

-- | Return next active bit, using @Maybe@.

maybeNextActive :: Ranked t => Int -> t -> Maybe Int
maybeNextActive k t = if t'==0 then Nothing else Just (lsb t')
  where t' = (t `shiftR` (k+1) `shiftL` (k+1))
{-# Inline maybeNextActive #-}

-- | @Maybe@ the lowest active bit.

maybeLsb :: Ranked t => t -> Maybe Int
maybeLsb t = if t==0 then Nothing else Just (lsb t)
{-# Inline maybeLsb #-}

-- | List of all active bits, from lowest to highest.

activeBitsL :: Ranked t => t -> [Int]
activeBitsL = S.toList . activeBitsS
{-# Inline activeBitsL #-}

-- | A generic vector (specializes to the corrept type) of the active bits,
-- lowest to highest.

activeBitsV :: (Ranked t, VG.Vector v Int) => t -> v Int
activeBitsV = VG.unstream . activeBitsS
{-# Inline activeBitsV #-}

-- | A stream with the currently active bits, lowest to highest.

activeBitsS :: (Ranked t, Monad m) => t -> SM.Stream m Int
activeBitsS t = SM.unfoldr (fmap (id &&& (`maybeNextActive` t))) (maybeLsb t)
{-# Inline activeBitsS #-}

-- * Population count methods

-- | The /slow/ default implementation. We sort the vector, not the list,
-- as sorting will walk the whole data structure anyway, and the vector
-- requires not as much memory.
--
-- Replaced @popCount &&& id@ as sort, which provides for @a<b@ on equal
-- @popCount@ with @popCount &&& activeBitsL@ which sorts according to
-- a list of increasing bit indices. Mostly to stay in sync with the @pred@
-- / @succ@ functions below.

popCntSorted :: (Unbox n, Integral n, Bits n, Ranked n) => Int -> VU.Vector n
popCntSorted n = VU.modify (AI.sortBy (comparing (popCount &&& activeBitsL))) $ VU.enumFromN 0 (2^n)
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
--
-- Note that @sort permutations == sort (nub permutations)@ if
-- @permutations@ is a set of all permutations for a given @popCount@
-- generated by @popPermutation@. The @Data.List.permutations@ functions
-- will create duplicates.
--
-- cf
-- <http://en.wikipedia.org/wiki/Permutation#Algorithms_to_generate_permutations>

popPermutation
  :: Ranked t
  => Int        -- size of the set we want. (i.e. numbor of bits available for @0@ or @1@)
  -> t          -- current population
  -> Maybe t    -- Just the new population, or nothing if now higher-ordered population exists.
popPermutation !h' !s'
  | popCount s' < 1 || h' < 2 = Nothing
  | Just k <- findK   (h' -2)
  , Just l <- findL k (h' -1)
  = let swp = setBit (clearBit s' k) l
    in  Just $ reverseFrom (k+1) (h' -1) swp swp
  | otherwise = Nothing
  where findK k
          | k < 0                                  = Nothing
          | testBit s' k && not (testBit s' (k+1)) = Just k
          | otherwise                              = findK (k-1)
        findL k l
          | l <= k             = Nothing
          | not $ testBit s' l = Just l
          | otherwise          = findL k $ l-1
        reverseFrom u d src tgt
          | u >= h'   = tgt
          | otherwise = reverseFrom (u+1) (d-1) src (assignBit (assignBit tgt u (testBit src d)) d (testBit src u))
{-# INLINE popPermutation #-}

-- | Given a population, get the complement. The first argument is the size
-- of the population (say. 8 for 8 bits); the second the current
-- population.
--
-- Examples:
--
-- >>> popComplement 5 (3 :: Int)
-- 28
--
-- >>> popComplement 6 (3 :: Int)
-- 60

popComplement
  :: Ranked t
  => Int        -- size of the population set
  -> t          -- current population
  -> t          -- complement of the population. All bits higher than the highest bit are kept zero.
popComplement !h !s = mask .&. complement s
  where mask = (2^h -1)
{-# INLINE popComplement #-}

-- | Move a population around. Assume that you have a bitmask @mask
-- = 10101@ and a least-significant aligned population @11@, then given
-- mask and population you'd like to see @00101@, i.e. the two lowest
-- one bits of the mask are set. @101@ would set the lowest and third one
-- bit.
--
-- Examples:
--
-- >>> popMove (21::Int) 3 -- 10101 00011  -- 00101
-- 5
-- >>> popMove (28::Int) 0 -- 11100 00000  -- 00000
-- 0
-- >>> popMove (28::Int) 1 -- 11100 00001  -- 00100
-- 4
-- >>> popMove (28::Int) 2 -- 11100 00010  -- 01000
-- 8
-- >>> popMove (28::Int) 3 -- 11100 00011  -- 01100
-- 12

popMove
  :: (Ranked t)
  => t          -- the mask
  -> t          -- the population
  -> t          -- final population
popMove mask lsp = go 0 0 mask lsp where
  go !acc !(k::Int) !m !l
    | l==0              = acc
    | testBit m 0
    , testBit l 0       = go (acc + unsafeShiftL 1 k) (k+1) (unsafeShiftR m 1) (unsafeShiftR l 1)
    | not $ testBit m 0 = go acc                      (k+1) (unsafeShiftR m 1) l
    | not $ testBit l 0 = go acc                      (k+1) (unsafeShiftR m 1) (unsafeShiftR l 1)
{-# Inline popMove #-}



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

