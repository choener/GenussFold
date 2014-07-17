
-- | Efficiently enumerate the bits in data types in order of population
-- count. This yields, say, @000, 001, 010, 100, 011, 101, 110, 111@ (or
-- @0, 1, 2, 4, 3, 5, 6, 7@). Another view is of looking at the bits as
-- a bitset, first enumerating the empty set, then all 1-element sets, all
-- 2-element sets, up to the set size.
--
-- The enumerator can be inlined with @unfoldr@ (of the @vector@ package)
-- and is a good producer.

module Data.Bits.Ordered where



-- | Given the full set (which is of the form @n==2^z-1@!) and the current
-- element, yield either @Just@ the new element and a new seed or
-- @Nothing@.

orderedBits :: () => a -> a -> Maybe (a,a)
orderedBits n c = undefined
