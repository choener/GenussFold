
-- | Triangular numbers and various helper functions.
--
-- Main use is for linearization of triangular array indexing.
-- 
-- Triangular numbers:
-- @
-- T_n = Σ_{k=1)^n k = 1 + 2 + 3 + ... + n =
--
-- n * (n+1) / 2 = (n+1) `choose` 2
-- @
--
--

module Math.TriangularNumbers where



-- | Triangular numbers.
--
-- https://oeis.org/A000217

triangularNumber :: Int -> Int
triangularNumber x = (x * (x+1)) `quot` 2
{-# INLINE triangularNumber #-}

-- | Size of an upper triangle starting at 'i' and ending at 'j'. "(0,N)" what
-- be the normal thing to use.

upperTri :: Subword t -> Int
upperTri (Subword (i:.j)) = triangularNumber $ j-i+1
{-# INLINE upperTri #-}

-- | Subword indexing. Given the longest subword and the current subword,
-- calculate a linear index "[0,..]". "(l,n)" in this case means "l"ower bound,
-- length "n". And "(i,j)" is the normal index.
--
-- TODO probably doesn't work right with non-zero base ?!

subwordIndex :: Subword s -> Subword t -> Int
subwordIndex (Subword (l:.n)) (Subword (i:.j)) = adr n (i,j) -- - adr n (l,n)
  where
    adr n (i,j) = (n+1)*i - triangularNumber i + j
{-# INLINE subwordIndex #-}

