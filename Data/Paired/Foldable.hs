
-- | 

module Data.Paired.Foldable where

import Data.IntMap as IM
import Data.Foldable as F
import Data.List as L
import Control.Arrow ((***))

import Data.Paired.Common
import Math.TriangularNumbers



-- | Generalized upper triangular elements. Given a list of elements
-- @[e_1,...,e_k]@, we want to return pairs @(e_i,e_j)@ such that we have
-- all ordered pairs with @i<j@ (if @NoDiag@onal elements), or @i<=j@ (if
-- @OnDiag@onal elements).
--
-- @upperTri@ will force the spine of @t a@.
--
-- This is important if the @Enumerate@ type is set to @FromN k n@. We
-- start at the @k@th element, and produce @n@ elements.

upperTri
  :: (Foldable t)
  => OnDiag
  -> Enumerate
  -> t a
  -> (IntMap a, Int, [(a,a)])
upperTri d e xs' = undefined $ initEnum e d
  where xs   = F.toList xs'
        ys   = L.unfoldr go undefined
        imp  = undefined
        go _ = Nothing
        -- Initialize the enumeration at the correct pair @(i,j)@. From
        -- then on we can @take@ the correct number of elements, or stream
        -- all of them.
        initEnum All OnDiag = (0,0)
        initEnum All NoDiag = (0,1)
        initEnum (FromN s k) OnDiag = fromLinear sz s
        initEnum (FromN s k) NoDiag = id *** (+1) $ fromLinear (sz-1) s
        sz = F.length xs'

{-
upperTriVG d as = (z, unfoldrN z go (0,if d == OnDiag then 0 else 1))
  where la = VG.length as
        z  = la * (la + if d == OnDiag then 1 else 0) `div` 2
        go (k,l)
          | k >= la   = Nothing
          | l >= la   = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just ((as `VG.unsafeIndex` k, as `VG.unsafeIndex` l), (k,l+1))
{-# Inline upperTriVG #-}
-}

