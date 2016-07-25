
-- | 

module Data.Combined.Foldable where

import Data.IntMap as IM
import Data.Foldable as F

import Data.Combined.Common



-- | Generalized upper triangular elements. Given a list of elements
-- @[e_1,...,e_k]@, we want to return pairs @(e_i,e_j)@ such that we have
-- all ordered pairs with @i<j@ (if @NoDiag@onal elements), or @i<=j@ (if
-- @OnDiag@onal elements).
--
-- In particular, we make sure that we access the elements in the input @t
-- a@ structure lazily. Assuming that the resulting list is forced at some
-- point, this should make sure that we do not do useless work in
-- evaluating elements of the input that are never used.
--
-- This is important if the @Enumerate@ type is set to @FromN k n@. We
-- start at the @k@th element, and produce @n@ elements.

upperTri
  :: (Foldable t)
  => OnDiag
  -> Enumerate
  -> t a
  -> (IntMap a, Int, [(a,a)])
upperTri d e xs' = (xmap, k, ys)
  where xs   = F.toList xs'
        xmap = undefined
        k    = undefined
        ys   = undefined
        strt = (undefined, undefined)

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

