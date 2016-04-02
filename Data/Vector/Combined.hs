
module Data.Vector.Combined where

import Data.Vector.Generic as VG



-- | Shall we combine elements on the main diagonal as well?

data OnDiag = OnDiag | NoDiag
  deriving (Eq)

-- | Upper triangular elements.

upperTriVG
  :: (Vector v a, Vector w (a,a))
  => OnDiag
  -> v a
  -> w (a,a)
upperTriVG d as = unfoldrN z go (0,if d == OnDiag then 0 else 1)
  where la = VG.length as
        z  = la * (la + if d == OnDiag then 1 else 0) `div` 2
        go (k,l)
          | k >= la   = Nothing
          | l >= la   = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just ((as `VG.unsafeIndex` k, as `VG.unsafeIndex` l), (k,l+1))
{-# Inline upperTriVG #-}

