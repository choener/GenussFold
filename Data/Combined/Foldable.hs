
-- | 

module Data.Combined.Foldable where

import Data.IntMap as IM

import Data.Combined.Common



-- | Generalized upper triangular elements.

upperTri
  :: ()
  => OnDiag
  -> Enumerate
  -> f a
  -> (IntMap a, Int, f (a,a))
upperTri = undefined

