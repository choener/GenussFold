
{-# Language MagicHash #-}

-- | This module provides a small set of function for extremely fast @max@ and
-- @min@ operations that are branchless.
--
-- This should be temporary, since GHC is supposed to get the branchless variants.

module Data.Ord.Fast where

import GHC.Exts



class FastMinMax x where
  fastmin ∷ x → x → x
  fastmax ∷ x → x → x

instance FastMinMax Int where
  fastmin x' y' =
    let I# x = coerce x' ∷ Int
        I# y = coerce y' ∷ Int
        l    = x <# y
        res  = I# (  (x *# l) +# (y *# (1# -# l))  )
    in  coerce res
  {-# Inline fastmin #-}
  fastmax x' y' =
    let I# x = coerce x' ∷ Int
        I# y = coerce y' ∷ Int
        l    = x <# y
        res  = I# (  (x *# (1# -# l)) +# (y *# l)  )
    in  coerce res
  {-# Inline fastmax #-}

