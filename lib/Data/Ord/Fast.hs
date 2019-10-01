
{-# Language MagicHash #-}
{-# Language ForeignFunctionInterface #-}
{-# Language UnliftedFFITypes #-}
{-# Language GHCForeignImportPrim #-}
{-# Language UnboxedTuples #-}
{-# Language CPP #-}

-- | This module provides a small set of function for extremely fast @max@ and
-- @min@ operations that are branchless.
--
-- This should be temporary, since GHC is supposed to get the branchless variants.
--
-- NOTE these do not seem to be faster anyway.

module Data.Ord.Fast where

import GHC.Exts



class FastMinMax x where
  fastmin ∷ x → x → x
  fastmax ∷ x → x → x

instance FastMinMax Int where
  fastmin x' y' =
    let I# x = x'
        I# y = y'
        l    = x <# y
        res  = I# (  (x *# l) +# (y *# (1# -# l))  )
        --res  = I# ( x +# (x -# y) `andI#` uncheckedIShiftRA# (x -# y) 63# )
    in  res
  {-# Inline fastmin #-}
  fastmax x' y' =
    let I# x = x'
        I# y = y'
        l    = x <# y
        res  = I# (  (x *# (1# -# l)) +# (y *# l)  )
        --res = I# ( case (x>=# y) of { 0# -> y; 1# -> x } )
        --res  = I# ( x -# (x -# y) `andI#` uncheckedIShiftRA# (x -# y) 63# )
        --res = I# ( (x >=# y) *# x +# (x <# y) *# y )
    in  res
  {-# Inline fastmax #-}

