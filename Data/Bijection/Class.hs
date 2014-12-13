
{-# LANGUAGE TypeFamilies #-}

module Data.Bijection.Class where



class Bijection z where
  type ContL z :: *
  type ContR z :: *
  type ElemL z :: *
  type ElemR z :: *
  contL :: z -> ContL z
  contR :: z -> ContR z
  memberL :: z -> ElemL z -> Bool
  memberL = (maybe False (const True) . ) . lookupL
  memberR :: z -> ElemR z -> Bool
  memberR = (maybe False (const True) . ) . lookupR
  lookupL :: z -> ElemL z -> Maybe (ElemR z)
  lookupR :: z -> ElemR z -> Maybe (ElemL z)
  empty :: z
  null :: z -> Bool
  size :: z -> Int
  fromList :: [(ElemL z, ElemR z)] -> z
  toList :: z -> [(ElemL z, ElemR z)]
  insert :: z -> (ElemL z, ElemR z) -> z
  deleteByL :: z -> ElemL z -> z
  deleteByR :: z -> ElemR z -> z

findWithDefaultL :: Bijection z => ElemR z -> z -> ElemL z -> ElemR z
findWithDefaultL def = (maybe def id . ) . lookupL
{-# INLINE findWithDefaultL #-}

findWithDefaultR :: Bijection z => ElemL z -> z -> ElemR z -> ElemL z
findWithDefaultR def = (maybe def id . ) . lookupR
{-# INLINE findWithDefaultR #-}

