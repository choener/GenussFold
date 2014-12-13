
{-# LANGUAGE TypeFamilies #-}

-- | Bijections via strict maps.

module Data.Bijection.Map where

import qualified Data.Map.Strict as S
import           Data.Tuple (swap)
import           Control.DeepSeq

import           Data.Bijection.Class



-- | A bijection between values of type @l@ and type @r@, implemented via
-- strict maps.

newtype Bimap l r = Bimap (S.Map l r, S.Map r l)

instance (Ord l, Ord r) => Bijection (Bimap l r) where
  type ContL (Bimap l r) = S.Map l r
  type ContR (Bimap l r) = S.Map r l
  type ElemL (Bimap l r) = l
  type ElemR (Bimap l r) = r
  contL (Bimap (l,r)) = l
  contR (Bimap (l,r)) = r
  lookupL (Bimap (l,r)) k = S.lookup k l
  lookupR (Bimap (l,r)) k = S.lookup k r
  empty = Bimap (S.empty,S.empty)
  null (Bimap (l,_)) = S.null l
  size (Bimap (l,_)) = S.size l
  fromList xs = Bimap (S.fromList xs, S.fromList $ map swap xs)
  toList (Bimap (l,_)) = S.toList l
  insert (Bimap (l,r)) (x,y) = Bimap (S.insert x y l, S.insert y x r)
  deleteByL (Bimap (l,r)) x =
    let r' = maybe r (`S.delete` r) $ S.lookup x l
        l' = S.delete x l
    in  Bimap (l',r')
  deleteByR (Bimap (l,r)) y =
    let l' = maybe l (`S.delete` l) $ S.lookup y r
        r' = S.delete y r
    in  Bimap (l',r')

instance (NFData l, NFData r) => NFData (Bimap l r) where
  rnf (Bimap (l,r)) = rnf (l,r)

