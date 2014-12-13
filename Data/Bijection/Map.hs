
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Bijections via strict maps.

module Data.Bijection.Map
  ( module Data.Bijection.Class
  , Bimap
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Tuple (swap)
import           GHC.Generics
import qualified Data.Map.Strict as S

import           Data.Bijection.Class



-- | A bijection between values of type @l@ and type @r@, implemented via
-- strict maps.

newtype Bimap l r = Bimap (S.Map l r, S.Map r l)
  deriving (Read,Show,Eq,Generic)

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
  {-# INLINE lookupL #-}
  {-# INLINE lookupR #-}

instance (NFData l, NFData r) => NFData (Bimap l r) where
  rnf (Bimap (l,r)) = rnf (l,r)

instance (Binary l, Binary r) => Binary (Bimap l r)
instance (Ord l, Ord r, Serialize l, Serialize r) => Serialize (Bimap l r)
instance (ToJSON (S.Map l r), ToJSON (S.Map r l)) => ToJSON (Bimap l r)
instance (FromJSON (S.Map l r), FromJSON (S.Map r l)) => FromJSON (Bimap l r)

