
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A bijection between boxed, immutable vectors.

module Data.Bijection.Vector.Storable where

import qualified Data.Vector.Generic as G
import           Data.Vector.Storable (Vector, Storable)
import           Data.Tuple (swap)
import           Control.Applicative ((<$>))
import           Foreign.Storable.Tuple
import           Control.DeepSeq

import           Data.Bijection.Class



newtype Bimap l r = Bimap (Vector (l,r))

instance (Eq l, Eq r, Storable l, Storable r, Storable (l,r)) => Bijection (Bimap l r) where
  type ContL (Bimap l r) = Vector (l,r)
  type ContR (Bimap l r) = Vector (r,l)
  type ElemL (Bimap l r) = l
  type ElemR (Bimap l r) = r
  contL (Bimap v) = v
  contR (Bimap v) = G.map swap v
  lookupL (Bimap v) k = snd <$> G.find ((==k) . fst) v
  lookupR (Bimap v) k = fst <$> G.find ((==k) . snd) v
  empty = Bimap G.empty
  null (Bimap v) = G.null v
  size (Bimap v) = G.length v
  fromList = Bimap . G.fromList
  toList (Bimap v) = G.toList v
  insert (Bimap v) = Bimap . G.snoc v
  deleteByL (Bimap v) x = Bimap $ G.filter ((/=x) . fst) v
  deleteByR (Bimap v) y = Bimap $ G.filter ((/=y) . snd) v

instance (NFData l, NFData r) => NFData (Bimap l r) where
  rnf (Bimap v) = rnf v

