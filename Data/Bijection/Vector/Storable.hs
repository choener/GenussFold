
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A bijection between boxed, immutable vectors.

module Data.Bijection.Vector.Storable where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Serialize
import           Data.Tuple (swap)
import           Data.Vector.Binary
import           Data.Vector.Cereal
import           Data.Vector.Storable (Vector, Storable)
import           Foreign.Storable.Tuple
import           GHC.Generics
import qualified Data.Vector.Generic as G

import           Data.Bijection.Class



newtype Bimap l r = Bimap (Vector (l,r))
  deriving (Read,Show,Eq,Generic)

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

instance (Storable l, Storable r, Binary l, Binary r) => Binary (Bimap l r)
instance (Storable l, Storable r, Serialize l, Serialize r) => Serialize (Bimap l r)
instance (Storable l, Storable r, ToJSON l, ToJSON r) => ToJSON (Bimap l r)
instance (Storable l, Storable r, FromJSON l, FromJSON r) => FromJSON (Bimap l r)

