
module Data.Bijection.Hash
  ( module Data.Bijection.Class
  , H.HashMap
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Serialize
import           Data.Tuple (swap)
import           GHC.Generics
import qualified Data.HashMap.Strict as H

import           Data.Bijection.Class



instance (Eq d, Hashable d) => DomCod (H.HashMap d c) where
  type Dom (H.HashMap d c) = d
  type Cod (H.HashMap d c) = c
  member h k = H.member k h
  lookup h k = H.lookup k h
  deleteDC h k = (,H.delete k h) <$> H.lookup k h
  insertDC h (d,c) = H.insert d c h
  toListDC = H.toList
  nullDC = H.null
  emptyDC = H.empty
  sizeDC = H.size
  fromListDC = H.fromList

