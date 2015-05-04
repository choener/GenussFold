
module Data.Bijection.Hash
  ( module Data.Bijection.Class
  , Bimap
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



-- | A bijection between values of type @l@ and type @r@.

newtype Bimap l r = Bimap (H.HashMap l r, H.HashMap r l)
  deriving (Read,Show,Eq,Generic)

instance (Eq l, Eq r, Hashable l, Hashable r) => Bijection (Bimap l r) where
  type ContL (Bimap l r) = H.HashMap l r
  type ContR (Bimap l r) = H.HashMap r l
  type ElemL (Bimap l r) = l
  type ElemR (Bimap l r) = r
  contL (Bimap (l,r)) = l
  contR (Bimap (l,r)) = r
  lookupL (Bimap (l,r)) k = H.lookup k l
  lookupR (Bimap (l,r)) k = H.lookup k r
  empty = Bimap (H.empty, H.empty)
  null (Bimap (l,_)) = H.null l
  size (Bimap (l,_)) = H.size l
  fromList xs = Bimap (H.fromList xs, H.fromList $ map swap xs)
  toList (Bimap (l,_)) = H.toList l
  insert (Bimap (l,r)) (x,y) = Bimap (H.insert x y l, H.insert y x r)
  deleteByL (Bimap (l,r)) x =
    let r' = maybe r (`H.delete` r) $ H.lookup x l
        l' = H.delete x l
    in  Bimap (l',r')
  deleteByR (Bimap (l,r)) y =
    let l' = maybe l (`H.delete` l) $ H.lookup y r
        r' = H.delete y r
    in  Bimap (l',r')
  {-# INLINE lookupL #-}
  {-# INLINE lookupR #-}

instance (NFData l, NFData r) => NFData (Bimap l r) where
  rnf (Bimap (l,r)) = rnf (l,r)

instance (Binary (H.HashMap l r), Binary (H.HashMap r l)) => Binary (Bimap l r)
instance (Ord l, Ord r, Serialize (H.HashMap l r), Serialize (H.HashMap r l)) => Serialize (Bimap l r)
instance (ToJSON (H.HashMap l r), ToJSON (H.HashMap r l)) => ToJSON (Bimap l r)
instance (FromJSON (H.HashMap l r), FromJSON (H.HashMap r l)) => FromJSON (Bimap l r)

