
module ADPfusion.Term.PeekIndex.Subword where

import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic (map)
import GHC.Exts
import Prelude hiding (map)

import Data.PrimitiveArray hiding (map)

import ADPfusion.Core
import ADPfusion.Core.Subword



instance
  ( Monad m
  , Element ls (Subword C)
  , MkStream m ls (Subword C)
  ) => MkStream m (ls :!: PeekIndex (Subword C)) (Subword C) where
  mkStream grd (ls :!: PeekIndex) Complemented h ij
    = map (\s -> let ri@(RiSwC k l) = getIdx s in ElmPeekIndex (subword k l) ri s)
    $ mkStream (grd `andI#` termStaticCheck (PeekIndex :: PeekIndex (Subword C)) ij) ls Complemented h ij
  {-# Inline mkStream #-}

instance TermStaticVar (PeekIndex (Subword C)) (Subword C) where

