
module ADP.Fusion.Subword.Term.Epsilon where

import Data.Proxy
import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic as S
import GHC.Exts
import GHC.TypeLits
import Prelude hiding (map)

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Core
import ADP.Fusion.Subword.Core



type instance LeftPosTy (IStatic d) Epsilon (Subword I) = IStatic d

instance
  forall pos posLeft m ls i
  . ( TermStream m (Z:.pos) (TermSymbol M Epsilon) (Elm (Term1 (Elm ls (Subword i))) (Z:.Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos Epsilon (Subword i)
    , TermStaticVar pos Epsilon (Subword i)
    , MkStream m posLeft ls (Subword i)
  )
  ⇒ MkStream m pos (ls :!: Epsilon) (Subword i) where
  mkStream Proxy (ls :!: Epsilon) grd us is
    = map (\(ss,ee,ii) -> ElmEpsilon ii ss)
    . addTermStream1 (Proxy ∷ Proxy pos) Epsilon us is
    $ mkStream (Proxy ∷ Proxy posLeft)
               ls
               (termStaticCheck (Proxy ∷ Proxy pos) Epsilon is grd)
               us
               (termStreamIndex (Proxy ∷ Proxy pos) Epsilon is)
  {-# Inline mkStream #-}


instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  )
  ⇒ TermStream m (ps:.IStatic d) (TermSymbol ts Epsilon) s (is:.Subword I) where
  termStream Proxy (ts:|Epsilon) (us:..u) (is:.Subword (i:.j))
    = map (\(TState s ii ee) ->
              TState s (ii:.:RiSwI j) (ee:.()) )
    . termStream (Proxy ∷ Proxy ps) ts us is
    . staticCheck (i==j)
  {-# Inline termStream #-}


{-
instance
  ( TermStreamContext m ts s xi0 i0 is (Subword O)
  ) => TermStream m (TermSymbol ts Epsilon) s (is:.Subword O) where
  termStream (ts:|Epsilon) (cs:.OStatic d) (us:.Subword (ui:.uj)) (is:.Subword (i:.j))
    = staticCheck (ui == i && uj == j) -- TODO correct ?
    . map (\(TState s ii ee) ->
              let io = getIndex (getIdx s) (Proxy :: PRI is (Subword O))
              in  TState s (ii:.:io) (ee:.()) )
    . termStream ts cs us is
  {-# Inline termStream #-}
-}


instance TermStaticVar (IStatic 0) Epsilon (Subword I) where
  termStreamIndex Proxy Epsilon ij = ij
  termStaticCheck Proxy Epsilon ij grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

{-
instance TermStaticVar Epsilon (Subword O) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ ij = ij
  termStaticCheck _ _ = 1#
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}

