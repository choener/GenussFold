
module ADPfusion.Subword.Term.Deletion where

import Data.Proxy
import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic as S
import GHC.Exts
import Prelude hiding (map)

import Data.PrimitiveArray hiding (map)

import ADPfusion.Core
import ADPfusion.Subword.Core



type instance LeftPosTy (IStatic d)   Deletion (Subword I) = IStatic d
type instance LeftPosTy (IVariable d) Deletion (Subword I) = IVariable d

instance
  forall pos posLeft m ls i
  . ( TermStream m (Z:.pos) (TermSymbol M Deletion) (Elm (Term1 (Elm ls (Subword i))) (Z:.Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos Deletion (Subword i)
    , TermStaticVar pos Deletion (Subword i)
    , MkStream m posLeft ls (Subword i)
  )
  => MkStream m pos (ls :!: Deletion) (Subword i) where
  mkStream Proxy (ls :!: Deletion) grd us is
    = map (\(ss,ee,ii) -> ElmDeletion ii ss)
    . addTermStream1 (Proxy :: Proxy pos) Deletion us is
    $ mkStream (Proxy :: Proxy posLeft)
               ls
               (termStaticCheck (Proxy :: Proxy pos) Deletion us is grd)
               us
               (termStreamIndex (Proxy :: Proxy pos) Deletion is)
  {-# Inline mkStream #-}



instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  )
  => TermStream m (ps:.IStatic d) (TermSymbol ts Deletion) s (is:.Subword I) where
  termStream Proxy (ts:|Deletion) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) -> TState s (ii:.:RiSwI j) (ee:.()) )
    . termStream (Proxy :: Proxy ps) ts us is
  {-# Inline termStream #-}

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  )
  => TermStream m (ps:.IVariable d) (TermSymbol ts Deletion) s (is:.Subword I) where
  termStream Proxy (ts:|Deletion) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) ->
                let l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                in  TState s (ii:.:l) (ee:.()) )
    . termStream (Proxy :: Proxy ps) ts us is
  {-# Inline termStream #-}

{-
instance
  ( TermStreamContext m ts s x0 i0 is (Subword O)
  ) => TermStream m (TermSymbol ts Deletion) s (is:.Subword O) where
  -- X_ij  -> Y_ik  Z_kj  d_jj        0   i Y k Z j-j   N
  -- Y^_ik -> X^_ij Z_kj  d_jj        0 x i   k Z j-j x N
  -- Z^_kj -> Y_ik  X^_ij d_jj        0 x i Y k   j-j x N
  termStream (ts:|Deletion) (cs:._) (us:.u) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) ->
                let RiSwO _ k oi oj = getIndex (getIdx s) (Proxy :: PRI is (Subword O))
                in  TState s (ii:.:RiSwO k k oi oj) (ee:.()) )
    . termStream ts cs us is
  {-
  termStream (ts:|Deletion) (cs:.OStatic (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = S.map (\(TState s a ii ee) ->
                let RiSwO _ k oi oj = getIndex a (Proxy :: PRI is (Subword O))
                in  TState s a (ii:.:RiSwO k k oi oj) (ee:.()) )
    . termStream ts cs us is
  --
  termStream (ts:|Deletion) (cs:.ORightOf (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = S.map (\(TState s a ii ee) ->
                let RiSwO _ k oi oj = getIndex a (Proxy :: PRI is (Subword O))
                in  TState s a (ii:.:RiSwO k k oi oj) (ee:.()) )
    . termStream ts cs us is
  --
  termStream (ts:|Deletion) (cs:.OFirstLeft (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = S.map (\(TState s a ii ee) ->
                let RiSwO _ k oi oj = getIndex a (Proxy :: PRI is (Subword O))
                in  TState s a (ii:.:RiSwO k k oi oj) (ee:.()) )
    . termStream ts cs us is
  --
  termStream (ts:|Deletion) (cs:.OLeftOf (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = S.map (\(TState s a ii ee) ->
                let RiSwO _ k oi oj = getIndex a (Proxy :: PRI is (Subword O))
                in  TState s a (ii:.: RiSwO k k oi oj) (ee:.()) )
    . termStream ts cs us is
  -}
  {-# Inline termStream #-}
-}


instance TermStaticVar (IStatic 0) Deletion (Subword I) where
  termStreamIndex Proxy Deletion ij = ij
  termStaticCheck Proxy Deletion _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

instance TermStaticVar (IVariable d) Deletion (Subword I) where
  termStreamIndex Proxy Deletion ij = ij
  termStaticCheck Proxy Deletion _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

