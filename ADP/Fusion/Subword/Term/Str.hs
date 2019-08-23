
module ADP.Fusion.Subword.Term.Str where

import           Data.Proxy
import           Data.Strict.Tuple
import           Debug.Trace
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADP.Fusion.Core
import           ADP.Fusion.Core.Term.Str
import           ADP.Fusion.Subword.Core



-- minSz done via TermStaticVar ?!

type instance LeftPosTy (IStatic   d) (Str linked minSz maxSz v x) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Str linked minSz maxSz v x) (Subword I) = IVariable d

{-
type instance LeftPosTy (OStatic d) (Chr r x) (PointL O) = OStatic (d+1)
-}

-- | 

instance
  forall pos posLeft m ls linked minSz maxSz v x i
  . ( TermStream m (Z:.pos) (TermSymbol M (Str linked minSz maxSz v x))
                 (Elm (Term1 (Elm ls (Subword i))) (Z :. Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos (Str linked minSz maxSz v x) (Subword i)
    , TermStaticVar pos (Str linked minSz maxSz v x) (Subword i)
    , MkStream m posLeft ls (Subword i)
    )
  ⇒ MkStream m pos (ls :!: Str linked minSz maxSz v x) (Subword i) where
  mkStream pos (ls :!: Str xs) grd us is
    = S.map (\(ss,ee,ii) -> ElmStr ee ii ss) -- recover ElmChr
    . addTermStream1 pos (Str @v @x @linked @minSz @maxSz xs) us is
    $ mkStream (Proxy ∷ Proxy posLeft) ls
               (termStaticCheck pos (Str @v @x @linked @minSz @maxSz xs) us is grd)
               us (termStreamIndex pos (Str @v @x @linked @minSz @maxSz xs) is)
  {-# Inline mkStream #-}

-- | Note that the @minSz@ should automatically work out due to the encoding in
-- @d@.

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  ) ⇒ TermStream m (ps:.IStatic d) (TermSymbol ts (Str Nothing minSz Nothing v x)) s (is:.Subword I) where
  termStream Proxy (ts:|Str xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) ->
                let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                in  TState s (ii:.:RiSwI j) (ee:.VG.unsafeSlice l (j-l) xs) )
    . termStream (Proxy ∷ Proxy ps) ts us is
  {-# Inline termStream #-}

-- |
--
-- X_ij -> S_ik Y_kj

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , KnownNat minSz
  ) ⇒ TermStream m (ps:.IVariable d) (TermSymbol ts (Str Nothing minSz Nothing v x)) s (is:.Subword I) where
  termStream Proxy (ts:|Str xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.flatten mk step . termStream (Proxy ∷ Proxy ps) ts us is
    where mk (tstate@(TState s ii ee)) =
            let RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I))
                msz     = fromIntegral $ natVal (Proxy ∷ Proxy minSz)
            in  return (tstate,k+msz)
          step (TState s ii ee, curK)
            | curK > j  = return $ S.Done
            | otherwise =
                let RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I))
                in  return $ S.Yield (TState s (ii:.:RiSwI curK) (ee:.VG.unsafeSlice k (curK-k) xs))
                                     (TState s ii ee, curK +1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}



instance (KnownNat minSz)
  ⇒ TermStaticVar (IStatic d) (Str Nothing minSz Nothing v x) (Subword I) where
  termStreamIndex Proxy (Str xs) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str xs) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

instance (KnownNat minSz)
  ⇒ TermStaticVar (IVariable d) (Str Nothing minSz Nothing v x) (Subword I) where
  termStreamIndex Proxy (Str xs) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str xs) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

{-
instance TermStaticVar (OStatic d) (Chr r x) (PointL O) where
  termStreamIndex Proxy (Chr f x) (PointL j) = PointL $ j
  termStaticCheck Proxy (Chr f x) (PointL j) = 1#
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}

