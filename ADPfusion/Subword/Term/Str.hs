
module ADPfusion.Subword.Term.Str where

import           Data.Proxy
import           Data.Strict.Tuple
import           Data.Type.Equality
import           Debug.Trace
import           GHC.Exts
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADPfusion.Core
import           ADPfusion.Core.Term.Str
import           ADPfusion.Subword.Core



-- minSz done via TermStaticVar ?!

type instance LeftPosTy (IStatic   d) (Str linked minSz maxSz v x r) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Str linked minSz maxSz v x r) (Subword I) = IVariable d

{-
type instance LeftPosTy (OStatic d) (Chr r x) (PointL O) = OStatic (d+1)
-}

-- | 

instance
  forall pos posLeft m ls linked minSz maxSz v x r i
  . ( TermStream m (Z:.pos) (TermSymbol M (Str linked minSz maxSz v x r))
                 (Elm (Term1 (Elm ls (Subword i))) (Z :. Subword i)) (Z:.Subword i)
    , posLeft ~ LeftPosTy pos (Str linked minSz maxSz v x r) (Subword i)
    , TermStaticVar pos (Str linked minSz maxSz v x r) (Subword i)
    , MkStream m posLeft ls (Subword i)
    )
  => MkStream m pos (ls :!: Str linked minSz maxSz v x r) (Subword i) where
  mkStream pos (ls :!: Str f xs) grd us is
    = S.map (\(ss,ee,ii) -> ElmStr ee ii ss)
    . addTermStream1 pos (Str @v @x @r @linked @minSz @maxSz f xs) us is
    $ mkStream (Proxy :: Proxy posLeft) ls
               (termStaticCheck pos (Str  @v @x @r @linked @minSz @maxSz f xs) us is grd)
               us (termStreamIndex pos (Str  @v @x @r @linked @minSz @maxSz f xs) is)
  {-# Inline mkStream #-}

-- | Note that the @minSz@ should automatically work out due to the encoding in
-- @d@ / @termStaticVar@
--
-- NOTE The @maybeMaxSz@ check makes the assumption that in case of @maxSz == Nothing@, stream
-- fusion can fully elimiate the @S.mapMaybe / Just@ part. This should work, since it ends up being
-- case of known constructor.

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , MaybeMaxSz maxSz
  ) => TermStream m (ps:.IStatic d) (TermSymbol ts (Str linked minSz maxSz v x r)) s (is:.Subword I) where
  termStream Proxy (ts:|Str f xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.mapMaybe (\(TState s ii ee) -> let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
        in maybeMaxSz (Proxy @maxSz) (j-l) $ TState s (ii:.:RiSwI j) (ee:.f xs l j) )
--    = S.map (\(TState s ii ee) -> let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
--        in TState s (ii:.:RiSwI j) (ee:.f xs l j) )
    . termStream (Proxy ∷ Proxy ps) ts us is
  {-# Inline termStream #-}



-- | The linked name is non-empty, hence we now need to sum up linked sizes.

--instance LinkedSz False linked (Term1 (Elm S (Subword I))) (Z:.Subword I) where
--  {-# Inline linkedSz #-}
--  linkedSz _ _ _ = 0

instance ( LinkedSz False p ls (Subword I), Element ls (Subword I) )
  => LinkedSzEq 'True p (ls :!: Str linked minSz maxSz v x r) (Subword I) where
  {-# Inline linkedSzEq #-}
  -- We have the correct type @Str@ and the same non-empty linked annotation. Extract the previous
  -- running index, and the current running index. Calculate their size difference, and recursively
  -- add more linked sizes.
  linkedSzEq _ _ (ElmStr _ (RiSwI j) ls) = let RiSwI i = getIdx ls in (j-i) + linkedSz (Proxy @False) (Proxy @p) ls

-- | This @Str@ does NOT have the same type-level string annotation

instance ( LinkedSz False p ls i )
  => LinkedSzEq 'False p (ls :!: Str linked minSz maxSz v x r) i where
  {-# Inline linkedSzEq #-}
  linkedSzEq _ _ (ElmStr _ _ ls) = linkedSz (Proxy @False) (Proxy @p) ls



-- |
--
-- X_ij -> S_ik Y_kj

instance
  ( TermStreamContext m ps ts s x0 i0 is (Subword I)
  , KnownNat minSz, KnownSymbol linked, MaybeMaxSz maxSz, LinkedSz (linked == "") linked x0 i0
  ) => TermStream m (ps:.IVariable d) (TermSymbol ts (Str linked minSz maxSz v x r)) s (is:.Subword I) where
  termStream Proxy (ts:|Str f xs) (us:..LtSubword u) (is:.Subword (i:.j))
    = S.flatten mk step . termStream (Proxy ∷ Proxy ps) ts us is
    where mk (tstate@(TState s ii ee)) =
            let !msz = fromIntegral $ natVal (Proxy ∷ Proxy minSz)
            in  return (tstate,msz)
          step (TState s ii ee, !sz)
            | ksz > j || gtMaxSz (Proxy @maxSz) (lsz+sz) = return $ S.Done
            | otherwise = return $ S.Yield (TState s (ii:.:RiSwI ksz) (ee:.f xs k ksz))
                                           (TState s ii ee, sz+1)
            where RiSwI k = getIndex (getIdx s) (Proxy ∷ PRI is (Subword I))
                  ksz = k+sz
                  lsz = linkedSz (Proxy @(linked == "")) (Proxy @linked) s
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

instance (KnownNat minSz)
  => TermStaticVar (IStatic d) (Str linked minSz maxSz v x r) (Subword I) where
  termStreamIndex Proxy (Str _ _) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str _ _) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

instance (KnownNat minSz)
  => TermStaticVar (IVariable d) (Str linked minSz maxSz v x r) (Subword I) where
  termStreamIndex Proxy (Str _ _) (Subword (i:.j)) = subword i $ j - fromIntegral (natVal (Proxy ∷ Proxy minSz))
  termStaticCheck Proxy (Str _ _) _ _ grd = grd
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

{-
instance TermStaticVar (OStatic d) (Chr r x) (PointL O) where
  termStreamIndex Proxy (Chr f x) (PointL j) = PointL $ j
  termStaticCheck Proxy (Chr f x) (PointL j) = 1#
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}
-}

