
-- | Instances to allow 'Subword's to be used as index structures in
-- @ADPfusion@.

module ADPfusion.Subword.Core where

import Data.Vector.Fusion.Stream.Monadic (singleton,filter,enumFromStepN,map,unfoldr)
import Debug.Trace
import Prelude hiding (map,filter)
import GHC.Exts

import Data.PrimitiveArray hiding (map)

import ADPfusion.Core



type instance InitialContext (Subword I) = IStatic 0

type instance InitialContext (Subword O) = OStatic 0

{-

instance RuleContext (Subword I) where
  type Context (Subword I) = InsideContext ()
  initialContext _ = IStatic ()
  {-# Inline initialContext #-}

instance RuleContext (Subword O) where
  type Context (Subword O) = OutsideContext (Int:.Int)
  initialContext _ = OStatic (0:.0)
  {-# Inline  initialContext #-}

instance RuleContext (Subword C) where
  type Context (Subword C) = ComplementContext
  initialContext _ = Complemented
  {-# Inline initialContext #-}
-}

-- | The moving index @k@ in @Subword (i:.k)@.

newtype instance RunningIndex (Subword I) = RiSwI Int

-- | The moving indices @Inside (i:.j)@ and @Outside (k:.l)@ in order @i
-- j k l@.
--
-- TODO can we do with 2x Int?

data instance RunningIndex (Subword O) = RiSwO !Int !Int !Int !Int

-- | The indices @Subword (i:.j)@ in order @i j@.

data instance RunningIndex (Subword C) = RiSwC !Int !Int



-- | NOTE it seems that a static check within an @IVariable@ context
-- destroys fusion; maybe because of the outer flatten? We don't actually
-- need a static check anyway because the next flatten takes care of
-- conditional checks. @filter@ on the other hand, does work.
--
-- TODO test with and without filter using quickcheck
--
-- TODO shouldn't the new @staticCheck@ impl handle this?

instance
  ( Monad m
  , KnownNat d
  )
  ⇒ MkStream m (IStatic d) S (Subword I) where
  mkStream Proxy S grd (LtSubword (I# h)) (Subword (I# i:.I# j))
    = staticCheck# (grd `andI#` (0# <=# i) `andI#` (i ==# j))
    . singleton
    . ElmS $ RiSwI (I# i)
  {-# Inline mkStream #-}

instance
  ( Monad m
  )
  ⇒ MkStream m (IVariable d) S (Subword I) where
  mkStream Proxy S grd (LtSubword (I# h)) (Subword (I# i:.I# j))
    = staticCheck# (grd `andI#` (0# <=# i) `andI#` (i <=# j))
    . singleton
    . ElmS $ RiSwI (I# i)
  {-# Inline mkStream #-}

{-
instance (Monad m) => MkStream m S (Subword O) where
  mkStream grd S (OStatic (di:.I# dj)) (Subword (_:.I# h)) (Subword (I# i:.I# j))
    = staticCheck# (grd `andI#` (i ==# 0#) `andI#` ((j +# dj) ==# h))
    . singleton
    . ElmS $ RiSwO (I# i) (I# j)  (I# i) (I# (j +# dj))
  mkStream grd S (OFirstLeft (I# di:.I# dj)) (Subword (_:.I# h)) (Subword (I# i:.I# j))
    = staticCheck# (grd `andI#` (0# <=# i') `andI#` (i <=# j) `andI#` ((j +# dj) <=# h))
    . singleton
    . ElmS $ RiSwO (I# i') (I# i') (I# i') (I# i')
    where i' = i -# di
  mkStream grd S (OLeftOf (I# di:.I# dj)) (Subword (_:.I# h)) (Subword (I# i:.I# j))
    = staticCheck# (grd `andI#` (0# <=# i') `andI#` (i <=# j) `andI#` ((j +# dj) <=# h))
    . map (\k -> ElmS $ RiSwO 0 k k (I# j))
    $ enumFromStepN 0 1 (I# i' + 1)
    where i' = i -# di
  mkStream grd S e _ _ = error $ show e ++ "maybe only inside syntactic terminals on the RHS of an outside rule?" -- TODO mostly because I'm not sure if that would be useful
  {-# Inline mkStream #-}

-- | 
--
-- TODO The @go@ here needs an explanation.

instance (Monad m) => MkStream m S (Subword C) where
  mkStream grd S Complemented (Subword (_:.h)) (Subword (i:.j))
    = staticCheck# grd
    . map (\(k,l) -> ElmS $ RiSwC k l)
    $ unfoldr go (i,i)
    where go (k,l)
            | k >h || k >j = Nothing
            | l==h || l==j = Just ( (k,l) , (k+1,k+1) )
            | otherwise    = Just ( (k,l) , (k  ,l+1) )
          {-# Inline [0] go #-}
  {-# Inline mkStream #-}
-}

instance
  ( Monad m
  , MkStream m ps S is
  ) => MkStream m (ps:.IStatic d) S (is:.Subword I) where
  mkStream Proxy S grd (lus:..LtSubword (I# h)) (ixs:.Subword(I# i:.I# j))
    = map (\(ElmS zi) -> ElmS (zi:.:RiSwI (I# i)))
    $ mkStream (Proxy ∷ Proxy ps) S (grd `andI#` (0# <=# i) `andI#` (i ==# j)) lus ixs
  {-# Inline mkStream #-}

instance
  ( Monad m
  , MkStream m ps S is
  ) => MkStream m (ps:.IVariable d) S (is:.Subword I) where
  mkStream Proxy S grd (lus:..LtSubword (I# h)) (ixs:.Subword(I# i:.I# j))
    = map (\(ElmS zi) -> ElmS (zi:.:RiSwI (I# i)))
    $ mkStream (Proxy ∷ Proxy ps) S (grd `andI#` (0# <=# i) `andI#` (i <=# j)) lus ixs
  {-# Inline mkStream #-}

instance (MinSize minSize) => TableStaticVar pos minSize u (Subword I) where
  tableStreamIndex Proxy minSz _ (Subword (i:.j)) = subword i (j - minSize minSz)
  {-# INLINE [0] tableStreamIndex #-}

{-
-- | This instance is chosen if we consider an outside table (i.e.
-- a syntactic variable) in an outside index.
--
-- TODO @tableStreamIndex@ needs to be fixed

instance TableStaticVar (u O) c (Subword O) where
  tableStaticVar _ _ (OStatic  d) _ = OFirstLeft d
  tableStaticVar _ _ (ORightOf d) _ = OFirstLeft d
  tableStreamIndex _ c _ (Subword (i:.j)) = subword i j
  {-# INLINE [0] tableStaticVar   #-}
  {-# INLINE [0] tableStreamIndex #-}

-- | This instance is chosen if we consider an inside table (i.e.
-- a terminal symbol!) in an outside index.
--
-- TODO @tableStreamIndex@ needs to be fixed

instance TableStaticVar (u I) c (Subword O) where
  tableStaticVar _ _ (OStatic    d) _ = ORightOf d
  tableStaticVar _ _ (ORightOf   d) _ = ORightOf d
  tableStaticVar _ _ (OFirstLeft d) _ = OLeftOf d
  tableStaticVar _ _ (OLeftOf    d) _ = OLeftOf d
  tableStreamIndex _ c _ (Subword (i:.j)) = subword i j
  {-# INLINE [0] tableStaticVar   #-}
  {-# INLINE [0] tableStreamIndex #-}

instance TableStaticVar (u I) c (Subword C) where
  tableStaticVar _ _ _ _ = Complemented
  tableStreamIndex _ c _ (Subword (i:.j)) = subword i j
  {-# INLINE [0] tableStaticVar   #-}
  {-# INLINE [0] tableStreamIndex #-}

instance TableStaticVar (u O) c (Subword C) where
  tableStaticVar _ _ _ _ = Complemented
  tableStreamIndex _ c _ (Subword (i:.j)) = subword i j
  {-# INLINE [0] tableStaticVar   #-}
  {-# INLINE [0] tableStreamIndex #-}

-}

-- * Index Conversion

instance IndexConversion (Z:.Subword I:.Subword I) (Z:.Subword I:.Subword I) where
--{{{
  {-# Inline convertIndex #-}
  convertIndex = Just
--}}}

instance IndexConversion (Z:.Subword I:.Subword I) (Subword I) where
--{{{
  {-# Inline convertIndex #-}
  convertIndex (Z:.ij:.kl)
    | ij==kl = Just ij
    | otherwise = Nothing
--}}}

