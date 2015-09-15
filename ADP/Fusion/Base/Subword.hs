
-- | Instances to allow 'Subword's to be used as index structures in
-- @ADPfusion@.

module ADP.Fusion.Base.Subword where

import Data.Vector.Fusion.Stream.Monadic (singleton,filter,enumFromStepN,map,unfoldr)
import Data.Vector.Fusion.Stream.Size
import Debug.Trace
import Prelude hiding (map,filter)

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Base.Classes
import ADP.Fusion.Base.Multi



instance RuleContext Subword where
  type Context Subword = InsideContext ()
  initialContext _ = IStatic ()
  {-# Inline initialContext #-}

instance RuleContext (Outside Subword) where
  type Context (Outside Subword) = OutsideContext (Int:.Int)
  initialContext _ = OStatic (0:.0)
  {-# Inline  initialContext #-}

instance RuleContext (Complement Subword) where
  type Context (Complement Subword) = ComplementContext
  initialContext _ = Complemented
  {-# Inline initialContext #-}

-- TODO write instance

-- instance RuleContext (Complement Subword)



instance (Monad m) => MkStream m S Subword where
  mkStream S (IStatic ()) (Subword (_:.h)) (Subword (i:.j))
    = staticCheck (i>=0 && i==j && j<=h)
    . singleton
    $ ElmS (subword i i) (subword 0 0)
  -- NOTE it seems that a static check within an @IVariable@ context
  -- destroys fusion; maybe because of the outer flatten? We don't actually
  -- need a static check anyway because the next flatten takes care of
  -- conditional checks. @filter@ on the other hand, does work.
  -- TODO test with and without filter using quickcheck
  mkStream S (IVariable ()) (Subword (_:.h)) (Subword (i:.j))
    = filter (const $ 0<=i && i<=j && j<=h) . singleton $ ElmS (subword i i) (subword 0 0)
  {-# Inline mkStream #-}

instance (Monad m) => MkStream m S (Outside Subword) where
  mkStream S (OStatic (di:.dj)) (O (Subword (_:.h))) (O (Subword (i:.j)))
    = staticCheck (i==0 && j+dj==h) . singleton $ ElmS (O $ subword i j) (O $ Subword (i:.j+dj))
  mkStream S (OFirstLeft (di:.dj)) (O (Subword (_:.h))) (O (Subword (i:.j)))
    = let i' = i-di
      in  staticCheck (0 <= i' && i<=j && j+dj<=h) . singleton $ ElmS (O $ subword i' i') (O $ subword i' i')
  mkStream S (OLeftOf (di:.dj)) (O (Subword (_:.h))) (O (Subword (i:.j)))
    = let i' = i-di
      in  staticCheck (0 <= i' && i<=j && j+dj<=h)
    $ map (\k -> ElmS (O $ subword 0 k) (O $ subword k j))
    $ enumFromStepN 0 1 (i'+1)
  {-# Inline mkStream #-}

instance (Monad m) => MkStream m S (Complement Subword) where
  mkStream S Complemented (C (Subword (_:.h))) (C (Subword (i:.j)))
    = map (\(k,l) -> ElmS (C $ subword k l) (C $ subword k l))
    $ unfoldr go (i,i)
    where go (k,l)
            | k >h || k >j = Nothing
            | l==h || l==j = Just ( (k,l) , (k+1,k+1) )
            | otherwise    = Just ( (k,l) , (k  ,l+1) )
          {-# Inline [0] go #-}
  {-# Inline mkStream #-}



instance
  ( Monad m
  , MkStream m S is
  , Context (is:.Subword) ~ (Context is:.(InsideContext ()))
  ) => MkStream m S (is:.Subword) where
  mkStream S (vs:.IStatic ()) (lus:.Subword (_:.h)) (ixs:.Subword(i:.j))
    = staticCheck (i>=0 && i==j && j<=h)
    . map (\(ElmS zi zo) -> ElmS (zi:.subword i i) (zo:.subword 0 0))
    $ mkStream S vs lus ixs
  mkStream S (vs:.IVariable ()) (lus:.Subword (_:.h)) (ixs:.Subword (i:.j))
    = map (\(ElmS zi zo) -> ElmS (zi:.subword i i) (zo:.subword 0 0))
    . filter (const $ 0<=i && i<=j && j<=h)
    $ mkStream S vs lus ixs
  {-# Inline mkStream #-}

instance TableStaticVar Subword where
  tableStaticVar (IStatic   d) _ = IVariable d
  tableStaticVar (IVariable d) _ = IVariable d
  tableStreamIndex c _ (Subword (i:.j))
    | c==EmptyOk  = subword i j
    | c==NonEmpty = subword i (j-1)
    | c==NonEmpty = error "A.F.B.Subword ???"
  {-# INLINE [0] tableStaticVar   #-}
  {-# INLINE [0] tableStreamIndex #-}

