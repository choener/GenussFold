
module ADP.Fusion.SynVar.Recursive.Type where

import Control.Monad.Morph
import Data.Proxy
import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic (Stream,head,map,mapM)
import Prelude hiding (head,map,mapM)

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Base
import ADP.Fusion.SynVar.Axiom
import ADP.Fusion.SynVar.Backtrack
import ADP.Fusion.SynVar.Indices



-- | A syntactic variable that does not memoize but simplify recurses. One
-- needs to be somewhat careful when using this one. @ITbl@ performs
-- memoization to perform DP in polynomial time (roughly speaking). If the
-- rules for an @IRec@ are of a particular type, they will exponential
-- running time. Things like @X -> X X@ are, for example, rather bad. Rules
-- of the type @X -> Y, Y -> Z@ are ok, if @Y@ is an @IRec@ since we just
-- continue on. The same holds for @Y -> a Y@. Basically, things are safe
-- if there is only a (small) constant number of parses of an @IRec@
-- synvar.

data IRec m c i x where
  IRec :: { iRecConstraint  :: !c
          , iRecFrom        :: !i
          , iRecTo          :: !i
          , iRecFun         :: !(i -> i -> m x)
          } -> IRec m c i x

instance Build (IRec m c i x)

type instance TermArg (IRec m c i x) = x

instance GenBacktrackTable (IRec mF c i x) mF mB r where
  data Backtrack (IRec mF c i x) mF mB r = BtIRec !c !i !i !(i -> i -> mB x) !(i -> i -> mB [r])
  type BacktrackIndex (IRec mF c i x) = i
  toBacktrack (IRec c iF iT f) mrph bt = BtIRec c iF iT (\lu i -> mrph $ f lu i) bt
  {-# Inline toBacktrack #-}



instance
  ( Monad m
  , IndexStream i
  ) => Axiom (IRec m c i x) where
  type AxiomStream (IRec m c i x) = m x
  axiom (IRec _ l h fun) = do
    k <- head $ streamDown l h
    fun h k
  {-# Inline axiom #-}

instance
  ( Monad mB
  , IndexStream i
  ) => Axiom (Backtrack (IRec mF c i x) mF mB r) where
  type AxiomStream (Backtrack (IRec mF c i x) mF mB r) = mB [r]
  axiom (BtIRec c l h fun btfun) = do
    k <- head $ streamDown l h
    btfun h k
  {-# Inline axiom #-}



instance Element ls i => Element (ls :!: IRec m c u x) i where
  data Elm (ls :!: IRec m c u x) i = ElmIRec !x !(RunningIndex i) !(Elm ls i)
  type Arg (ls :!: IRec m c u x)   = Arg ls :. x
  getArg (ElmIRec x _ ls) = getArg ls :. x
  getIdx (ElmIRec _ i _ ) = i
  {-# Inline getArg #-}
  {-# Inline getIdx #-}

instance Element ls i => Element (ls :!: (Backtrack (IRec mF c u x) mF mB r)) i where
  data Elm (ls :!: (Backtrack (IRec mF c u x) mF mB r)) i = ElmBtIRec !x [r] !(RunningIndex i) !(Elm ls i)
  type Arg (ls :!: (Backtrack (IRec mF c u x) mF mB r))   = Arg ls :. (x, [r])
  getArg (ElmBtIRec x s _ ls) = getArg ls :. (x,s)
  getIdx (ElmBtIRec _ _ i _ ) = i
  {-# Inline getArg #-}
  {-# Inline getIdx #-}

instance
  ( Monad m
  , Element ls (is:.i)
  , TableStaticVar (us:.u) (cs:.c) (is:.i)
  , AddIndexDense (Elm ls (is:.i)) (us:.u) (cs:.c) (is:.i)
  , MkStream m ls (is:.i)
  ) => MkStream m (ls :!: IRec m (cs:.c) (us:.u) x) (is:.i) where
  mkStream (ls :!: IRec c l h fun) vs us is
    = mapM (\(s,tt,ii) -> (\res -> ElmIRec res ii s) <$> fun h tt)
    . addIndexDense c vs us is
    $ mkStream ls (tableStaticVar (Proxy :: Proxy (us:.u)) c vs is) us (tableStreamIndex (Proxy :: Proxy (us:.u)) c vs is)
  {-# Inline mkStream #-}

instance
  ( Monad mB
  , Element ls (is:.i)
  , TableStaticVar (us:.u) (cs:.c) (is:.i)
  , AddIndexDense (Elm ls (is:.i)) (us:.u) (cs:.c) (is:.i)
  , MkStream mB ls (is:.i)
  ) => MkStream mB (ls :!: Backtrack (IRec mF (cs:.c) (us:.u) x) mF mB r) (is:.i) where
  mkStream (ls :!: BtIRec c l h fun bt) vs us is
    = mapM (\(s,tt,ii) -> (\res bb -> ElmBtIRec res bb ii s) <$> fun h tt <*> bt h tt)
    . addIndexDense c vs us is
    $ mkStream ls (tableStaticVar (Proxy :: Proxy (us:.u)) c vs is) us (tableStreamIndex (Proxy :: Proxy (us:.u)) c vs is)
  {-# Inline mkStream #-}

