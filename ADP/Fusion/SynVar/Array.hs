
module ADP.Fusion.SynVar.Array
  ( module ADP.Fusion.SynVar.Array.Type
  , module ADP.Fusion.SynVar.Array
  ) where


import Data.Proxy
import Data.Strict.Tuple hiding (snd)
import Data.Vector.Fusion.Stream.Monadic
import Prelude hiding (map,mapM)

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Base
import ADP.Fusion.SynVar.Backtrack
import ADP.Fusion.SynVar.Indices

import ADP.Fusion.SynVar.Array.TermSymbol
import ADP.Fusion.SynVar.Array.Type



-- | Constraints needed to use @iTblStream@.

type ITblCx m ls arr x u c i =
  ( TableStaticVar u c i
  , MkStream m ls i
  , Element ls i
  , AddIndexDense (SynVar1 (Elm ls i)) (Z:.u) (Z:.c) (Z:.i)
  , PrimArrayOps arr u x
  )

-- | General function for @ITbl@s with skalar indices.

iTblStream
  :: forall m ls arr x u c i . ITblCx m ls arr x u c i
  => Pair ls (ITbl m arr c u x)
  -> Context i
  -> i
  -> i
  -> Stream m (Elm (ls :!: ITbl m arr c u x) i)
iTblStream (ls :!: ITbl _ _ c t _) vs us is
  = map (\(s,tt,ii') -> ElmITbl (t!tt) ii' s)
  . addIndexDense1 c vs us is
  $ mkStream ls (tableStaticVar (Proxy :: Proxy u) c vs is) us (tableStreamIndex (Proxy :: Proxy u) c vs is)
{-# Inline iTblStream #-}

-- | General function for @Backtrack ITbl@s with skalar indices.

btITblStream
  :: forall mB mF ls arr x r u c i . ITblCx mB ls arr x u c i
  => Pair ls (Backtrack (ITbl mF arr c u x) mF mB r)
  -> Context i
  -> i
  -> i
  -> Stream mB (Elm (ls :!: Backtrack (ITbl mF arr c u x) mF mB r) i)
btITblStream (ls :!: BtITbl c t bt) vs us is
    = mapM (\(s,tt,ii') -> bt us' tt >>= \ ~bb -> return $ ElmBtITbl (t!tt) bb ii' s)
    . addIndexDense1 c vs us is
    $ mkStream ls (tableStaticVar (Proxy :: Proxy u) c vs is) us (tableStreamIndex (Proxy :: Proxy u) c vs is)
    where !us' = snd $ bounds t
{-# Inline btITblStream #-}



-- ** Instances

instance
  ( Monad m
  , ITblCx m ls arr x u c (i I)
  ) => MkStream m (ls :!: ITbl m arr c u x) (i I) where
  mkStream = iTblStream
  {-# Inline mkStream #-}

instance
  ( Monad m
  , ITblCx m ls arr x u c (i O)
  ) => MkStream m (ls :!: ITbl m arr c u x) (i O) where
  mkStream = iTblStream
  {-# Inline mkStream #-}

instance
  ( Monad m
  , ITblCx m ls arr x u c (i C)
  ) => MkStream m (ls :!: ITbl m arr c u x) (i C) where
  mkStream = iTblStream
  {-# Inline mkStream #-}

instance
  ( Monad mB
  , ITblCx mB ls arr x u c (i I)
  ) => MkStream mB (ls :!: Backtrack (ITbl mF arr c u x) mF mB r) (i I) where
  mkStream = btITblStream
  {-# Inline mkStream #-}

instance
  ( Monad mB
  , ITblCx mB ls arr x u c (i O)
  ) => MkStream mB (ls :!: Backtrack (ITbl mF arr c u x) mF mB r) (i O) where
  mkStream = btITblStream
  {-# Inline mkStream #-}

instance
  ( Monad mB
  , ITblCx mB ls arr x u c (i C)
  ) => MkStream mB (ls :!: Backtrack (ITbl mF arr c u x) mF mB r) (i C) where
  mkStream = btITblStream
  {-# Inline mkStream #-}

instance ModifyConstraint (ITbl m arr EmptyOk i x) where
  type TNE (ITbl m arr EmptyOk i x) = ITbl m arr NonEmpty i x
  type TE  (ITbl m arr EmptyOk i x) = ITbl m arr EmptyOk  i x
  toNonEmpty (ITbl b l _ arr f) = ITbl b l NonEmpty arr f
  {-# Inline toNonEmpty #-}

instance ModifyConstraint (Backtrack (ITbl mF arr EmptyOk i x) mF mB r) where
  type TNE (Backtrack (ITbl mF arr EmptyOk i x) mF mB r) = Backtrack (ITbl mF arr NonEmpty i x) mF mB r
  type TE  (Backtrack (ITbl mF arr EmptyOk i x) mF mB r) = Backtrack (ITbl mF arr EmptyOk  i x) mF mB r
  toNonEmpty (BtITbl _ arr bt) = BtITbl NonEmpty arr bt
  {-# Inline toNonEmpty #-}

--instance ModifyConstraint (ITbl m arr EmptyOk (Subword t) x) where
--  toNonEmpty (ITbl b l _ arr f) = ITbl b l NonEmpty arr f
--  toEmpty    = id
--  {-# Inline toNonEmpty #-}
--  {-# Inline toEmpty #-}
--
--instance ModifyConstraint (ITbl m arr NonEmpty (Subword t) x) where
--  toNonEmpty = id
--  toEmpty    (ITbl b l _ arr f) = ITbl b l EmptyOk arr f
--  {-# Inline toNonEmpty #-}
--  {-# Inline toEmpty #-}

--instance ModifyConstraint (ITbl m arr (Z:.Subword t:.Subword t) x) where
--  toNonEmpty (ITbl b l _ arr f) = ITbl b l (Z:.NonEmpty:.NonEmpty) arr f
--  toEmpty    (ITbl b l _ arr f) = ITbl b l (Z:.EmptyOk :.EmptyOk ) arr f
--  {-# Inline toNonEmpty #-}
--  {-# Inline toEmpty #-}
--
--instance ModifyConstraint (Backtrack (ITbl mF arr (Subword t) x) mF mB r) where
--  toNonEmpty (BtITbl _ arr bt) = BtITbl NonEmpty arr bt
--  toEmpty    (BtITbl _ arr bt) = BtITbl EmptyOk  arr bt
--  {-# Inline toNonEmpty #-}
--  {-# Inline toEmpty #-}
--
--instance ModifyConstraint (Backtrack (ITbl mF arr (Z:.Subword t:.Subword t) x) mF mB r) where
--  toNonEmpty (BtITbl _ arr bt) = BtITbl (Z:.NonEmpty:.NonEmpty) arr bt
--  toEmpty    (BtITbl _ arr bt) = BtITbl (Z:.EmptyOk :.EmptyOk ) arr bt
--  {-# Inline toNonEmpty #-}
--  {-# Inline toEmpty #-}

