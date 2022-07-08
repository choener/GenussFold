
-- |
--
-- TODO Strng needs its type fixed a level lower in ADPfusion
--
-- TODO need Outside code

module ADPfusion.Term.Strng.Subword where

import           Data.Proxy
import           Data.Strict.Tuple
import           Data.Vector.Fusion.Util (delay_inline)
import           Debug.Trace
import           GHC.Exts
import           Prelude hiding (map)
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic as VG

import           Data.PrimitiveArray

import           ADPfusion.Core
import           ADPfusion.Core.Subword



instance
  ( TmkCtx1 m ls (Strng v x) (Subword i)
  ) => MkStream m (ls :!: Strng v x) (Subword i) where
  mkStream grd (ls :!: strng) sv us is
    = S.map (\(ss,ee,ii) -> ElmStrng ee ii ss)
    . addTermStream1 strng sv us is
    $ mkStream (grd `andI#` termStaticCheck strng is) ls (termStaticVar strng sv is) us (termStreamIndex strng sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx m ts s x0 i0 is (Subword I)
  ) => TermStream m (TermSymbol ts (Strng v x)) s (is:.Subword I) where
  -- TODO need to check that the size @l--j@ is within the allowed string
  -- limits (which are currently not part of @Strng@.
  termStream (ts:|Strng v) (cs:.IStatic d) (us:.Subword (ui:.uj)) (is:.Subword (i:.j))
    = S.map (\(TState s ii ee) ->
                let RiSwI l = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                in  TState s (ii:.:RiSwI j) (ee:.VG.unsafeSlice l (j-l) v) )
    . termStream ts cs us is
  --
  termStream (ts:|Strng v) (cs:.IVariable d) (us:._) (is:.Subword (i:.j))
    = S.flatten mk step . termStream ts cs us is
    where mk (tstate@(TState s ii ee)) =
            let RiSwI k = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
            in  return (tstate, k, j, k) -- to enforce limits here
          step ( TState s ii ee, minK, maxK, curK )
            | curK > maxK = return $ S.Done
            | otherwise   = let RiSwI k = getIndex (getIdx s) (Proxy :: PRI is (Subword I))
                            in  do return $ S.Yield (TState s (ii:.:RiSwI curK) (ee:.VG.unsafeSlice k (curK-k) v)) (TState s ii ee, minK, maxK, curK +1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

-- | TODO this is almost certainlywrong

instance TermStaticVar (Strng v x) (Subword I) where
  termStaticVar _ (IStatic d) _ = IVariable d
  termStaticVar _ (IVariable d) _ = IVariable d
  termStreamIndex _ _ (Subword (i:.j)) = subword i (j-1)
  termStaticCheck _ _ = 1#
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}
  {-# Inline [0] termStaticCheck #-}

