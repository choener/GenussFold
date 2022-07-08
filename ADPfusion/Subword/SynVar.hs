
module ADPfusion.Subword.SynVar where

import qualified Data.Vector.Fusion.Stream.Monadic as SP

import Data.PrimitiveArray

import ADPfusion.Core
import ADPfusion.Subword.Core

-- | TODO cf with tables for subword
instance TermStaticVar (IStatic 0) (TwITbl bo so m arr c (Subword I) x) (Subword I) where
----{{{
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
----}}}

-- | TODO cf with tables for subword
instance TermStaticVar (IVariable d) (TwITbl bo so m arr c (Subword I) x) (Subword I) where
----{{{
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
----}}}


instance (Monad m, PrimArrayOps arr (Subword I) x, TermStream m ps ts s is)
  => TermStream m (ps:.IStatic 0) (TermSymbol ts (TwITbl bo so m arr c (Subword I) x)) s (is:.Subword I) where
--{{{
  {-# Inline termStream #-}
  termStream Proxy (ts:| TW (ITbl _ arr) f) (us:..u) (is:.i)
    = SP.map (\(TState s ii ee) ->
        let ri = error "get running index from the left, build up"
            ix = error "get running index from the left, build up"
        in  TState s (ii:.:RiSwI ri) (ee:.arr!ix))
    . termStream (Proxy :: Proxy ps) ts us is
--}}}

instance (Monad m, PrimArrayOps arr (Subword I) x, TermStream m ps ts s is)
  => TermStream m (ps:.IVariable 0) (TermSymbol ts (TwITbl bo so m arr c (Subword I) x)) s (is:.Subword I) where
--{{{
  {-# Inline termStream #-}
  termStream Proxy (ts:| TW (ITbl _ arr) f) (us:..u) (is:.i)
    -- TODO this probably wants SP.flatten, since we want to move indices around
    = SP.map (\(TState s ii ee) ->
        let ri = error "get running index from the left, build up"
            ix = error "get running index from the left, build up"
        in  TState s (ii:.:RiSwI ri) (ee:.arr!ix))
    . termStream (Proxy :: Proxy ps) ts us is
--}}}

