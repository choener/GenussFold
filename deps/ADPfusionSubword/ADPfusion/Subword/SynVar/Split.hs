
-- |

module ADPfusion.Subword.SynVar.Split where

import Data.Proxy
import Data.Strict.Tuple
import Data.Type.Equality
import qualified Data.Vector.Fusion.Stream.Monadic as SP
import Data.Vector.Fusion.Util (delay_inline)
import Debug.Trace
import GHC.Exts
import GHC.TypeLits
import Prelude hiding (map,mapM)

import qualified Data.PrimitiveArray as PA
import Data.PrimitiveArray hiding (map)

import ADPfusion.Core
import ADPfusion.Subword.Core

-- | static fragments should be impossible...

type instance LeftPosTy (IStatic   d) (Split uId Fragment (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)) (Subword I) = TypeError (Text "IStatic / Fragment should be impossible, since 'Final' should always induce a variable left side")
type instance LeftPosTy (IStatic   d) (Split uId Final    (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Split uId Final    (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)) (Subword I) = IVariable d
type instance LeftPosTy (IVariable d) (Split uId Fragment (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)) (Subword I) = IVariable d

instance
  ( split ~ Split uId Fragment (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)
  , left ~ LeftPosTy (IVariable 0) split (Subword I)
  , Monad m, MkStream m left ls (Subword I), TermStaticVar (IVariable 0) split (Subword I)
  , Element ls (Subword I)
  ) => MkStream m (IVariable 0) (ls :!: Split uId Fragment (TwITbl b s m (Dense v) (cs:.c) (us:.u) x)) (Subword I) where
  --{{{
  {-# Inline mkStream #-}
  mkStream proxy (ls :!: split@(Split (TW (ITbl _ arr) _))) grd us i@(Subword(_:.j))
    = seq csize . SP.flatten mk step $ mkStream (Proxy @left) ls (termStaticCheck proxy split us i grd) us i
    where
      {-# Inline [0] mk #-}
      mk elm = let RiSwI l = getIdx elm
               in  return (elm:.j-l-csize)
      {-# Inline [0] step #-}
      step (elm:.zz)
        | zz >= 0 = do let RiSwI k = getIdx elm; l = j - zz; kl = subword k l
                       return $ SP.Yield (ElmSplitITbl (Proxy @uId) () (RiSwI l) elm (Subword (k:.l))) (elm:.zz-1)
        | otherwise = return SP.Done
      csize = 0 -- TODO
  --}}}



instance
  ( us ~ SplitIxTy uId (SameSid uId (Elm ls (Subword I))) (Elm ls (Subword I))
  , split ~ Split uId Final (TwITbl b s m (Dense v) (cs:.c) (us:.Subword I) x)
  , left ~ LeftPosTy (IStatic 0) split (Subword I)
  , Monad m, MkStream m left ls (Subword I), TermStaticVar (IStatic 0) split (Subword I)
  , SplitIxCol uId (SameSid uId (Elm ls (Subword I))) (Elm ls (Subword I))
  , PrimArrayOps (Dense v) (us:.Subword I) x
  , Element ls (Subword I)
  )
  => MkStream m (IStatic 0) (ls :!: Split uId Final (TwITbl b s m (Dense v) (cs:.c) (us:.Subword I) x)) (Subword I) where
--{{{
  {-# Inline mkStream #-}
  mkStream proxy (ls :!: split@(Split (TW (ITbl _ arr) _))) grd us i@(Subword (_:.j))
    = SP.map (\elm ->
      let RiSwI l = getIdx elm
          ix      = collectIx (Proxy @uId) elm :. Subword (l:.j)
          val     = arr PA.! ix
      in  ElmSplitITbl (Proxy @uId) val (RiSwI j) elm i)
    $ mkStream (Proxy :: Proxy left) ls
        (termStaticCheck proxy split us i grd)
        us i
--}}}

instance
  ( us ~ SplitIxTy uId (SameSid uId (Elm ls (Subword I))) (Elm ls (Subword I))
  , split ~ Split uId Final (TwITbl b s m (Dense v) (cs:.c) (us:.Subword I) x)
  , left ~ LeftPosTy (IStatic 0) split (Subword I)
  , Monad m, MkStream m left ls (Subword I), TermStaticVar (IVariable 0) split (Subword I)
  , SplitIxCol uId (SameSid uId (Elm ls (Subword I))) (Elm ls (Subword I))
  , PrimArrayOps (Dense v) (us:.Subword I) x
  , Element ls (Subword I)
  )
  => MkStream m (IVariable 0) (ls :!: Split uId Final (TwITbl b s m (Dense v) (cs:.c) (us:.Subword I) x)) (Subword I) where
  --{{{
  {-# Inline mkStream #-}
  mkStream proxy (ls :!: split@(Split (TW (ITbl _ arr) _))) grd us i@(Subword(_:.j))
    = seq csize . SP.flatten mk step $ mkStream (Proxy @left) ls (termStaticCheck proxy split us i grd) us i
    where
      {-# Inline [0] mk #-}
      mk elm = let RiSwI l = getIdx elm
               in  return (elm:.j-l-csize)
      {-# Inline [0] step #-}
      step (elm:.zz)
        | zz >= 0 = do let RiSwI k = getIdx elm; l = j - zz; kl = subword k l
                           ix      = collectIx (Proxy @uId) elm :. Subword (k:.l)
                           val     = arr PA.! ix
                       return $ SP.Yield (ElmSplitITbl (Proxy @uId) val (RiSwI l) elm (Subword (k:.l))) (elm:.zz-1)
        | otherwise = return SP.Done
      csize = 0 -- TODO
  --}}}

instance TermStaticVar (IStatic d) (Split uId fragTy (TwITbl bo so m arr c i x)) (Subword I) where
--{{{
  -- | Calculate how much the index changes.
  --
  -- TODO replace '0' by an appropriate (EmptyOk vs not) amount
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
--}}}

instance TermStaticVar (IVariable d) (Split uId fragTy (TwITbl bo so m arr c i x)) (Subword I) where
--{{{
  -- | Calculate how much the index changes.
  --
  -- TODO replace '0' by an appropriate (EmptyOk vs not) amount
  {-# Inline [0] termStreamIndex #-}
  termStreamIndex Proxy _ (Subword (i:.j)) = Subword (i:.j)
  {-# Inline [0] termStaticCheck #-}
  termStaticCheck Proxy _ _ _ grd = grd
--}}}

