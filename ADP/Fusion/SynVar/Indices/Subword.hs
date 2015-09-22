
-- | Instance code for @Inside@, @Outside@, and @Complement@ indices.
--
-- TODO actual @Outside@ and @Complement@ code ...
--
-- TODO we have quite a lot of @subword i j@ code where only the @type@
-- is different; check if @coerce@ yields improved performance or if the
-- compiler optimizes this out!

module ADP.Fusion.SynVar.Indices.Subword where

import Data.Proxy
import Data.Vector.Fusion.Stream.Monadic (map,Stream,head,mapM,Step(..))
import Data.Vector.Fusion.Util (delay_inline)
import Prelude hiding (map,head,mapM)
import Debug.Trace

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Base
import ADP.Fusion.SynVar.Indices.Classes



-- |
-- @
-- Table: Inside
-- Grammar: Inside
-- @

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword I)
  , GetIx a (is:.Subword I) ~ (Subword I)
  ) => AddIndexDense a (us:.Subword I) (is:.Subword I) where
  addIndexDenseGo (cs:._) (vs:.IStatic ()) (us:.Subword (_:.u)) (is:.Subword (i:.j))
    = staticCheck (j<=u)
    . map (\(SvS s a b y z y' z') -> let Subword (_:.l) = getIndex a (Proxy :: Proxy (is:.Subword I))
                                         lj = subword l j
                                         oo = subword 0 0
                                     in  SvS s a b (y:.lj) (z:.oo) (y':.lj) (z':.oo))
    . addIndexDenseGo cs vs us is
  addIndexDenseGo (cs:.c) (vs:.IVariable ()) (us:.Subword (_:.u)) (is:.Subword (i:.j))
    = staticCheck (j<=u)
    . flatten mk step . addIndexDenseGo cs vs us is
    where mk   svS = let (Subword (_:.l)) = getIndex (sIx svS) (Proxy :: Proxy (is:.Subword I))
                     in  return $ svS :. (j - l - csize)
          step (svS@(SvS s a b y z y' z') :. zz)
            | zz >= 0 = do let Subword (_:.k) = getIndex a (Proxy :: Proxy (is:.Subword I))
                               l = j - zz ; kl = subword k l
                               oo = subword 0 0
                           return $ Yield (SvS s a b (y:.kl) (z:.oo) (y':.kl) (z':.oo)) (svS :. zz-1)
            | otherwise =  return $ Done
          csize = delay_inline minSize c
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline addIndexDenseGo #-}

-- |
-- @
-- Table: Outside
-- Grammar: Outside
-- @
--
-- TODO Take care of @c@ in all cases to correctly handle @NonEmpty@ tables
-- and the like.

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword O)
  , GetIx a (is:.Subword O) ~ (Subword O)
  ) => AddIndexDense a (us:.Subword O) (is:.Subword O) where
  addIndexDenseGo (cs:.c) (vs:.OStatic (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = map (\(SvS s a b y z y' z') -> let Subword (k:._) = getIndex b (Proxy :: Proxy (is:.Subword O))
                                         kj = subword k (j+dj)
                                         ij' = subword i j -- (j+dj)
                                         oo = subword 0 0
                                     in  SvS s a b (y:.oo) (z:.kj) (y':.ij') (z':.kj))
    . addIndexDenseGo cs vs us is
  addIndexDenseGo (cs:.c) (vs:.ORightOf (di:.dj)) (us:.Subword (_:.h)) (is:.Subword (i:.j))
    = flatten mk step . addIndexDenseGo cs vs us is
    where mk svS = return (svS :. j+dj)
          step (svS@(SvS s a b y z y' z') :. l)
            | l <= h = let Subword (k:._) = getIndex a (Proxy :: Proxy (is:.Subword O))
                           kl = subword k l
                           jj = subword (j+dj) (j+dj)
                           oo = subword 0 0
                       in  return $ Yield (SvS s a b (y:.oo) (z:.kl) (y':.jj) (z':.kl)) (svS :. l+1)
            | otherwise = return Done
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  addIndexDenseGo _ (_:.OFirstLeft _) _ _ = error "SynVar.Indices.Subword : OFirstLeft"
  addIndexDenseGo _ (_:.OLeftOf    _) _ _ = error "SynVar.Indices.Subword : LeftOf"
  {-# Inline addIndexDenseGo #-}

-- |
-- @
-- Table: Inside
-- Grammar: Outside
-- @
--
-- TODO take care of @c@

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword O)
  , GetIx a (is:.Subword O) ~ (Subword O)
  ) => AddIndexDense a (us:.Subword I) (is:.Subword O) where
  addIndexDenseGo (cs:.c) (vs:.OStatic (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = map (\(SvS s a b y z y' z') -> let Subword (_:.k) = getIndex a (Proxy :: Proxy (is:.Subword O))
                                         ll@(Subword (_:.l)) = getIndex b (Proxy :: Proxy (is:.Subword O))
                                         klI = subword (k-dj) (l-dj)
                                         klO = subword (k-dj) (l-dj)
                                         oo  = subword 0 0
                                     in  SvS s a b (y:.klI) (z:.oo) (y':.klO) (z':.ll))
    . addIndexDenseGo cs vs us is
  addIndexDenseGo (cs:.c) (vs:.ORightOf d) (us:.u) (is:.Subword (i:.j))
    = flatten mk step . addIndexDenseGo cs vs us is
    where mk svS = let Subword (_:.l) = getIndex (sIx svS) (Proxy :: Proxy (is:.Subword O))
                   in  return (svS :. l :. l + csize)
          step (svS@(SvS s a b y z y' z') :. k :. l)
            | l <= o    = return $ Yield (SvS s a b (y:.klI) (z:.oo) (y':.klO) (z':.zo))
                                         (svS :. k :. l+1)
            | otherwise = return $ Done
            where zo@(Subword (_:.o)) = getIndex b (Proxy :: Proxy (is:.Subword O))
                  klI = subword k l
                  klO = subword k l
                  oo = subword 0 0
          csize = minSize c
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  addIndexDenseGo (cs:.c) (vs:.OFirstLeft (di:.dj)) (us:.u) (is:.Subword (i:.j))
    = map (\(SvS s a b y z y' z') -> let Subword (_:.k) = getIndex a (Proxy :: Proxy (is:.Subword O))
                                         ll@(Subword (l:._)) = getIndex b (Proxy :: Proxy (is:.Subword O))
                                         klI = subword k $ i - di
                                         klO = subword k $ i - di
                                         oo  = subword 0 0
                                     in  SvS s a b (y:.klI) (z:.oo) (y':.klO) (z':.ll))
    . addIndexDenseGo cs vs us is
  addIndexDenseGo (cs:.c) (vs:.OLeftOf d) (us:.u) (is:.Subword (i:.j))
    = flatten mk step . addIndexDenseGo cs vs us is
    where mk svS = let Subword (_:.l) = getIndex (sIx svS) (Proxy :: Proxy (is:.Subword O))
                   in  return $ svS :. l
          step (svS@(SvS s a b y z y' z') :. l)
            | l <= i    = let Subword (_:.k) = getIndex a (Proxy :: Proxy (is:.Subword O))
                              omx = getIndex b (Proxy :: Proxy (is:.Subword O))
                              klI = subword k l
                              klO = subword k l
                              oo  = subword 0 0
                          in  return $ Yield (SvS s a b (y:.klI) (z:.oo) (y':.klO) (z':.omx))
                                             (svS :. l+1)
            | otherwise = return $ Done
          csize = minSize c
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline addIndexDenseGo #-}




-- TODO
-- @
-- Table: Inside
-- Grammar: Complement
-- @

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword C)
  , GetIx a (is:.Subword C) ~ (Subword C)
  ) => AddIndexDense a (us:.Subword I) (is:.Subword C) where
  addIndexDenseGo (cs:.c) (vs:.Complemented) (us:.u) (is:.i)
    = map (\(SvS s a b y z y' z') -> let Subword kk = getIndex a (Proxy :: Proxy (is:.Subword C))
                                         kT = Subword kk -- @k@ Table
                                         kC = Subword kk
                                     in  SvS s a b (y:.kT) (z:.kT) (y':.kC) (z':.kC))
    . addIndexDenseGo cs vs us is
  {-# Inline addIndexDenseGo #-}

-- TODO
-- @
-- Table: Outside
-- Grammar: Complement
-- @

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword C)
  , GetIx a (is:.Subword C) ~ (Subword C)
  ) => AddIndexDense a (us:.Subword O) (is:.Subword C) where
  addIndexDenseGo (cs:.c) (vs:.Complemented) (us:.u) (is:.i)
    = map (\(SvS s a b y z y' z') -> let Subword kk = getIndex a (Proxy :: Proxy (is:.Subword C))
                                         kT = Subword kk
                                         kC = Subword kk
                                     in  SvS s a b (y:.kT) (z:.kT) (y':.kC) (z':.kC))
    . addIndexDenseGo cs vs us is
  {-# Inline addIndexDenseGo #-}

-- |
-- @
-- Table: Complement
-- Grammar: Complement
-- @

instance
  ( AddIndexDense a us is
  , GetIndex a (is:.Subword C)
  , GetIx a (is:.Subword C) ~ (Subword C)
  ) => AddIndexDense a (us:.Subword C) (is:.Subword C) where
  addIndexDenseGo (cs:.c) (vs:.Complemented) (us:.u) (is:.i)
    = map (\(SvS s a b y z y' z') -> let k = getIndex a (Proxy :: Proxy (is:.Subword C))
                                         oo = subword 0 0
                                     in  SvS s a b (y:.k) (z:.oo) (y':.k) (z':.oo))
    . addIndexDenseGo cs vs us is
  {-# Inline addIndexDenseGo #-}

