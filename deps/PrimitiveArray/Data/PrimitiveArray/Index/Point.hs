
{-# Language MagicHash #-}

-- | @Point@ index structures are used for left- and right-linear grammars.
-- Such grammars have at most one syntactic symbol on each r.h.s. of a rule.
-- The syntactic symbol needs to be in an outermost position.

module Data.PrimitiveArray.Index.Point where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras (Ranked)
import           Data.Hashable (Hashable)
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Unbox(..))
import           GHC.Exts
import           GHC.Generics (Generic)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck as TQ
import           Test.SmallCheck.Series as TS

import           Data.PrimitiveArray.Index.Class
import           Data.PrimitiveArray.Index.IOC



-- | A point in a left-linear grammar. The syntactic symbol is in left-most
-- position.

newtype PointL t = PointL {fromPointL :: Int}
  deriving stock (Eq,Ord,Read,Show,Generic)
  deriving newtype (Num)

pointLI :: Int -> PointL I
pointLI = PointL
{-# Inline pointLI #-}

pointLO :: Int -> PointL O
pointLO = PointL
{-# Inline pointLO #-}

pointLC :: Int -> PointL C
pointLC = PointL
{-# Inline pointLC #-}



derivingUnbox "PointL"
  [t| forall t . PointL t -> Int    |]
  [| \ (PointL i) -> i |]
  [| \ i -> PointL i   |]

instance Binary       (PointL t)
instance Serialize    (PointL t)
instance FromJSON     (PointL t)
instance FromJSONKey  (PointL t)
instance ToJSON       (PointL t)
instance ToJSONKey    (PointL t)
instance Hashable     (PointL t)

instance NFData (PointL t) where
  rnf (PointL l) = rnf l
  {-# Inline rnf #-}

instance Index (PointL t) where
  newtype LimitType (PointL t) = LtPointL Int
  linearIndex _ (PointL z) = z
  {-# INLINE linearIndex #-}
  fromLinearIndex (LtPointL h) k = (PointL k)
  {-# Inline fromLinearIndex #-}
  size (LtPointL h) = h + 1
  {-# INLINE size #-}
  inBounds (LtPointL h) (PointL x) = 0<=x && x<=h
  {-# INLINE inBounds #-}
  zeroBound = PointL 0
  {-# Inline [0] zeroBound #-}
  zeroBound' = LtPointL 0
  {-# Inline [0] zeroBound' #-}
  totalSize (LtPointL h) = [fromIntegral $ h + 1]
  {-# Inline [0] totalSize #-}
  showBound (LtPointL h) = ["LtPointL " ++ show h]
  showIndex (PointL i) = ["PointL " ++ show i]

deriving instance Eq      (LimitType (PointL t))
deriving instance Generic (LimitType (PointL t))
deriving instance Read    (LimitType (PointL t))
deriving instance Show    (LimitType (PointL t))

instance IndexStream z => IndexStream (z:.PointL I) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   PointL ht) $ streamUp ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep PointL lf) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

instance IndexStream z => IndexStream (z:.PointL O) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep PointL lf) $ streamUp   ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   PointL ht) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

instance IndexStream z => IndexStream (z:.PointL C) where
  streamUp   (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamUpMk   lf) (streamUpStep   PointL ht) $ streamUp ls hs
  streamDown (ls:..LtPointL lf) (hs:..LtPointL ht) = SM.flatten (streamDownMk ht) (streamDownStep PointL lf) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

data SP z = SP !z !Int#

streamUpMk (I# lf) z = return $ SP z lf
{-# Inline [0] streamUpMk #-}

streamUpStep wrapper (I# ht) (SP z k)
  | 1# <- k ># ht = return $ SM.Done
  | otherwise     = return $ SM.Yield (z:.wrapper (I# k)) (SP z (k +# 1#))
{-# Inline [0] streamUpStep #-}

streamDownMk (I# ht) z = return $ SP z ht
{-# Inline [0] streamDownMk #-}

streamDownStep wrapper (I# lf) (SP z k)
  | 1# <- k <# lf = return $ SM.Done
  | otherwise     = return $ SM.Yield (z:.wrapper (I# k)) (SP z (k -# 1#))
{-# Inline [0] streamDownStep #-}

instance IndexStream (Z:.PointL t) => IndexStream (PointL t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}


instance Arbitrary (PointL t) where
  arbitrary = do
    b <- choose (0,100)
    return $ PointL b
  shrink (PointL j)
    | 0<j = [PointL $ j-1]
    | otherwise = []

instance Monad m => Serial m (PointL t) where
  series = PointL . TS.getNonNegative <$> series



-- * @PointR@

-- | A point in a right-linear grammars.

newtype PointR t = PointR {fromPointR :: Int}
  deriving stock (Eq,Ord,Read,Show,Generic)
  deriving newtype (Num)



derivingUnbox "PointR"
  [t| forall t . PointR t -> Int    |]
  [| \ (PointR i) -> i |]
  [| \ i -> PointR i   |]

instance Binary       (PointR t)
instance Serialize    (PointR t)
instance FromJSON     (PointR t)
instance FromJSONKey  (PointR t)
instance ToJSON       (PointR t)
instance ToJSONKey    (PointR t)
instance Hashable     (PointR t)

instance NFData (PointR t) where
  rnf (PointR l) = rnf l
  {-# Inline rnf #-}

instance Index (PointR t) where
  newtype LimitType (PointR t) = LtPointR Int
  linearIndex _ (PointR z) = z
  {-# INLINE linearIndex #-}
  size (LtPointR h) = h + 1
  {-# INLINE size #-}
  inBounds (LtPointR h) (PointR x) = 0<=x && x<=h
  {-# INLINE inBounds #-}
  zeroBound = PointR 0
  {-# Inline [0] zeroBound #-}
  zeroBound' = LtPointR 0
  {-# Inline [0] zeroBound' #-}
  totalSize (LtPointR h) = [fromIntegral $ h + 1]
  {-# Inline [0] totalSize #-}
  fromLinearIndex _ = PointR
  {-# Inline [0] fromLinearIndex #-}
  showBound (LtPointR b) = ["LtPointR " ++ show b]
  showIndex (PointR p) = ["PointR " ++ show p]

deriving instance Eq      (LimitType (PointR t))
deriving instance Generic (LimitType (PointR t))
deriving instance Read    (LimitType (PointR t))
deriving instance Show    (LimitType (PointR t))

instance IndexStream z => IndexStream (z:.PointR I) where
  streamUp   (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamDownMk ht) (streamDownStep PointR lf) $ streamUp ls hs
  streamDown (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamUpMk   lf) (streamUpStep   PointR ht) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

instance IndexStream z => IndexStream (z:.PointR O) where
  streamUp   (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamUpMk   lf) (streamUpStep   PointR ht) $ streamUp   ls hs
  streamDown (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamDownMk ht) (streamDownStep PointR lf) $ streamDown ls hs
  {-# Inline [0] streamUp #-}
  {-# Inline [0] streamDown #-}

--instance IndexStream z => IndexStream (z:.PointR C) where
--  streamUp   (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamUpMkR   lf) (streamUpStepR   ht) $ streamUp ls hs
--  streamDown (ls:..LtPointR lf) (hs:..LtPointR ht) = SM.flatten (streamDownMkR ht) (streamDownStepR lf) $ streamDown ls hs
--  {-# Inline [0] streamUp #-}
--  {-# Inline [0] streamDown #-}

instance IndexStream (Z:.PointR t) => IndexStream (PointR t) where
  streamUp l h = SM.map (\(Z:.i) -> i) $ streamUp (ZZ:..l) (ZZ:..h)
  {-# INLINE streamUp #-}
  streamDown l h = SM.map (\(Z:.i) -> i) $ streamDown (ZZ:..l) (ZZ:..h)
  {-# INLINE streamDown #-}

-- arbitrarily set maximum here to

arbMaxPointR = 100

instance Arbitrary (PointR t) where
  arbitrary = do
    b <- choose (0,arbMaxPointR)
    return $ PointR b
  shrink (PointR j)
    | j<arbMaxPointR = [PointR $ j+1]
    | otherwise = []

--instance Monad m => Serial m (PointR t) where
--  series = PointR . TS.getNonNegative <$> series



instance SparseBucket (PointL I) where
  {-# Inline manhattan #-}
  manhattan (LtPointL u) (PointL i) = i
  {-# Inline manhattanMax #-}
  manhattanMax (LtPointL u) = u


-- |
--
-- TODO Is this instance correct? Outside indices shrink?

instance SparseBucket (PointL O) where
  {-# Inline manhattan #-}
  manhattan (LtPointL u) (PointL i) = u-i
  {-# Inline manhattanMax #-}
  manhattanMax (LtPointL u) = u

