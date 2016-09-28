
{-# Options_GHC -O0 #-}

module QuickCheck.Point where

import           Control.Applicative
import           Control.Monad
import           Data.Strict.Tuple
import           Data.Vector.Fusion.Util
import           Debug.Trace
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe
import           Test.QuickCheck
import           Test.QuickCheck.All
import           Test.QuickCheck.Monadic
#ifdef ADPFUSION_TEST_SUITE_PROPERTIES
import           Test.Tasty.TH
import           Test.Tasty.QuickCheck
#endif

import           Data.PrimitiveArray

import           ADP.Fusion.Point



-- * Epsilon cases

prop_I_Epsilon ix@(PointL j) = zs == ls where
  zs = (id <<< Epsilon ... stoList) maxPLi ix
  ls = [ () | j == 0 ]

prop_O_Epsilon ix@(PointL j) = zs == ls where
  zs = (id <<< Epsilon ... stoList) maxPLo ix
  ls = [ () | j == maxI ]

prop_I_ZEpsilon ix@(Z:.PointL j) = zs == ls where
  zs = (id <<< (M:|Epsilon) ... stoList) (Z:.maxPLi) ix
  ls = [ Z:.() | j == 0 ]

prop_O_ZEpsilon ix@(Z:.PointL j) = zs == ls where
  zs = (id <<< (M:|Epsilon) ... stoList) (Z:.maxPLo) ix
  ls = [ Z:.() | j == maxI ]

prop_O_ZEpsilonEpsilon ix@(Z:.PointL j:.PointL l) = zs == ls where
  zs = (id <<< (M:|Epsilon:|Epsilon) ... stoList) (Z:.maxPLo:.maxPLo) ix
  ls = [ Z:.():.() | j == maxI, l == maxI ]



-- * Deletion cases

prop_I_ItNC ix@(PointL j) = zs == ls where
  zs = ((,,) <<< tSI % Deletion % chr xs ... stoList) maxPLi ix
  ls = [ ( unsafeIndex xsP (PointL $ j-1)
         , ()
         , xs VU.! (j-1)
         ) | j >= 1, j <= (maxI) ]

prop_O_ItNC ix@(PointL j) = zs == ls where
  zs = ((,,) <<< tSO % Deletion % chr xs ... stoList) maxPLo ix
  ls = [ ( unsafeIndex xsPo (PointL $ j+1)
         , ()
         , xs VU.! (j+0)
         ) | j >= 0, j <= (maxI-1) ]
{-# Noinline prop_O_ItNC #-}

prop_O_ZItNC ix@(Z:.PointL j) = zs == ls where
  zs = ((,,) <<< tZ1O % (M:|Deletion) % (M:|chr xs) ... stoList) (Z:.maxPLo) ix
  ls = [ ( unsafeIndex xsZPo (Z:.PointL (j+1))
         , Z:.()
         , Z:.xs VU.! (j+0)
         ) | j >= 0, j <= (maxI-1) ]

prop_O_2dimIt_NC_CN ix@(Z:.PointL j:.PointL l) = zs == ls where
  zs = ((,,) <<< tZ2O % (M:|Deletion:|chr xs) % (M:|chr xs:|Deletion) ... stoList) (Z:.maxPLo:.maxPLo) ix
  ls = [ ( unsafeIndex xsPPo (Z:.PointL (j+1):.PointL (l+1))
         , Z:.()           :.xs VU.! (l+0)
         , Z:.xs VU.! (j+0):.()
         ) | j>=0, l>=0, j<=(maxI-1), l<=(maxI-1) ]

prop_I_2dimIt_NC_CN ix@(Z:.PointL j:.PointL l) = zs == ls where
  zs = ((,,) <<< tZ2I % (M:|Deletion:|chr xs) % (M:|chr xs:|Deletion) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ ( unsafeIndex xsPP (Z:.PointL (j-1):.PointL (l-1))
         , Z:.()           :.xs VU.! (l-1)
         , Z:.xs VU.! (j-1):.()
         ) | j>=1, l>=1, j<=maxI, l<=maxI ]



-- * terminal cases

-- | A single character terminal

prop_I_Tt ix@(Z:.PointL j) = zs == ls where
  zs = (id <<< (M:|chr xs) ... stoList) (Z:.maxPLi) ix
  ls = [ (Z:.xs VU.! (j-1)) | 1==j ]

--prop_O_Tt ix@(Z:.O (PointL j)) = traceShow (j,zs,ls) $ zs == ls where
--  zs = (id <<< (M:|chr xs) ... stoList) (Z:.O maxPLo) ix
--  ls = [ (Z:.xs VU.! (j-1)) | 1==j ]

-- | Two single-character terminals

prop_I_CC ix@(Z:.PointL i) = zs == ls where
  zs = ((,) <<< (M:|chr xs) % (M:|chr xs) ... stoList) (Z:.maxPLi) ix
  ls = [ (Z:.xs VU.! (i-2), Z:.xs VU.! (i-1)) | 2==i ]

tSI  = TW (ITbl 0 0 EmptyOk xsP)  (\ (_ :: PointL I) (_ :: PointL I) -> Id (1::Int))
tSO  = TW (ITbl 0 0 EmptyOk xsPo) (\ (_ :: PointL O) (_ :: PointL O) -> Id (1::Int))
tZ1I = TW (ITbl 0 0 (Z:.EmptyOk) xsZP) (\ (_::Z:.PointL I) (_::Z:.PointL I) -> Id (1::Int))
tZ1O = TW (ITbl 0 0 (Z:.EmptyOk) xsZPo) (\ (_::Z:.PointL O) (_::Z:.PointL O) -> Id (1::Int))
tZ2I = TW (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) xsPP) (\ (_::Z:.PointL I:.PointL I) (_::Z:.PointL I:.PointL I) -> Id (1::Int))
tZ2O = TW (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) xsPPo) (\ (_::Z:.PointL O:.PointL O) (_::Z:.PointL O:.PointL O) -> Id (1::Int))

-- | Just a table

prop_I_It ix@(PointL j) = zs == ls where
  zs = (id <<< tSI ... stoList) maxPLi ix
  ls = [ unsafeIndex xsP ix | j>=0, j<=maxI ]

prop_O_It ix@(PointL j) = zs == ls where
  zs = (id <<< tSO ... stoList) maxPLo ix
  ls = [ unsafeIndex xsPo ix | j>=0, j<=maxI ]

prop_I_ZIt ix@(Z:.PointL j) = zs == ls where
  zs = (id <<< tZ1I ... stoList) (Z:.maxPLi) ix
  ls = [ unsafeIndex xsZP ix | j>=0, j<=maxI ]

prop_O_ZIt ix@(Z:.PointL j) = zs == ls where
  zs = (id <<< tZ1O ... stoList) (Z:.maxPLo) ix
  ls = [ unsafeIndex xsZPo ix | j>=0, j<=maxI ]

-- | Table, then single terminal

prop_I_ItC ix@(PointL j) = zs == ls where
  zs = ((,) <<< tSI % chr xs ... stoList) maxPLi ix
  ls = [ ( unsafeIndex xsP (PointL $ j-1)
         , xs VU.! (j-1)
         ) | j>=1, j<=maxI ]

-- | @A^*_j -> A^*_{j+1} c_{j+1)@ !

prop_O_ItC ix@(PointL j) = zs == ls where
  zs = ((,) <<< tSO % chr xs ... stoList) maxPLo ix
  ls = [ ( unsafeIndex xsPo (PointL $ j+1)
         , xs VU.! (j+0)
         ) | j >= 0, j <= (maxI-1) ]

prop_O_ItCC ix@(PointL j) = zs == ls where
  zs = ((,,) <<< tSO % chr xs % chr xs ... stoList) maxPLo ix
  ls = [ ( unsafeIndex xsPo (PointL $ j+2)
         , xs VU.! (j+0)
         , xs VU.! (j+1)
         ) | j >= 0, j <= (maxI-2) ]

prop_O_ItCCC ix@(PointL j) = zs == ls where
  zs = ((,,,) <<< tSO % chr xs % chr xs % chr xs ... stoList) maxPLo ix
  ls = [ ( unsafeIndex xsPo (PointL $ j+3)
         , xs VU.! (j+0)
         , xs VU.! (j+1)
         , xs VU.! (j+2)
         ) | j >= 0, j <= (maxI-3) ]

prop_O_ZItCC ix@(Z:.PointL j) = zs == ls where
  zs = ((,,) <<< tZ1O % (M:|chr xs) % (M:|chr xs) ... stoList) (Z:.maxPLo) ix
  ls = [ ( unsafeIndex xsZPo (Z:.PointL (j+2))
         , Z:.xs VU.! (j+0)
         , Z:.xs VU.! (j+1)
         ) | j >= 0, j <= (maxI-2) ]

-- | synvar followed by a 2-tape character terminal

prop_I_2dimItCC ix@(Z:.PointL j:.PointL l) = zs == ls where
  zs = ((,,) <<< tZ2I % (M:|chr xs:|chr xs) % (M:|chr xs:|chr xs) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ ( unsafeIndex xsPP (Z:.PointL (j-2):.PointL (l-2))
         , Z:.xs VU.! (j-2):.xs VU.! (l-2)
         , Z:.xs VU.! (j-1):.xs VU.! (l-1)
         ) | j>=2, l>=2, j<=maxI, l<=maxI ]

prop_O_2dimItCC ix@(Z:.PointL j:.PointL l) = zs == ls where
  zs = ((,,) <<< tZ2O % (M:|chr xs:|chr xs) % (M:|chr xs:|chr xs) ... stoList) (Z:.maxPLo:.maxPLo) ix
  ls = [ ( unsafeIndex xsPPo (Z:.PointL (j+2):.PointL (l+2))
         , Z:.xs VU.! (j+0):.xs VU.! (l+0)
         , Z:.xs VU.! (j+1):.xs VU.! (l+1)
         ) | j>=0, l>=0, j<=(maxI-2), l<=(maxI-2) ]

-- * 'Strng' tests

-- ** Just the 'Strng' terminal

prop_I_ManyS ix@(PointL j) = zs == ls where
  zs = (id <<< manyS xs ... stoList) maxPLi ix
  ls = [ (VU.slice 0 j xs) ]

prop_I_SomeS ix@(PointL j) = zs == ls where
  zs = (id <<< someS xs ... stoList) maxPLi ix
  ls = [ (VU.slice 0 j xs) | j>0 ]

prop_2dim_ManyS_ManyS ix@(Z:.PointL i:.PointL j) = zs == ls where
  zs = (id <<< (M:|manyS xs:|manyS xs) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ (Z:.VU.slice 0 i xs:.VU.slice 0 j xs) ]

prop_2dim_SomeS_SomeS ix@(Z:.PointL i:.PointL j) = zs == ls where
  zs = (id <<< (M:|someS xs:|someS xs) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ (Z:.VU.slice 0 i xs:.VU.slice 0 j xs) | i > 0 && j > 0 ]

-- ** Together with a syntactic variable.

prop_I_Itbl_ManyS ix@(PointL i) = zs == ls where
  zs = ((,) <<< tSI % manyS xs ... stoList) maxPLi ix
  ls = [ (unsafeIndex xsP (PointL k), VU.slice k (i-k) xs) | k <- [0..i] ]

prop_I_Itbl_SomeS ix@(PointL i) = zs == ls where
  zs = ((,) <<< tSI % someS xs ... stoList) maxPLi ix
  ls = [ (unsafeIndex xsP (PointL k), VU.slice k (i-k) xs) | k <- [0..i-1] ]

prop_I_1dim_Itbl_ManyS ix@(Z:.PointL i) = zs == ls where
  zs = ((,) <<< tZ1I % (M:|manyS xs) ... stoList) (Z:.maxPLi) ix
  ls = [ (unsafeIndex xsZP (Z:.PointL k), Z:. VU.slice k (i-k) xs) | k <- [0..i] ]

prop_I_1dim_Itbl_SomeS ix@(Z:.PointL i) = zs == ls where
  zs = ((,) <<< tZ1I % (M:|someS xs) ... stoList) (Z:.maxPLi) ix
  ls = [ (unsafeIndex xsZP (Z:.PointL k), Z:. VU.slice k (i-k) xs) | k <- [0..i-1] ]

prop_I_2dim_Itbl_ManyS_ManyS ix@(Z:.PointL i:.PointL j) = zs == ls where
  zs = ((,) <<< tZ2I % (M:|manyS xs:|manyS xs) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ (unsafeIndex xsPP (Z:.PointL k:.PointL l), Z:. VU.slice k (i-k) xs :. VU.slice l (j-l) xs) | k <- [0..i], l <- [0..j] ]

prop_I_2dim_Itbl_SomeS_SomeS ix@(Z:.PointL i:.PointL j) = zs == ls where
  zs = ((,) <<< tZ2I % (M:|someS xs:|someS xs) ... stoList) (Z:.maxPLi:.maxPLi) ix
  ls = [ (unsafeIndex xsPP (Z:.PointL k:.PointL l), Z:. VU.slice k (i-k) xs :. VU.slice l (j-l) xs) | k <- [0..i-1], l <- [0..j-1] ]



stoList = unId . SM.toList

infixl 8 >>>
(>>>) f xs = \lu ij -> SM.map f . mkStream (build xs) (initialContext ij) lu $ ij

xsP :: Unboxed (PointL I) Int
xsP = fromList (PointL 0) maxPLi [0 ..]

xsZP :: Unboxed (Z:.PointL I) Int
xsZP = fromList (Z:.PointL 0) (Z:.maxPLi) [0 ..]

xsPo :: Unboxed (PointL O) Int
xsPo = fromList (PointL 0) maxPLo [0 ..]

xsZPo :: Unboxed (Z:.PointL O) Int
xsZPo = fromList (Z:.PointL 0) (Z:.maxPLo) [0 ..]

xsPP :: Unboxed (Z:.PointL I:.PointL I) Int
xsPP = fromList (Z:.PointL 0:.PointL 0) (Z:.maxPLi:.maxPLi) [0 ..]

xsPPo :: Unboxed (Z:.PointL O:.PointL O) Int
xsPPo = fromList (Z:.PointL 0:.PointL 0) (Z:.maxPLo:.maxPLo) [0 ..]

mxsPP = unsafePerformIO $ zzz where
  zzz :: IO (MutArr IO (Unboxed (Z:.PointL I:.PointL I) Int))
  zzz = fromListM (Z:.PointL 0:.PointL 0) (Z:.maxPLi:.maxPLi) [0 ..]

maxI =100

maxPLi :: PointL I
maxPLi = PointL maxI

maxPLo :: PointL O
maxPLo = PointL maxI

xs = VU.fromList [0 .. maxI - 1 :: Int]

-- * general quickcheck stuff

options = stdArgs {maxSuccess = 1000 } -- 0}

customCheck = quickCheckWithResult options

return []
allProps = $forAllProperties customCheck



#ifdef ADPFUSION_TEST_SUITE_PROPERTIES
testgroup_point = $(testGroupGenerator)
#endif

