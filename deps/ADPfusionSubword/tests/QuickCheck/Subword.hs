
{-# Options_GHC -O0 #-}

-- |
--
-- TODO need to carefully check all props against boundary errors!
-- Especially the 2-dim cases!

module QuickCheck.Subword where

import           Data.Vector.Fusion.Util
import           Debug.Trace
import qualified Data.List as L
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           Test.QuickCheck.All
import           Test.QuickCheck.Monadic
#ifdef ADPFUSION_TEST_SUITE_PROPERTIES
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH
#endif

import           Data.PrimitiveArray

import           ADP.Fusion.Core
import           ADP.Fusion.Subword



-- * Epsilon

prop_I_Epsilon ix@(Subword (i:.j)) = zs == ls where
  zs = (id <<< Epsilon ... stoList) maxSWi ix
  ls = [ () | i==j ]

prop_Z1I_Epsilon ix@(Z:.Subword (i:.j)) = zs == ls where
  zs = (id <<< (M:|Epsilon) ... stoList) (ZZ:..maxSWi) ix
  ls = [ Z:.() | i==j ]

--prop_O_Epsilon ix@(Subword (i:.j)) = zs == ls where
--  zs = (id <<< Epsilon ... stoList) maxSWo ix
--  ls = [ () | ix == maxSWo ]

-- * Deletion
--
-- Pure deletions without other symbols behave like @Epsilon@ in that they
-- require a subword of size 0.

prop_I_Deletion ix@(Subword (i:.j)) = zs == ls where
  zs = (id <<< Deletion ... stoList) maxSWi ix
  ls = [ () | i==j ]

prop_Z1I_Deletion ix@(Z:.Subword (i:.j))
  | zs == ls  = True
  | otherwise = error $ show (zs,ls)
  where zs = (id <<< (M:|Deletion) ... stoList) (ZZ:..maxSWi) ix
        ls = [ Z:.() | i==j ]

-- * @Chr@
--
-- Single @Chr@ terminals are active on subword sizes of exactly @1@.

prop_I_Chr ix@(Subword (i:.j)) = zs == ls where
  zs = (id <<< chr csS ... stoList) maxSWi ix
  ls = [ (i,j) | i+1==j ]

prop_Z1I_Chr ix@(Z:.Subword (i:.j))
  | zs == ls  = True
  | otherwise = error $ show (zs,ls)
  where zs = (id <<< (M:|chr csS) ... stoList) (ZZ:..maxSWi) ix
        ls = [ Z:.(i,j) | i+1==j ]

-- * Str

prop_I_ManyV ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = traceShow (ix,zs,ls) False
  where
    zs = (id <<< manyV csS ... stoList) maxSWi ix
    ls = [ (VU.slice i (j-i) csS) ]

prop_I_SomeV ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = traceShow (ix,zs,ls) False
  where
    zs = (id <<< someV csS ... stoList) maxSWi ix
    ls = [ (VU.slice i (j-i) csS) | i<j ]

prop_I_Itbl_ManyV ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = traceShow (ix,zs,ls) False
  where
    zs = ((,) <<< tsI % manyV csS ... stoList) maxSWi ix
    ls = [ (unsafeIndex xsS (subwordI i k), VU.slice k (j-k) csS)
         | k <- [i..j] ]

prop_I_Itbl_SomeV ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = traceShow (ix,zs,ls) False
  where
    zs = ((,) <<< tsI % someV csS ... stoList) maxSWi ix
    ls = [ (unsafeIndex xsS (subwordI i k), VU.slice (k) (j-k) csS)
         | k <- [i..j-1] ]

prop_I_SomeV_Itbl ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = traceShow (ix,zs,ls) False
  where
    zs = ((,) <<< someV csS % tsI ... stoList) maxSWi ix
    ls = [ (VU.slice i (k-i) csS, unsafeIndex xsS (subwordI k j))
         | k <- [i+1..j] ]



-- * Mixed symbols

prop_I_ItNC ix@(Subword (i:.j)) = zs == ls where
  zs = ((,,) <<< tsI % Deletion % chr csS ... stoList) maxSWi ix
  ls = [ ( unsafeIndex xsS (subwordI i (j-1))
         , ()
         , csS VU.! (j-1)
         ) | i >= 0, j >= 1, i<j, j <= highest ]

-- * Outside checks

---- ** two non-terminals on the r.h.s.
----
---- A_ij -> B_ik C_kj
----
---- B*_ik -> A*_ij C_kj
---- C*_kj -> B_ik  A*_ij
--
--prop_O_sv_OI ox@(Subword (i:.k)) = zs === ls where
--  toa = TW (ITbl 0 0 EmptyOk xoS) (\ _ _ -> Id (1,1))
--  tic = TW (ITbl 0 0 EmptyOk xsS) (\ _ _ -> Id (1,1))
--  zs = ((,) <<< toa % tic ... stoList) maxSWo ox
--  ls = [ ( unsafeIndex xoS (subword i j)
--         , unsafeIndex xsS (subword k j) )
--       | j <- [ k .. highest ] ]

--prop_O_sv_IO ox@(Subword (k:.j)) = zs === ls where
--  tib = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs = ((,) <<< tib % toa ... stoList) maxSWo ox
--  ls = [ ( unsafeIndex xsS (subword i k)
--         , unsafeIndex xoS (subword i j) )
--       | j <= highest, i <- [ 0 .. k ] ]
--
---- ** three non-terminals on the r.h.s. (this provides situations where two
---- syntactic terminals are on the same side)
----
---- A_ij -> B_ik C_kl D_lj
----
---- B*_ik -> A*_ij C_kl  D_lj
---- C*_kl -> B_ik  A*_ij D_lj
---- D*_lj -> B_ik  C_kl  A*_ij
--
--prop_O_sv_OII ox@(Subword (i:.k)) = zs === ls where
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  tic = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  tid = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  zs = ((,,) <<< toa % tic % tid ... stoList) maxSWo ox
--  ls = [ ( unsafeIndex xoS (subword i j)
--         , unsafeIndex xsS (subword k l)
--         , unsafeIndex xsS (subword l j) )
--       | j <- [ k .. highest ], l <- [ k .. j ] ]
--
--prop_O_sv_IOI ox@(Subword (k:.l)) = zs === ls where
--  tib = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  tid = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  zs = ((,,) <<< tib % toa % tid ... stoList) maxSWo ox
--  ls = [ ( unsafeIndex xsS (subword i k)
--         , unsafeIndex xoS (subword i j)
--         , unsafeIndex xsS (subword l j) )
--       | i <- [ 0 .. k ], j <- [ l .. highest ] ]
--
--prop_O_sv_IIO ox@(Subword (l:.j)) = zs === ls where
--  tib = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  tic = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs = ((,,) <<< tib % tic % toa ... stoList) maxSWo ox
--  ls = [ ( unsafeIndex xsS (subword i k)
--         , unsafeIndex xsS (subword k l)
--         , unsafeIndex xoS (subword i j) )
--       | j <= highest, i <- [ 0 .. l ], k <- [ i .. l ] ]
--
---- ** four non-terminals on the r.h.s. ?
--
---- ** five non-terminals on the r.h.s. ?
--
---- ** Non-terminal and terminal combinations
--
--prop_O_cOc ox@(Subword (i:.j)) = zs === ls where
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs  = ((,,) <<< chr csS % toa % chr csS ... stoList) maxSWo ox
--  ls  = [ ( csS VU.! (i-1)
--          , unsafeIndex xoS (subword (i-1) (j+1))
--          , csS VU.! (j  ) )
--        | i > 0 && j < highest ]
--
--prop_O_ccOcc ox@(Subword (i:.j)) = zs === ls where
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs  = ((,,,,) <<< chr csS % chr csS % toa % chr csS % chr csS ... stoList) maxSWo ox
--  ls  = [ ( csS VU.! (i-2)
--          , csS VU.! (i-1)
--          , unsafeIndex xoS (subword (i-2) (j+2))
--          , csS VU.! (j  )
--          , csS VU.! (j+1) )
--        | i > 1 && j < highest -1 ]
--
--prop_O_cOccc ox@(Subword (i:.j)) = zs === ls where
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs  = ((,,,,) <<< chr csS % toa % chr csS % chr csS % chr csS ... stoList) maxSWo ox
--  ls  = [ ( csS VU.! (i-1)
--          , unsafeIndex xoS (subword (i-1) (j+3))
--          , csS VU.! (j  )
--          , csS VU.! (j+1)
--          , csS VU.! (j+2) )
--        | i > 0 && j < highest -2 ]
--
---- ** Terminals, syntactic terminals, and non-terminals
--
--prop_O_cOcIc ox@(Subword (i:.k)) = zs === ls where
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  tic = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  zs = ((,,,,) <<< chr csS % toa % chr csS % tic % chr csS ... stoList) maxSWo ox
--  ls = [ ( csS VU.! (i-1)
--         , unsafeIndex xoS (subword (i-1)  j   )
--         , csS VU.! (k  )
--         , unsafeIndex xsS (subword (k+1) (j-1))
--         , csS VU.! (j-1) )
--       | i > 0, j <- [ k+2 .. highest ] ]
--
--prop_O_cIcOc ox@(Subword (k:.j)) = zs === ls where
--  tib = ITbl 0 0 EmptyOk xsS (\ _ _ -> Id (1,1))
--  toa = ITbl 0 0 EmptyOk xoS (\ _ _ -> Id (1,1))
--  zs = ((,,,,) <<< chr csS % tib % chr csS % toa % chr csS ... stoList) maxSWo ox
--  ls = [ ( csS VU.! (i  )
--         , unsafeIndex xsS (subword (i+1) (k-1))
--         , csS VU.! (k-1)
--         , unsafeIndex xoS (subword  i    (j+1))
--         , csS VU.! (j  ) )
--       | j+1 <= highest, k>1, i <- [ 0 .. k-2 ] ]
--
---- ** Epsilonness
--
--prop_O_Epsilon ox@(Subword (i:.j)) = zs === ls where
--  zs = (id <<< Epsilon ... stoList) (maxSWo) ox
--  ls = [ () | i==0 && j==highest ]


-- ** Two synvars next to each other

prop_I_SynSyn ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = error $ show (zs,ls)
  where zs = ((,) <<< tsI % tsI ... stoList) (LtSubword highest) (ix∷Subword I)
        ls = [ ((i,k),(k,j)) | k ← [i..j] ]

-- ** Multi-tape cases

prop_I_2dimIt ix@(Z:.Subword (i:.j):.Subword (k:.l)) = zs === ls where
  zs = (id <<< tsZ2I ... stoList) (ZZ:..LtSubword highest:..LtSubword highest) ix
  ls = [ ( unsafeIndex xsSS ix ) | j<=highest && l<=highest ]

prop_I_2dimcIt ix@(Z:.Subword(i:.j):.Subword(k:.l)) = {- traceShow (zs,ls) $ -} zs === ls where
  zs = ((,) <<< (M:|chr csS:|chr csS) % tsZ2I ... stoList) (ZZ:..LtSubword highest:..LtSubword highest) (ix∷Z:.Subword I:.Subword I)
  ls = [ ( Z :. (csS VU.! i) :. (csS VU.! k)
         , unsafeIndex xsSS (Z :. subword (i+1) j :. subword (k+1) l) )
       | j<=highest && l<=highest
       , i+1<=j && k+1<=l ]

prop_I_2dimItc ix@(Z:.Subword(i:.j):.Subword(k:.l)) = (j<=highest && l<=highest) ==> zs === ls where
  zs = ((,) <<< tsZ2I % (M:|chr csS:|chr csS)  ... stoList) (ZZ:..LtSubword highest:..LtSubword highest) (ix∷Z:.Subword I:.Subword I)
  ls = [ ( unsafeIndex xsSS (Z :. subword i (j-1) :. subword k (l-1))
         , Z :. (csS VU.! (j-1)) :. (csS VU.! (l-1)) )
       | j<=highest && l<=highest
       , i+1<=j && k+1<=l ]

prop_I_2dimcItc ix@(Z:.Subword(i:.j):.Subword(k:.l)) = (j<=highest && l<=highest) ==> zs === ls where
  zs = ((,,) <<< (M:|chr csS:|chr csS) % tsZ2I % (M:|chr csS:| chr csS) ... stoList) (ZZ:..LtSubword highest:..LtSubword highest)
        (ix∷Z:.Subword I:.Subword I)
  ls = [ ( Z :. (csS VU.! i) :. (csS VU.! k)
         , unsafeIndex xsSS (Z :. subword (i+1) (j-1) :. subword (k+1) (l-1))
         , Z :. (csS VU.! (j-1)) :. (csS VU.! (l-1)) )
       | j<=highest && l<=highest
       , i+2<=j && k+2<=l ]

-- ** Complex

prop_I_ChrSynChrSynChr ix@(Subword (i:.j))
  | zs == ls  = True
  | otherwise = error $ show (ix,zs,ls)
  where zs = ((,,,,) <<< chr csS % tsI % chr csS % tsI % chr csS ... stoList) (LtSubword highest) (ix∷Subword I)
        ls = [ (csS VU.! i, (i+1,k), csS VU.! k, (k+1,j-1), csS VU.! (j-1)) | k ← [i+1..j-2] ]



stoList = unId . SM.toList

highest = 20

maxSWi :: LimitType (Subword I)
maxSWi = LtSubword highest

--maxSWo :: Subword O
--maxSWo = subword 0 highest

csS :: VU.Vector (Int,Int)
csS = VU.fromList [ (i,i+1) | i <- [0 .. highest-1] ] -- this should be @highest -1@, we should die if we see @(highest,highest+1)@

-- in case we want to test behaviour against stupid input.

csShort :: VU.Vector (Int,Int)
csShort = VU.fromList [ (i,i+1) | i <- [0 .. highest `div` 2] ]

xsS :: Unboxed (Subword I) (Int,Int)
xsS = fromList (LtSubword highest) [ (i,j) | i <- [ 0 .. highest ] , j <- [ i .. highest ] ]

--xoS :: Unboxed (Subword O) (Int,Int)
--xoS = fromList (subword 0 0) (subword 0 highest) [ (i,j) | i <- [ 0 .. highest ] , j <- [ i .. highest ] ]

xsSS :: Unboxed (Z:.Subword I:.Subword I) ( (Int,Int) , (Int,Int) )
xsSS = fromAssocs (ZZ:..LtSubword highest:..LtSubword highest) ((-1,-1),(-1,-1))
        $ Prelude.map (\((i,j),(k,l)) -> (Z:.subword i j:.subword k l, ((i,j),(k,l)) )) [ ((i,j) , (k,l)) | i <- [0 .. highest], j <-[i .. highest], k <- [0 .. highest], l <- [0 .. highest] ]

tsI   = TW (ITbl @0 @0 EmptyOk xsS)                (\ (_∷LimitType (Subword I)) (_::Subword I) -> Id (1::Int,1::Int))
tsZ2I = TW (ITbl @0 @0 (Z:.EmptyOk:.EmptyOk) xsSS)
           (\ (_∷LimitType (Z:.Subword I:.Subword I)) (_∷Z:.Subword I:.Subword I) -> Id ((1::Int,1::Int),(1::Int,1::Int)))

-- * general quickcheck stuff

options = stdArgs {maxSuccess = 10000}

customCheck = quickCheckWithResult options

return []
allProps = $forAllProperties customCheck



#ifdef ADPFUSION_TEST_SUITE_PROPERTIES
testgroup_subword = $(testGroupGenerator)
#endif

