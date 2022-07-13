
--{{{ GHC Options
{-# Options_GHC -fdicts-cheap                  #-}
{-# Options_GHC -flate-dmd-anal                #-}
{-# Options_GHC -fmax-worker-args=1000         #-}
{-# Options_GHC -fspec-constr-count=20000      #-}
{-# Options_GHC -fspec-constr-keen             #-}
{-# Options_GHC -fspec-constr-recursive=20000  #-}
{-# Options_GHC -fspec-constr-threshold=1000   #-}
{-# Options_GHC -Wno-partial-type-signatures   #-}
-- both, full laziness and no liberate case are essential to have things inline nicely!
{-# Options_GHC -fno-full-laziness             #-}
{-# Options_GHC -fno-liberate-case             #-}

{-# Language MagicHash #-}
--}}}

-- | The PKN DP algorithm.

module BioInf.GenussFold.PKN where

--{{{ Imports
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char (toUpper,toLower)
import Data.List
import Data.Vector.Fusion.Util
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import Text.Printf

import ADPfusion.Subword
import Data.PrimitiveArray as PA hiding (map)
import FormalLanguage

import BioInf.GenussFold.PKN.Grammar
--}}}



-- | Define a simple basepair maximization algebra over the PKN grammar. The weird "non-pairing"
-- scores, say @-777777@, can be used to check that fusion happens correctly.

bpmax :: Monad m => SigPKN m Int Int Char Char
--{{{
{-# Inline bpmax #-}
bpmax = SigPKN
  { unp = const
  , jux = \ x c y d -> if c `pairs` d then x + y + 1 else -777777
  , pse = \ () () x y -> x + y
  , nil = \ () -> 0
  , pkk = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if a `pairs` b then x + y + z + 1 else -888888
  , nll = \ (Z:.():.()) -> 0
  , idd = \x -> x
  , h   = SM.foldl' max (-999999)
  }
--}}}

pairs :: Char -> Char -> Bool
--{{{
{-# Inline pairs #-}
pairs c d = go c d
  where
    go 'A' 'U' = True
    go 'C' 'G' = True
    go 'G' 'C' = True
    go 'G' 'U' = True
    go 'U' 'A' = True
    go 'U' 'G' = True
    go _ _     = False
--}}}

-- | The pretty algebra is used for backtracing.

pretty :: Monad m => SigPKN m [String] [[String]] Char Char
--{{{
{-# Inline pretty #-}
pretty = SigPKN
  { unp = \ [x] c     -> [x ++ "."]
  , jux = \ [x] c [y] d -> [x ++ "(" ++ y ++ ")"]
  , pse = \ () () [x1,x2] [y1,y2] -> [x1 ++ y1 ++ x2 ++ y2]
  , nil = \ ()      -> [""]
  , pkk = \ (Z:.[x]:.()) (Z:.a:.()) [y1,y2] (Z:.():.[z]) (Z:.():.b) -> [x ++ "[" ++ y1 , y2 ++ z ++ "]"]
  , nll = \ (Z:.():.()) -> ["",""]
  , idd = id
  , h   = SM.toList
  }
--}}}

-- | Scalar-style table for the Vienna part of the algorithm.

type X bo so = TwITbl bo so Id (Dense VU.Vector) EmptyOk (Subword I) Int

-- | Table of @(Subword,Subword)@ for pseudoknotted structures.

type T bo so = TwITbl bo so Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.Subword I:.Subword I) Int

-- | The heavy lifting happens here. Initialize three array structures, create the guideIndex
-- structure (which is trivial because we have just a single big-order), and fill the tables with
-- the help of the guide index.

runInsideForward :: VU.Vector Char -> Mutated (Z:.X 0 2:.T 0 0:.T 0 1)
--{{{
{-# NoInline runInsideForward #-}
runInsideForward i = runST $ do
  let n = VU.length i
  arrS <- newWithPA (LtSubword n) (-999999)
  arrUU <- newWithPA (ZZ:..LtSubword n:..LtSubword n) (-999999)
  arrVV <- newWithPA (ZZ:..LtSubword n:..LtSubword n) (-999999)
  let guideIndex = Z:.BOI @0 (upperBound arrUU)
  fillTablesDim guideIndex
    $ gPKN bpmax
        (ITbl @_ @_ @_ @_ @_ @_ EmptyOk arrS)
        (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrUU)
        (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrVV)
        (chr i)
        (chr i)
--}}}

type X' bo so = TwITblBt bo so (Dense VU.Vector) EmptyOk (Subword I) Int Id Id [String]
type T' bo so = TwITblBt bo so (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.Subword I:.Subword I) Int Id Id [String]

runInsideBacktrack :: VU.Vector Char -> Z:.X 0 2:.T 0 0:.T 0 1 -> [[String]]
--{{{
{-# NoInline runInsideBacktrack #-}
runInsideBacktrack i (Z:.t:.u:.v) = unId $ axiom b
  where !(Z:.b:._:._) = gPKN (bpmax <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (chr i)
                          (chr i)
                          :: Z:.X' 0 2:.T' 0 0:.T' 0 1
--}}}



-- | Wrapper function that just calls the forward and backtrack parts of the DP algorithm and does a
-- bit of input / output manipulation.

pknPairMax :: Int -> String -> (Int,[[String]],String)
--{{{
{-# NoInline pknPairMax #-}
pknPairMax k inp = (d, take k bs, showPerfCounter perf) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  Mutated (Z:.t:.u:.v) perf eachPerf = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t:.u:.v)
--}}}

