
{-# Options_GHC -Wno-partial-type-signatures  #-}
{-# Options_GHC -fspec-constr-count=1000      #-}
{-# Options_GHC -fspec-constr-keen            #-}
{-# Options_GHC -fspec-constr-recursive=1000  #-}
{-# Options_GHC -fspec-constr-threshold=1000  #-}
{-# Options_GHC -fmax-worker-args=1000        #-}
-- both, full laziness and no liberate case are essential to have things inline nicely!
{-# Options_GHC -fno-full-laziness            #-}
{-# Options_GHC -fno-liberate-case            #-}

-- |

module BioInf.GenussFold.PKN where

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



bpmax :: Monad m => SigPKN m Int Int Char Char
{-# Inline bpmax #-}
bpmax = SigPKN
  { unp = const
  , jux = \ x c y d -> if c `pairs` d then x + y + 1 else -999999
  , pse = \ () () x y -> x + y
  , nil = \ ()      -> 0
  , pkk = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if a `pairs` b then x + y + z + 1 else -888888
  , nll = \ (Z:.():.()) -> 0
  , idd = id
  , h   = SM.foldl' max (-999999)
  }

pairs :: Char -> Char -> Bool
{-# Inline pairs #-}
pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'

-- |
--
-- TODO It could be beneficial to introduce
-- @type Splitted = Either String (String,String)@
-- or something isomorphic. While [String] works, it allows for too many
-- possibilities here! ([] ist lightweight, on the other hand ...)

pretty :: Monad m => SigPKN m [String] [[String]] Char Char
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

-- |
--
-- @
-- [{]}(())
-- caguagcu
-- [ ]
--  { }
--     (())
-- @

pknPairMax :: Int -> String -> (Int,[[String]])
{-# NoInline pknPairMax #-}
pknPairMax = undefined
{-
pknPairMax k inp = (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  !(Z:.t:.u:.v) = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t:.u:.v)
-}

type X = TwITbl 0 0 Id (Dense VU.Vector) EmptyOk (Subword I) Int
type T = TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.Subword I:.Subword I) Int

runInsideForward :: VU.Vector Char -> Mutated (Z:.X:.T:.T)
{-# NoInline runInsideForward #-}
runInsideForward i = runST $ do
  let n = VU.length i
  arrS <- newWithPA (LtSubword n) (-999999)
  arrUU <- newWithPA (ZZ:..LtSubword n:..LtSubword n) (-999999)
  arrVV <- newWithPA (ZZ:..LtSubword n:..LtSubword n) (-999999)
  let guideIndex = Z:.BOI @0 (upperBound arrUU)
  tbls@(Mutated (Z:.s:.uu:.vv) _ _) <- fillTablesDim guideIndex
    $ gPKN bpmax
        (ITbl @_ @_ @_ @_ @_ @_ EmptyOk arrS)
        (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrUU)
        (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrVV)
        (chr i)
        (chr i)
  return tbls

type X' = TwITblBt 0 0 (Dense VU.Vector) EmptyOk (Subword I) Int Id Id [String]
type T' = TwITblBt 0 0 (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.Subword I:.Subword I) Int Id Id [String]

runInsideBacktrack :: VU.Vector Char -> Z:.X:.T:.T -> [[String]]
{-# NoInline runInsideBacktrack #-}
runInsideBacktrack i (Z:.t:.u:.v) = unId $ undefined -- axiom b
{-
  where !(Z:.b:._:._) = gPKN (bpmax <|| pretty)
                          (toBacktrack t (undefined :: Id a -> Id a))
                          (toBacktrack u (undefined :: Id a -> Id a))
                          (toBacktrack v (undefined :: Id a -> Id a))
                          (chr i)
                          (chr i)
                          :: Z:.X':.T':.T'
-}
