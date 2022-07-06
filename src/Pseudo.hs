
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf
import           System.Environment
import           Debug.Trace (traceShow)

import           ADPfusion.Core
import           ADPfusion.PointL
import           Data.PrimitiveArray as PA hiding (map)

import           FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
N: <U,2>
T: c
S: S
S -> unp <<< S c
S -> nil <<< e
S -> pse <<< U U

-- <U,U> -> pk1 <<< <U,U> [-,S] [-,c]
-- <U,U> -> pk2 <<< <U,U> [S,-] [c,-]
[U,U] -> pk3 <<< [S,-] [-,S] [c,c]  -- TODO want to be able to write [S,S], but [S,-] [-,S] works as well
[U,U] -> nll <<< [e,e]

//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

bpmax :: Monad m => SigPKN m Int Int Char Char
bpmax = SigPKN
  { unp = \ x c     -> x
  , pse = \ () x    -> x
  , nil = \ ()      -> 1
  , pk3 = \ (Z:.s:._) (Z:._:.t) (Z:.a:.b) -> s * t
  , nll = \ (Z:.():.()) -> 1
  , h   = SM.foldl' (+) 0
  }
{-# INLINE bpmax #-}



runPseudoknot :: Int -> String -> (Int,[[String]])
runPseudoknot k inp = traceShow (perf, eachPerf) (d, take k bs) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  Mutated (Z:.s:.u) perf eachPerf = runInsideForward i
  d = unId $ axiom s
  bs = [] -- runInsideBacktrack i (Z:.t:.u:.v)
{-# NOINLINE runPseudoknot #-}

runInsideForward
  :: VU.Vector Char
  -> Mutated (Z
                :.TwITbl 0 0 Id (Dense VU.Vector) EmptyOk               (PointL I)              Int
                :.TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Int
             )
{-# NoInline runInsideForward #-}
runInsideForward i = runST $ do
  let n = VU.length i
  arrS  <- newWithPA (LtPointL n)                   0
  arrUU <- newWithPA (ZZ:..LtPointL n:..LtPointL n) 0
  let guideIndex = Z:. BOI @0 (upperBound arrUU)
  fillTablesDim guideIndex $ gPKN bpmax
    (ITbl @_ @_ @_ @_ @_ @_ EmptyOk               arrS)
    (ITbl @_ @_ @_ @_ @_ @_ (Z:.EmptyOk:.EmptyOk) arrUU)
    (chr i) (chr i)

--runInsideBacktrack :: VU.Vector Char -> Z:.X:.T:.T -> [[String]]
--runInsideBacktrack i (Z:.t:.u:.v) = unId $ axiom b
--  where !(Z:.b:._:._) = gPKN (bpmax <|| pretty)
--                          (toBacktrack t (undefined :: Id a -> Id a))
--                          (toBacktrack u (undefined :: Id a -> Id a))
--                          (toBacktrack v (undefined :: Id a -> Id a))
--                          (chr i)
--{-# NoInline runInsideBacktrack #-}

main = do
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (s,xs) = runPseudoknot k l
    print s
    mapM_ (\[x] -> printf "%s %5d\n" x s) xs

