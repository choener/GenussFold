{-# Options_GHC -Wno-partial-type-signatures #-}
{-# Options_GHC -fspec-constr-count=100      #-}
{-# Options_GHC -fspec-constr-keen           #-}
{-# Options_GHC -fspec-constr-recursive=100  #-}
{-# Options_GHC -fspec-constr-threshold=100  #-}
{-# Options_GHC -fmax-worker-args=100        #-}

-- | Nussinovs RNA secondary structure prediction algorithm via basepair
-- maximization.

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List as L
import           Data.Vector.Fusion.Util
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
--import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.Environment (getArgs)
import           Text.Printf

import           Data.PrimitiveArray as PA

import           ADP.Fusion.Subword



data Nussinov m x r c = Nussinov
  { unp ∷ x → c → x
  , jux ∷ x → c → x → c → x
  , nil ∷ () → x
  , h   ∷ SM.Stream m x → m r
  }

makeAlgebraProduct ''Nussinov

{-
 - due to backtracking schemes, we need a bunch of combintors
 -
 - how to deal with sup-optimal backtracking, without having to use (*||) ?

(<||)   :: Single a -> List b   -> List b         -- co-optimal backtracking
(*||)   :: Vector a -> List b   -> List (a,b)     -- classified co-optimal backtracking
(***)   :: Single a -> Single b -> Vector (a,b)   -- classified DP

-}

bpmax ∷ Monad m ⇒ Nussinov m Int Int Char
bpmax = Nussinov
  { unp = \ x c     → x
  , jux = \ x c y d → if c `pairs` d then x + y + 1 else -999999
  , nil = \ ()      → 0
  , h   = SM.foldl' max (-999999)
  }
{-# INLINE bpmax #-}

prob ∷ Monad m ⇒ Nussinov m Double Double Char
prob = Nussinov
  { unp = \ x c     → 0.3 * x
  , jux = \ x c y d → 0.6 * if c `pairs` d then x * y else 0
  , nil = \ ()      → 0.1
  , h   = SM.foldl' (+) 0
  }

-- |

pairs !c !d
  =  c=='A' && d=='U'
  || c=='C' && d=='G'
  || c=='G' && d=='C'
  || c=='G' && d=='U'
  || c=='U' && d=='A'
  || c=='U' && d=='G'
{-# INLINE pairs #-}

pretty ∷ Monad m ⇒ Nussinov m String [String] Char -- (SM.Stream m String)
pretty = Nussinov
  { unp = \ x c     → x ++ "."
  , jux = \ x c y d → x ++ "(" ++ y ++ ")"
  , nil = \ ()      → ""
  , h   = SM.toList -- return . id
  }
{-# INLINE pretty #-}

prettyL ∷ Monad m ⇒ Nussinov m String String Char
prettyL = Nussinov
  { unp = \ x c     → x ++ "."
  , jux = \ x c y d → x ++ "(" ++ y ++ ")"
  , nil = \ ()      → ""
  , h   = SM.head -- return . id
  }
{-# INLINE prettyL #-}

grammar Nussinov{..} !c !t' =
  let t = TW t' ( unp <<< t % c           |||
                  jux <<< t % c % t % c   |||
                  nil <<< Epsilon         ... h
                )
  in Z:.t
{-# INLINE grammar #-}

runNussinov ∷ Int → String → (Int,[String],PerfCounter)
runNussinov k inp = (d, take k bs,perf) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  Mutated (Z:.t) perf _ = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i t
{-# NOINLINE runNussinov #-}

runInsideForward ∷ VU.Vector Char → Mutated (Z:.TwITbl 0 0 Id Unboxed EmptyOk (Subword I) Int)
runInsideForward i = runST $ do
  arr ← newWithPA (LtSubword n) (-999999)
  fillTables $
    grammar bpmax
      (chr i)
      (ITbl EmptyOk arr)
  where n = VU.length i
{-# NoInline runInsideForward #-}

runInsideBacktrack ∷ VU.Vector Char → TwITbl 0 0 Id Unboxed EmptyOk (Subword I) Int → [String]
runInsideBacktrack i t = unId $ axiom b
  where !(Z:.b) = grammar (bpmax <|| pretty) (chr i) (toBacktrack t (undefined ∷ Id a → Id a))
--                    :: Z:.TwITblBt Unboxed EmptyOk (Subword I) Int Id Id String
{-# NoInline runInsideBacktrack #-}

main = do
  as <- getArgs
  let k = if null as then 1 else read $ head as
  ls <- lines <$> getContents
  forM_ ls $ \l -> do
    putStrLn l
    let (s,xs,perf) = runNussinov k l
    print perf
    mapM_ (\x -> printf "%s %5d\n" x s) xs

