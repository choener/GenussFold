
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
import Data.Ord
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

bpmax :: Monad m => SigPKN m Int Int (Int,Char) (Int,Char)
--{{{
{-# Inline bpmax #-}
bpmax = SigPKN
  { unp = const
  , jux = \ x c y d -> if snd c `pairs` snd d then x + y + 1 else -777777
  , pse = \ () () x y -> x + y
  , nil = \ () -> 0
  , pkk = \ (Z:.x:.()) (Z:.a:.()) y (Z:.():.z) (Z:.():.b) -> if snd a `pairs` snd b then x + y + z + 1 else -888888
--  , sng = \ (Z:.x:.()) (Z:.a:.())   (Z:.():.z) (Z:.():.b) -> if snd a `pairs` snd b then x +     z + 1 else -888888
  , nll = \ (Z:.():.()) -> 0
  , idd = id
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

pretty :: Monad m => SigPKN m [String] [[String]] (Int,Char) (Int,Char)
--{{{
{-# Inline pretty #-}
pretty = SigPKN
  { unp = \ [x] c     -> [x ++ "." ++ shw c]
  , jux = \ [x] c [y] d -> [x ++ "(" ++ shw c ++ y ++ ")" ++ shw d]
  , pse = \ () () [x1,x2] [y1,y2] -> [concat $ intersperse " " [x1,y1,x2,y2]]
  , nil = \ ()      -> [""]
  , pkk = \ (Z:.[x]:.()) (Z:.a:.()) [y1,y2] (Z:.():.[z]) (Z:.():.b) -> [x ++ "[" ++ shw a ++ y1 , y2 ++ z ++ "]" ++ shw b]
--  , sng = \ (Z:.[x]:.()) (Z:.a:.())         (Z:.():.[z]) (Z:.():.b) -> [x ++ "[" ++ shw a       ,       z ++ "]" ++ shw b]
  , nll = \ (Z:.():.()) -> ["",""]
  , idd = id
  , h   = SM.toList
  } where shw (k,c) = show k ++ [c]
--}}}

data BT = Unp BT (Int,Char)
        | Jux BT (Int,Char) BT (Int,Char)
        | PSE BT BT
        | Nil
        | PKK BT (Int,Char) BT BT (Int,Char)
        | SNG BT (Int,Char)    BT (Int,Char)
        | NLL
        | IDD BT
  deriving (Eq,Ord,Show)

-- | Decompose into PK-free substructures. Might introduce more substructures than necessary.

bt2pk :: BT -> [(Int,Int)] -- [Either (Int,Int) (Int,Int)]
bt2pk = go
  where
    go (Unp bt _) = go bt
    go (Jux xss (l,_) yss (r,_)) = (l,r) : go xss ++ go yss
    go (PSE xss yss) = go xss ++ go yss
    go Nil = []
    go (PKK xss (l,_) yss zss (r,_)) = (l,r) : go xss ++ go yss ++ go zss
    go (SNG xss (l,_)     zss (r,_)) = (l,r) : go xss ++ go zss
    go NLL = []
    go (IDD xs) = go xs

-- | Costly!

groupify :: [(Int,Int)] -> [[(Int,Int)]]
groupify [] = []
groupify (x:xs) = map sort . rec . sortBy (comparing (Down . length)) $ go x xs
  where
    rec [] = []
    rec [r] = [r]
    rec (r:rs) = r : groupify (concat rs)
    go (l,r) [] = [[(l,r)]]
    go (l,r) xs = (if not (null ass) then (l,r) `prependHead` go a as else [[(l,r)]]) ++ (if not (null bss) then go b bs else [])
      where
        (ass,bss) = partition (\(x,y) -> y<l || r<x || l<x && y<r || x<l && r<y) xs
        a:as = ass; b:bs = bss
        prependHead q (z:zs) = (q:z) : zs

showbt :: Int -> BT -> String
showbt k = unlines . map (stringify k) . groupify . sort . bt2pk

stringify :: Int -> [(Int,Int)] -> String
stringify k xs = VU.toList $ go $ VU.replicate k '.'
  where
    go v = v VU.// ([ (x,'(') | (x,_) <- xs ] ++ [ (x,')') | (_,x) <- xs ])

bt :: Monad m => SigPKN m BT [BT] (Int,Char) (Int,Char)
--{{{
{-# Inline bt #-}
bt = SigPKN
  { unp = Unp
  , jux = Jux
  , pse = \ () () x y -> PSE x y
  , nil = const Nil
  , pkk = \ (Z:.x:.()) (Z:.a:.()) bt (Z:.():.z) (Z:.():.b) -> PKK x a bt z b
--  , sng = \ (Z:.x:.()) (Z:.a:.())    (Z:.():.z) (Z:.():.b) -> SNG x a    z b
  , nll = const NLL
  , idd = IDD
  , h   = SM.toList
  } where shw (k,c) = show k ++ [c]
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
runInsideForward i' = runST $ do
  let i = VU.indexed i'
      n = VU.length i
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

type X' bo so = TwITblBt bo so (Dense VU.Vector) EmptyOk (Subword I) Int Id Id BT
type T' bo so = TwITblBt bo so (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.Subword I:.Subword I) Int Id Id BT

runInsideBacktrack :: VU.Vector Char -> Z:.X 0 2:.T 0 0:.T 0 1 -> [BT]
--{{{
{-# NoInline runInsideBacktrack #-}
runInsideBacktrack i' (Z:.t:.u:.v) = unId $ axiom b
  where
    i = VU.indexed i'
    !(Z:.b:._:._) = gPKN (bpmax <|| bt)
                      (toBacktrack t (undefined :: Id a -> Id a))
                      (toBacktrack u (undefined :: Id a -> Id a))
                      (toBacktrack v (undefined :: Id a -> Id a))
                      (chr i)
                      (chr i)
                      :: Z:.X' 0 2:.T' 0 0:.T' 0 1
--}}}



-- | Wrapper function that just calls the forward and backtrack parts of the DP algorithm and does a
-- bit of input / output manipulation.

pknPairMax :: Int -> String -> (Int,[BT],String)
--{{{
{-# NoInline pknPairMax #-}
pknPairMax k inp = (d, take k bs, showPerfCounter perf) where
  i = VU.fromList . Prelude.map toUpper $ inp
  n = VU.length i
  Mutated (Z:.t:.u:.v) perf eachPerf = runInsideForward i
  d = unId $ axiom t
  bs = runInsideBacktrack i (Z:.t:.u:.v)
--}}}

testrun :: Int -> String -> IO ()
testrun k inp = do
  let (s,bt,_) = pknPairMax k inp
  forM_ bt $ \b -> do
    print s
    print b
    putStrLn $ showbt (length inp) b

