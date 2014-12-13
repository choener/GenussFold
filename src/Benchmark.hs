
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as VV
import           Text.Printf
import           Data.Tuple (swap)
import           Control.Applicative ((<$>))
import           System.Random.MWC
import           Control.DeepSeq

import qualified Data.Bijection.Vector as BV
import qualified Data.Bijection.Vector.Unboxed as BU
import qualified Data.Bijection.Vector.Storable as BS
import qualified Data.Bijection.Map as BM
import qualified Data.Bijection.Class as B



runLookupBench xs' z = bench s $ whnf allLR xs'
  where s = printf "%5d" (B.size z)
        lL k = B.lookupL z k
        lR k = B.lookupR z k
        allL xs = VV.foldl' f 0 . VV.map lL . VG.convert $ xs
        allR xs = VV.foldl' f 0 . VV.map lR . VG.convert $ xs
        allLR xs = allL xs + allR xs
        f k (Just (!x)) = max k x
        f k _           = k
{-# INLINE runLookupBench #-}

benchLookup xs z = allLR -- bench s $ whnf allLR xs'
  where lL k = B.lookupL z k
        lR k = B.lookupR z k
        allL = VV.foldl' f 0 . VV.map lL . VG.convert $ xs
        allR = VV.foldl' f 0 . VV.map lR . VG.convert $ xs
        allLR = allL + allR
        f k (Just (!x)) = max k x
        f k _           = k
{-# INLINE benchLookup #-}

benchVU :: VU.Vector Int -> BU.Bimap Int Int -> Int
benchVU = benchLookup
{-# NOINLINE benchVU #-}

benchBM :: VU.Vector Int -> BM.Bimap Int Int -> Int
benchBM = benchLookup
{-# NOINLINE benchBM #-}

main :: IO ()
main = do
  lkup :: VU.Vector Int <- withSystemRandom . asGenIO $ \gen -> uniformVector gen 10
  inputs :: [[Int]] <- mapM (\l -> withSystemRandom . asGenIO $ \gen -> VU.toList <$> uniformVector gen l) [1, 5, 10, 50, 100, 1000] -- [1,10,100,1000,10000]
  let zVV :: [BV.Bimap Int Int] = map (\i -> B.fromList $ zip i i) inputs
  let zVU :: [BU.Bimap Int Int] = map (\i -> B.fromList $ zip i i) inputs
  let zVS :: [BS.Bimap Int Int] = map (\i -> B.fromList $ zip i i) inputs
  let zMS :: [BM.Bimap Int Int] = map (\i -> B.fromList $ zip i i) inputs
  deepseq (lkup,inputs,zVV,zVU,zVS,zMS) `seq` defaultMain
    [ bgroup "5"
      [ bench "vector/ unboxed" $ whnf (benchVU lkup) (zVU !! 1)
      , bench "   map/  strict" $ whnf (benchBM lkup) (zMS !! 1)
      ]
    ]
--    [ bgroup "vector/    boxed" (map (`benchLookup` lkup) zVV)
--    , bgroup "vector/ storable" (map (`benchLookup` lkup) zVS)
--    [ bgroup "vector/  unboxed" (map (`benchLookup` lkup) zVU)
--    , bgroup "   map/   strict" (map (`benchLookup` lkup) zMS)
--    ]

