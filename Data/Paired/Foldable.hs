
-- | 

module Data.Paired.Foldable where

import Data.IntMap as IM
import Data.Foldable as F
import Data.List as L
import Control.Arrow ((***))
import Data.Vector as V
import Data.Vector.Generic as VG

import Data.Paired.Common
import Math.TriangularNumbers



-- | Generalized upper triangular elements. Given a list of elements
-- @[e_1,...,e_k]@, we want to return pairs @(e_i,e_j)@ such that we have
-- all ordered pairs with @i<j@ (if @NoDiag@onal elements), or @i<=j@ (if
-- @OnDiag@onal elements).
--
-- @upperTri@ will force the spine of @t a@ but is consumed linearly with
-- a strict @Data.Foldable.foldl'@. Internally we keep a @Data.IntMap@ of
-- the retained elements.
--
-- This is important if the @Enumerate@ type is set to @FromN k n@. We
-- start at the @k@th element, and produce @n@ elements.
--
-- TODO compare @IntMap@ and @HashMap@.
--
-- TODO inRange is broken.

upperTri
  :: (Foldable t)
  => OnDiag
  -> Enumerate
  -> t a
  -> (IntMap a, Int, [(a,a)])
upperTri d e xs' = (undefined, numElems, ys)
  where ys   = case e of {All -> id ; FromN _ s -> L.take s}
             . L.unfoldr go $ initEnum e d
        -- how many elements we will emit depends on enumeration and on
        -- diagonal element counting
        numElems
          | All <- e       = allSize
          | FromN s k <- e = if s+k > allSize then max 0 (allSize - s) else k
        -- Construct an intmap @imp@ of all elements in the accepted range.
        -- At the same time, return the length or size of the foldable
        -- container we gave as input. @xs'@ is touched only once and can
        -- be efficiently consumed.
        -- @
        -- imp = IM.fromList . L.filter (inRange . fst) . L.zip [0..] $ F.toList xs'
        -- len = F.length xs'
        -- @
        (!imp,!len) = F.foldl' (\(!i,!l) x -> (if inRange l then IM.insert l x i else i,l+1)) (IM.empty, 0) xs'
        allSize = len * (len + if d == OnDiag then 1 else -1) `div` 2
        -- with minL we know the starting index for the 1st element, with
        -- strtR the starting index for the 2nd element. With maxL we know
        -- the stopping index for the 1st element.
        (minL,strtR) = case e of { All -> (0,len-1) ; FromN s k -> fromLinear (len-1) s     }
        (maxL,stopR) = case e of { All -> (0,len-1) ; FromN s k -> fromLinear (len-1) (s+k) }
        strtZ = case e of { All -> len-1 ; FromN s k -> min (len-1) (strtR+k) }
        stopA = case e of { All -> 0 ; FromN s k -> max 0 (stopR-k) }
        -- TODO we have a lot of redundant switching around All/FromN On/No
        inRange z = True
        {-
        inRange z =  minL  <= z && z <= maxL
                  || strtR <= z && z <= strtZ
                  || stopA <= z && z <= stopR
                  -}
        -- index into the generated vector @xs@ when generating elements
        -- via @go@
        go (k,l)
          | k >= len  = Nothing
          | l >= len  = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just ((imp IM.! k, imp IM.! l), (k,l+1))
        -- Initialize the enumeration at the correct pair @(i,j)@. From
        -- then on we can @take@ the correct number of elements, or stream
        -- all of them.
        initEnum All OnDiag = (0,0)
        initEnum All NoDiag = (0,1)
        initEnum (FromN s k) OnDiag
          | s >= allSize = (len,len)
          | otherwise    = fromLinear (len-1) s
        initEnum (FromN s k) NoDiag
          | s >= allSize = (len,len)
          | otherwise    = id *** (+1) $ fromLinear (len-2) s

{-
upperTriVG d as = (z, unfoldrN z go (0,if d == OnDiag then 0 else 1))
  where la = VG.length as
        z  = la * (la + if d == OnDiag then 1 else 0) `div` 2
        go (k,l)
          | k >= la   = Nothing
          | l >= la   = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just ((as `VG.unsafeIndex` k, as `VG.unsafeIndex` l), (k,l+1))
{-# Inline upperTriVG #-}
-}

