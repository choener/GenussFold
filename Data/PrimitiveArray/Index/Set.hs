
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language EmptyDataDecls #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

-- | Set with and without interfaces. We provide instances for sets, and
-- sets with one or two interfaces. The @First@ and @Last@ annotation is
-- purely cosmetical (apart from introducing type safety).

module Data.PrimitiveArray.Index.Set where

import           Data.Aeson
import           Data.Binary
import           Data.Bits
import           Data.Bits.Extras (Ranked)
import           Data.Serialize
import           Data.Vector.Fusion.Stream.Size
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Unbox(..))

import           Data.PrimitiveArray.Index.Class
import           Data.Bits.Ordered



-- | Certain sets have an interface, a particular element with special
-- meaning. In this module, certain ``meanings'' are already provided.
-- These include a @First@ element and a @Last@ element. We phantom-type
-- these to reduce programming overhead.

newtype Interface t = Interface Int
  deriving (Eq,Ord,Read,Show,Generic)

derivingUnbox "Interface"
  [t| forall t . Interface t -> Int |]
  [| \(Interface i) -> i            |]
  [| Interface                      |]

-- | Declare the interface to be the start of a path.

data First

-- | Declare the interface to be the end of a path.

data Last

-- | Declare the interface to match anything.
--
-- TODO needed? want to use later in ADPfusion

data Any

-- | Newtype for a bitset. We'd use @Word@s but that requires more shape
-- instances.

newtype BitSet = BitSet Int
  deriving (Eq,Ord,Read,Show,Generic,FiniteBits,Ranked,Num,Bits)

instance Binary    BitSet
instance Serialize BitSet
instance ToJSON    BitSet
instance FromJSON  BitSet

derivingUnbox "BitSet"
  [t| BitSet     -> Int |]
  [| \(BitSet s) -> s   |]
  [| BitSet             |]

{-
-- | Add interface options to a bitset.

data a :> b = !a :> !b
  deriving (Eq,Ord,Show,Generic)

infixl 3 :>

derivingUnbox "StrictIFace"
  [t| forall a b . (Unbox a, Unbox b) => (a:>b) -> (a,b) |]
  [| \(a:>b) -> (a, b) |]
  [| \(a,b)  -> (a:>b) |]

instance (Binary    a, Binary    b) => Binary    (a:>b)
instance (Serialize a, Serialize b) => Serialize (a:>b)
instance (ToJSON    a, ToJSON    b) => ToJSON    (a:>b)
instance (FromJSON  a, FromJSON  b) => FromJSON  (a:>b)
-}

{-
instance Index z => Index (z:.BitSet) where
  type LH (z:.BitSet) = LH z :. Int
  linearIndex (ls:.l) (hs:.h) (zs:.BitSet z) = linearIndex ls hs zs * (h+1) + z - l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (z:.BitSet s) =
    let b = popCount s
    in  smallestLinearIndex z :. (2^b -1)
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (z:.BitSet s) =
    let b = popCount s
    in  largestLinearIndex z :. (2^b -1)
  {-# INLINE largestLinearIndex #-}
  {-
  streamUp (ls:.BitSet l) (hs:.BitSet h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk i = return (i,2^(popCount l) -1)
          step (i,k)
            | k > fin   = return $ SM.Done
            | otherwise = return $ SM.Yield (i:.BitSet (VU.unsafeIndex v k)) (i,k+1)
          !fin = 2^(popCount h) -1
          !v   = popCntMemoInt (popCount h)
          {-# INLINE [0] mk #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.BitSet l) (hs:.BitSet h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk i = return (i,2^(popCount h) -1)
          step (i,k)
            | k < fin   = return $ SM.Done
            | otherwise = return $ SM.Yield (i:.BitSet (VU.unsafeIndex v k)) (i,k-1)
          !fin = 2^(popCount l) -1
          !v   = popCntMemoInt (popCount h)
          {-# INLINE [0] mk #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}
  -}

instance Index BitSet
-}

instance Index BitSet where
  linearIndex l _ (BitSet z) = z - smallestLinearIndex l -- (2 ^ popCount l - 1)
  {-# INLINE linearIndex #-}
  smallestLinearIndex (BitSet l) = 2 ^ popCount l - 1
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (BitSet h) = 2 ^ popCount h - 1
  {-# INLINE largestLinearIndex #-}
  size (BitSet l) (BitSet h) = 2 ^ popCount h - 2 ^ popCount l + 1
  {-# INLINE size #-}

instance Index (Interface i) where
  linearIndex l _ (Interface z) = z - smallestLinearIndex l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (Interface l) = l
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex (Interface h) = h
  {-# INLINE largestLinearIndex #-}
  size (Interface l) (Interface h) = h - l + 1
  {-# INLINE size #-}

instance IndexStream z => IndexStream (z:.BitSet) where
  streamUp   (ls:.l) (hs:.h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = let k = popCount l in return (z,k,Just $ 2^k-1)
          step (z,k,Nothing)
            | k > c     = return $ SM.Done
            | otherwise = return $ SM.Skip (z,k+1,Just $ 2^(k+1)-1)
          step (z,k,Just (!s))
            | otherwise = return $ SM.Yield (z:.s) (z,k,succPopulation c s)
          !c   = popCount h
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown = undefined

testBitset :: BitSet -> BitSet -> IO Int
testBitset l h = SM.foldl' (+) 0 $ SM.map (\(Z:.BitSet z) -> z) $ streamUp (Z:.l) (Z:.h)
{-# NOINLINE testBitset #-}

instance IndexStream z => IndexStream (z:.(BitSet:.Interface i)) where
  streamUp   = undefined
  streamDown = undefined

instance IndexStream z => IndexStream (z:.(BitSet:.Interface i:.Interface j)) where
  streamUp   = undefined
  streamDown = undefined


{-
instance Index z => Index (z:.(BitSet:.Interface i)) where
  linearIndex = undefined -- (ls:.l) (hs:.h) (zs:.(BitSet z:>i)) = linearIndex ls hs zs * (h+1) + undefined - l
  smallestLinearIndex = undefined
  largestLinearIndex = undefined
-}

{-
instance Index (BitSet:.Interface i) where
  linearIndex ls hs is = linearIndex (Z:.ls) (Z:.hs) (Z:.is)
  smallestLinearIndex 

instance Index z => Index (z:.(BitSet:>Interface i:>Interface j)) where
  linearIndex = undefined
  smallestLinearIndex = undefined
  largestLinearIndex = undefined

instance Index (BitSet:>Interface i:>Interface j)
-}

{-
instance Index z => Index (z:.Interface i) where
  linearIndex (ls:.l) (hs:.h) (zs:.Interface i) = linearIndex ls hs zs * (h+1) + i - l
  {-# INLINE linearIndex #-}
  smallestLinearIndex (z:.Interface i) = undefined
  {-# INLINE smallestLinearIndex #-}
  largestLinearIndex = undefined
  {-# INLINE largestLinearIndex #-}
  {-
  streamUp (ls:.Interface l) (hs:.Interface h) = SM.flatten mk step Unknown $ streamUp ls hs
    where mk z = return (z,l)
          step (z,k)
            | k > h     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.Interface k) (z,k+1)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamUp #-}
  streamDown (ls:.Interface l) (hs:.Interface h) = SM.flatten mk step Unknown $ streamDown ls hs
    where mk z = return (z,h)
          step (z,k)
            | k < l     = return $ SM.Done
            | otherwise = return $ SM.Yield (z:.Interface k) (z,k-1)
          {-# INLINE [0] mk   #-}
          {-# INLINE [0] step #-}
  {-# INLINE streamDown #-}
  -}
-}

