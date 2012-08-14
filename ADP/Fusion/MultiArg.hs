{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{- LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ADP.Fusion.MultiArg where

import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Size
import "PrimitiveArray" Data.Array.Repa.Index
import "PrimitiveArray" Data.Array.Repa.Shape
import Text.Printf
import GHC.Prim (Constraint(..))
import Debug.Trace


test1 = stack $ b ~~r  ~~ r ~~ b -- ~~ lr ~~ r ~~ lr ~~ b
{-# INLINE test1 #-}
--test2 = mapM_ print . take 500 . S.toList $ mkStream test1 (Z:.0:.50)
test2 = S.length $ mkStream test1 (Z:.0:.50)
{-# NOINLINE test2 #-}

class MkStream x where
  type LeftIdx x :: *
  type NewIdx x :: *
  type StreamConstraint x t :: Constraint
  mkStream :: (t ~ NewIdx x, StreamConstraint x t) => x -> DIM2 -> S.Stream t
  mk       :: (t ~ NewIdx x, StreamConstraint x t) => x -> LeftIdx x -> t
  step     :: (t ~ NewIdx x, StreamConstraint x t) => x -> Int -> t -> S.Step t t

instance MkStream Z where
  type LeftIdx Z = Z
  type NewIdx Z = Z:.Index Z
  type StreamConstraint Z t = (Deep t)
  mkStream Z (Z:.i:.j) = S.singleton (Z:.I i)
  {-# INLINE mkStream #-}

instance (MkStream y, StreamConstraint y (NewIdx y), (NewIdx y) ~ (t0 :. Index t1)) => MkStream (y :. Base) where
  type LeftIdx (y :. Base) = NewIdx y
  type NewIdx (y :. Base) = NewIdx y :. Index Base
  type StreamConstraint (y:.Base) t = (Deep t)
  mkStream yx@(y:._) ij@(Z:.i:.j) = S.flatten (mk yx) (step yx j) Unknown $ mkStream y ij
  step _ j zik@(z:.I i:.I k)
    | k<=j && d<10 = S.Yield (z:.I i:.I k) (z:.I i:. I (k+1))
    | otherwise    = S.Done
    where d = down zik
  mk _ zi@(z:.I i) = (zi:.I i)
  {-# INLINE mk #-}
  {-# INLINE step #-}
  {-# INLINE mkStream #-}

instance (MkStream y, StreamConstraint y (NewIdx y), (NewIdx y) ~ (t0 :. Index t1)) => MkStream (y :. Region) where
  type LeftIdx (y :. Region) = NewIdx y
  type NewIdx (y :. Region) = NewIdx y :. Index Region
  type StreamConstraint (y:.Region) t = ()
  mkStream yx@(y:._) ij@(Z:.i:.j) = S.flatten (mk yx) (step yx j) Unknown $ mkStream y ij
  step _ j zik@(z:.I i:.I k)
    | k<=j      = S.Yield (z:.I i:.I k) (z:.I i:. I (k+1))
    | otherwise = S.Done
  mk _ zi@(z:.I i) = (zi:.I i)
  {-# INLINE mk #-}
  {-# INLINE step #-}
  {-# INLINE mkStream #-}



newtype Index t = I Int
  deriving (Show)

class Deep t where
  get :: t -> Int
  down :: t -> Int

instance Deep (Z:.Index Z) where
  get (z:.I i) = i
  down (z:.I i) = i
  {-# INLINE get #-}
  {-# INLINE down #-}

instance Deep y => Deep (y:.Index Base) where
  get (z:.I i) = i
  down (z:.I i) = down z + (i - get z)
  {-# INLINE get #-}
  {-# INLINE down #-}

instance Deep y => Deep (y:.Index Region) where
  get (z:.I i) = i
  down (z:.I i) = i
  {-# INLINE get #-}
  {-# INLINE down #-}











type Dim1 z = z:.Int
type Dim2 z = Dim1 z :. Int
type Dim3 z = Dim2 z :. Int

infixr ~~
(~~) = (,)
{-# INLINE (~~) #-}



class MkStack x where
  type Stack x :: *
  stack :: x -> Stack x

instance MkStack Base where
  type Stack Base = Z :. Base
  stack b = Z :. b
  {-# INLINE stack #-}

instance MkStack Region where
  type Stack Region = Z :. Region
  stack r = Z :. r
  {-# INLINE stack #-}

instance (MkStack y) => MkStack (x,y) where
  type Stack (x,y) = Stack y :. x
  stack (x,y) = stack y :. x
  {-# INLINE stack #-}

data Base = Base [Char]
  deriving (Show)

b = Base ['a' .. 'z']

data Region = Region [Char]

r = Region ['a'..'z']

data LRegion = LRegion [Char]

lr = LRegion ['a'..'z']

