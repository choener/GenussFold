
module ADP.Fusion.Term.Epsilon.Subword where

import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic as S
import Prelude hiding (map)

import Data.PrimitiveArray hiding (map)

import ADP.Fusion.Base
import ADP.Fusion.Term.Epsilon.Type

--import Data.Vector.Fusion.Util



instance
  ( Monad m
  , MkStream m ls (Subword I)
  ) => MkStream m (ls :!: Epsilon) (Subword I) where
  mkStream (ls :!: Epsilon) (IStatic ()) hh ij@(Subword (i:.j))
    = staticCheck (i==j)
    $ map (ElmEpsilon (subword i j) (subword 0 0))
    $ mkStream ls (IStatic ()) hh ij
  {-# Inline mkStream #-}

instance
  ( Monad m
  , MkStream m ls (Subword O)
  ) => MkStream m (ls :!: Epsilon) (Subword O) where
  mkStream (ls :!: Epsilon) (OStatic d) u ij@(Subword (i:.j))
    = map (ElmEpsilon (subword i j) (subword i j))
    $ mkStream ls (OStatic d) u ij
  {-# Inline mkStream #-}



instance
  ( Monad m
  , TerminalStream m a is
  ) => TerminalStream m (TermSymbol a Epsilon) (is:.Subword I) where
  terminalStream (a:|Epsilon) (sv:.IStatic _) (is:.ij@(Subword (i:.j)))
    = S.map (\(S6 s (zi:._) (zo:._) is os e) -> S6 s zi zo (is:.subword i j) (os:.subword 0 0) (e:.()))
    . iPackTerminalStream a sv (is:.ij)
  {-# Inline terminalStream #-}

instance TermStaticVar Epsilon (Subword I) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ ij = ij
  {-# Inline termStaticVar #-}
  {-# Inline termStreamIndex #-}


