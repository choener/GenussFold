{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generalized fusion system for grammars.
--
-- NOTE Symbols typically do not check bound data for consistency. If you, say,
-- bind a terminal symbol to an input of length 0 and then run your grammar,
-- you probably get errors, garbled data or random crashes. Such checks are
-- done via asserts in non-production code.
--
-- TODO each combinator should come with a special outer check. Given some
-- index (say (i,j), this can then check if i-const >= 0, or j+const<=n, or
-- i+const<=j. That should speed up everything that uses GChr combinators.
-- Separating out this check means that certain inner loops can run without any
-- conditions and just jump.

module ADP.Fusion
  -- basic combinators
  ( (<<<)
  , (<<#)
  , (|||)
  , (...)
  , (~~)
  , (%)
  -- filters
  , check
  -- parsers
  , chr
  , chrLeft
  , chrRight
  , empty
  , region
  , sregion
  , Tbl (..)
  , BtTbl (..)
  , MTbl (..)
  , ENE (..)
  ) where

import Data.Strict.Tuple
import GHC.Exts (inline)
import qualified Data.Vector.Fusion.Stream.Monadic as S

import ADP.Fusion.Apply
import ADP.Fusion.Chr
import ADP.Fusion.Classes
import ADP.Fusion.Empty
import ADP.Fusion.Region
import ADP.Fusion.Table



-- | Apply a function to symbols on the RHS of a production rule. Builds the
-- stack of symbols from 'xs' using 'build', then hands this stack to
-- 'mkStream' together with the initial 'iniT' telling 'mkStream' that we are
-- in the "outer" position. Once the stream has been created, we 'S.map'
-- 'getArg' to get just the arguments in the stack, and finally 'apply' the
-- function 'f'.

infixl 8 <<<
(<<<) f xs = \ij -> outerCheck (checkValidIndex (build xs) ij) . S.map (apply (inline f) . getArg) . mkStream (build xs) Outer $ ij
{-# INLINE (<<<) #-}

infixl 8 <<#
(<<#) f xs = \ij -> outerCheck (checkValidIndex (build xs) ij) . S.mapM (apply (inline f) . getArg) . mkStream (build xs) Outer $ ij
{-# INLINE (<<#) #-}

-- | Combine two RHSs to give a choice between parses.

infixl 7 |||
(|||) xs ys = \ij -> xs ij S.++ ys ij
{-# INLINE (|||) #-}

-- | Applies the objective function 'h' to a stream 's'. The objective function
-- reduces the stream to a single optimal value (or some vector of co-optimal
-- things).

infixl 5 ...
(...) s h = h . s
{-# INLINE (...) #-}

-- | Additional outer check with user-given check function

infixl 6 `check`
check xs f = \ij -> let chk = f ij in chk `seq` outerCheck chk (xs ij)
{-# INLINE check #-}

-- | Separator between RHS symbols.

infixl 9 ~~
(~~) = (:!:)
{-# INLINE (~~) #-}

-- | This separator looks much paper "on paper" and is not widely used otherwise.

infixl 9 %
(%) = (:!:)
{-# INLINE (%) #-}








