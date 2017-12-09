
-- | This module re-exports the @ADP.Fusion@ module and imports all
-- instances defined for @Subword@s.
--
-- It should, in general, be enough to import this module to be able to
-- write context-free grammars on strings.

module ADP.Fusion.Subword
  ( module ADP.Fusion.Core
  , module ADP.Fusion.Subword.Core
  , module ADP.Fusion.Subword.Term.Chr
  , module ADP.Fusion.Subword.Term.Deletion
  , module ADP.Fusion.Subword.Term.Epsilon
  ) where

import ADP.Fusion.Core
import ADP.Fusion.Subword.Core
import ADP.Fusion.Subword.Term.Epsilon
--import ADP.Fusion.Subword.SynVar.Array.TermSymbol
import ADP.Fusion.Subword.SynVar.Indices
--import ADP.Fusion.Subword.SynVar.Recursive
--import ADP.Fusion.Subword.SynVar.Split
import ADP.Fusion.Subword.Term.Chr
import ADP.Fusion.Subword.Term.Deletion
--import ADP.Fusion.Subword.Term.PeekIndex
--import ADP.Fusion.Subword.Term.Strng

