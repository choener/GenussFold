
-- | This module re-exports the @ADP.Fusion@ module and imports all
-- instances defined for @Subword@s.
--
-- It should, in general, be enough to import this module to be able to
-- write context-free grammars on strings.

module ADP.Fusion.Subword
  ( module ADP.Fusion.Core
  ) where

import ADP.Fusion.Core
--import ADP.Fusion.Core.Subword
--import ADP.Fusion.SynVar.Array.TermSymbol.Subword
--import ADP.Fusion.SynVar.Indices.Subword
--import ADP.Fusion.SynVar.Recursive.Subword
--import ADP.Fusion.SynVar.Split.Subword
--import ADP.Fusion.Term.Chr.Subword
--import ADP.Fusion.Term.Deletion.Subword
--import ADP.Fusion.Term.Epsilon.Subword
--import ADP.Fusion.Term.PeekIndex.Subword
--import ADP.Fusion.Term.Strng.Subword

