
-- | This module re-exports the @ADPfusion@ module and imports all
-- instances defined for @Subword@s.
--
-- It should, in general, be enough to import this module to be able to
-- write context-free grammars on strings.

module ADPfusion.Subword
  ( module ADPfusion.Core
  , module ADPfusion.Subword.Core
  , module ADPfusion.Subword.Term.Chr
  , module ADPfusion.Subword.Term.Deletion
  , module ADPfusion.Subword.Term.Epsilon
  , module ADPfusion.Subword.Term.Str
  ) where

import ADPfusion.Core
import ADPfusion.Subword.Core
import ADPfusion.Subword.Term.Epsilon
--import ADPfusion.Subword.SynVar.Array.TermSymbol
import ADPfusion.Subword.SynVar.Indices
--import ADPfusion.Subword.SynVar.Recursive
import ADPfusion.Subword.SynVar.Split
import ADPfusion.Subword.Term.Chr
import ADPfusion.Subword.Term.Deletion
--import ADPfusion.Subword.Term.PeekIndex
import ADPfusion.Subword.Term.Str

