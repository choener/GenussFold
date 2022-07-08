
-- | This module re-exports the @ADPfusion@ module and imports all
-- instances defined for @Subword@s.
--
-- It should, in general, be enough to import this module to be able to
-- write context-free grammars on strings.

module ADPfusion.Subword
  ( module ADPfusion.Core
  , module ADPfusion.Subword.Core
  , module ADPfusion.Subword.SynVar
  , module ADPfusion.Subword.SynVar.Indices
  , module ADPfusion.Subword.SynVar.Split
  , module ADPfusion.Subword.Term.Chr
  , module ADPfusion.Subword.Term.Deletion
  , module ADPfusion.Subword.Term.Epsilon
  , module ADPfusion.Subword.Term.Str
  ) where

import ADPfusion.Core
import ADPfusion.Subword.Core
import ADPfusion.Subword.SynVar.Indices
import ADPfusion.Subword.SynVar.Split
import ADPfusion.Subword.SynVar
import ADPfusion.Subword.Term.Chr
import ADPfusion.Subword.Term.Deletion
import ADPfusion.Subword.Term.Epsilon
import ADPfusion.Subword.Term.Str

