
module Data.Combined.Common where



-- | Shall we combine elements on the main diagonal as well?

data OnDiag = OnDiag | NoDiag
  deriving (Eq)

-- | Select only a subset of the possible enumerations.

data Enumerate
  -- | Enumerate all elements
  = All
  -- | Enumerate from a value and at most @N@ elements
  | FromN Int Int

