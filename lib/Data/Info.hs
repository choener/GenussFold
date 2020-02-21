
-- | Similar to @Show@, this module provides a string via @info@. This string gives convenient
-- user-information on objects.

module Data.Info where



class Info c where
  -- | The string returned by 'info' should be around 60 chars per line, and one line if possible.
  info :: c -> String

