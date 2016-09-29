
-- | Test all properties automatically. We keep the QC2 modules in the main
-- library for now, as this allows for more efficient repl tests.

module Main where

import Test.Tasty

import QuickCheck.Subword (testgroup_subword)



main :: IO ()
main = defaultMain testgroup_subword

