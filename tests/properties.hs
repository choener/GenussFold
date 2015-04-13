
module Main where

import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH

import qualified Data.Bits.Ordered.QuickCheck as QC



prop_PopCountSet = QC.prop_PopCountSet
prop_lsb_Int = QC.prop_lsb_Int
prop_lsb_Word = QC.prop_lsb_Word
prop_OneBits_Int = QC.prop_OneBits_Int
prop_allPermutations = QC.prop_allPermutations

main :: IO ()
main = $(defaultMainGenerator)

