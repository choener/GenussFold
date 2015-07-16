
-- | Test all properties automatically. We keep the QC2 modules in the main
-- library for now, as this allows for more efficient repl tests.

module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toUpper)

import BioInf.GenussFold.PKN (pknPairMax)



foreign import ccall pseudoknot :: Int -> CString -> IO Int

c_pseudoknot :: String -> Int
c_pseudoknot s' = unsafePerformIO $ do
  let s = map toUpper s'
  cs <- newCAString s
  pseudoknot (length s) cs



prop_GenussFold_HS_C :: String -> Bool
prop_GenussFold_HS_C i = (fst $ pknPairMax 1 i) == (c_pseudoknot i)


main :: IO ()
main = $(defaultMainGenerator)

