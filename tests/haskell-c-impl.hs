
-- | Test all properties automatically. We keep the QC2 modules in the main
-- library for now, as this allows for more efficient repl tests.

module Main where

import Data.Char (toUpper)
import Debug.Trace
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck
import Control.Monad (replicateM)

import BioInf.GenussFold.PKN (pknPairMax)



foreign import ccall pseudoknot :: Int -> CWString -> IO Int

c_pseudoknot :: String -> Int
c_pseudoknot s' = unsafePerformIO $ do
  let s = map toUpper s'
  withCWString s $ \cw ->
    pseudoknot (length s) cw



prop_GenussFold_HS_C :: Nts -> Bool
prop_GenussFold_HS_C (Nts i) = (fst $ pknPairMax 1 i) == (c_pseudoknot i)



newtype Nts = Nts [Char]
  deriving (Eq,Ord)

instance Show Nts where
  show (Nts xs) = show xs

instance Arbitrary Nts where
  arbitrary = do k <- choose (1,50)
                 fmap Nts $ replicateM k (elements "ACGU")
  shrink (Nts xs) = map Nts $ shrinkList (const []) xs

main :: IO ()
main = $(defaultMainGenerator)

