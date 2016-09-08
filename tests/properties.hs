
module Main where

import qualified Data.Tree as T
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Data.Forest.Static

-- Given trees, create a pre-order forest and then generate the trees from
-- the pre-order forest again.

prop_bla :: QCTree Int -> Bool
prop_bla = undefined

-- Same, but with post-order.

main :: IO ()
main = $(defaultMainGenerator)

