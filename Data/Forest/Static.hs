
module Data.Forest.Static where

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Data.Either (either)
import           Data.Tree (drawForest,flatten)
import qualified Data.Tree as T

import           Biobase.Newick


data TreeOrder = Pre | Post



-- | A static forest structure...

data Forest (p :: TreeOrder) a where
  Forest :: (VG.Vector v a, Show (v a)) =>
    { label     :: v a
    , parent    :: VU.Vector Int
    , children  :: V.Vector (VU.Vector Int)
    , lsib      :: VU.Vector Int
    , rsib      :: VU.Vector Int
    , roots     :: VU.Vector Int
    } -> Forest p a

-- instance Show (Forest p a) where

deriving instance (Show a) => Show (Forest p a)



forestPreFromNewicks :: [NewickTree] -> Forest Pre Info
forestPreFromNewicks ts = error $ show pro
  where err = error . ("\n"++) . drawForest . map (fmap show) . map getNewickTree $ ts
        pro = map (flatten . getNewickTree) $ ts
        numNodes = sum . map length $ pro
        numRoots = length ts
        s = T.Node (Info "SUPER" 0) (map getNewickTree ts)

addPre :: T.Tree x -> T.Tree y
addPre = undefined




test = forestPreFromNewicks $ either error id $ newicksFromText t
  where t = "((raccoon:19.19959,bear:6.80041):0.84600,((sea_lion:11.99700, seal:12.00300):7.52973,((monkey:100.85930,cat:47.14069):20.59201, weasel:18.87953):2.09460):3.87382,dog:25.46154);"
