
module Data.Forest.Static where

import           Data.Graph.Inductive.Basic
import           Data.List (span)
import           Data.Traversable (mapAccumL)
import qualified Data.Map.Strict as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU



-- | Kind of possible @TreeOrder@s.
--
-- TODO @In@ for in-order traversal?

data TreeOrder = Pre | Post



-- | A static forest structure...

data Forest (p :: TreeOrder) v a where
  Forest :: (VG.Vector v a) =>
    { label     :: v a
    , parent    :: VU.Vector Int
    , children  :: V.Vector (VU.Vector Int)
    , lsib      :: VU.Vector Int
    , rsib      :: VU.Vector Int
    , roots     :: VU.Vector Int
    } -> Forest p v a

deriving instance (Show a, Show (v a)) => Show (Forest p v a)



-- |
--
-- TODO write addIndicesF to allow us to remove the non-nice 'error'
-- function / 'undefined'.
--
-- TODO Explain what this function does.

forestWith :: (VG.Vector v a) => (forall a . [T.Tree a] -> [a]) -> [T.Tree a] -> Forest (p::TreeOrder) v a
forestWith f ts
  = Forest { label    = VG.fromList $ f ss
           , parent   = VU.fromList $ map (\(_,k,_,_) -> k) $ f rs
           , children = V.fromList $ map (\(_,_,cs,_) -> VU.fromList cs) $ f rs
           , lsib     = VU.fromList $ map fst $ tail $ S.elems sb
           , rsib     = VU.fromList $ map snd $ tail $ S.elems sb
           , roots    = VU.fromList $ map fst $ T.levels rr !! 1
           }
  where
    ss = ts
    rs = relationsF (-1) $ addIndicesF 0 ss
    rr = addIndices (-1) $ T.Node (error "forestWith :: this should have been legal") ts
    sb = siblings rr


forestPre :: (VG.Vector v a) => [T.Tree a] -> Forest Pre v a
forestPre = forestWith preorderF


forestPost :: (VG.Vector v a) => [T.Tree a] -> Forest Post v a
forestPost = forestWith postorderF


addIndices :: Int -> T.Tree a -> T.Tree (Int,a)
addIndices k = snd . mapAccumL (\i e -> (i+1, (i,e))) k

addIndicesF :: Int -> [T.Tree a] -> [T.Tree (Int,a)]
addIndicesF k = snd . mapAccumL go k
  where go = mapAccumL (\i e -> (i+1, (i,e)))

relationsF :: Int -> [T.Tree (Int,a)] -> [T.Tree (Int,Int,[Int],a)]
relationsF k ts = [ T.Node (i,k,children sf,l) (relationsF i sf)  | T.Node (i,l) sf <- ts ]
  where children sf = map (fst . T.rootLabel) sf

siblings :: T.Tree (Int,a) -> S.Map Int (Int,Int)
{- -- because, well, stupid
siblings = S.fromList . concatMap (go . map fst) . T.levels
  where go xs = zipWith3 (\l x r -> (x,(l,r))) ((-1):xs) xs (tail xs ++ [-1])
        go :: [Int] -> [(Int,(Int,Int))]
-}
siblings = S.fromList . map splt . T.flatten . go ([]::[Int])
  where go sib (T.Node (k,lbl) frst) = let cs = [l | T.Node (l,_) _ <- frst] in T.Node (k,lbl,sib) [ go cs t | t <- frst]
        splt (k,_,sbl) = let (ls,rs) = span (/=k) sbl in (k,(last $ (-1):ls,head $ tail rs ++ [-1]))

