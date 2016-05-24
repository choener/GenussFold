
-- | A data structure for a static forest.

module Data.Forest.Static where

import           Data.Foldable (toList)
import           Data.Graph.Inductive.Basic
import           Data.List (span,uncons,sort)
import           Data.Traversable (mapAccumL)
import           Debug.Trace
import qualified Data.Map.Strict as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.List as L
import qualified Data.Set as Set



-- | Kind of possible @TreeOrder@s.
--
-- TODO @In@ for in-order traversal?
--
-- TODO @Unordered@ for trees that have no sorted order?

data TreeOrder = Pre | Post



-- | A static forest structure. While traversals are always explicitly
-- possible by following the indices, the nodes themselves shall always be
-- ordered by the type @p :: TreeOrder@. This is not completely enforced,
-- given that @Forest@ is exporting the constructor, but encouraged via
-- construction with helper functions.

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



-- | Construct a static 'Forest' with a tree traversal function. I.e.
-- @forestWith preorderF trees@ will construct a pre-order forest from the
-- list of @trees@.

forestWith :: (VG.Vector v a) => (forall a . [T.Tree a] -> [a]) -> [T.Tree a] -> Forest (p::TreeOrder) v a
forestWith f ts
  = Forest { label    = VG.fromList $ f ts
           , parent   = VU.fromList $ map (\(_,k,_ ,_) -> k             ) $ f pcs
           , children = V.fromList  $ map (\(_,_,cs,_) -> VU.fromList cs) $ f pcs
           , lsib     = VU.fromList $ map fst $ S.elems lr
           , rsib     = VU.fromList $ map snd $ S.elems lr
           , roots    = VU.fromList $ map (fst . T.rootLabel) us
           }
  where
    -- Step 1: construct a forest isomorphic to @ts@ but labelled with
    -- a total order of unique identifiers. (That is: label with @Int@s).
    -- The unique identifiers are in pre-order.
    ps = addIndicesF' 0 ts
    -- Step 2: use @f@ to produce a permutation map and apply this
    -- permutation to turn the pre-order @ps@ into the required order.
    backp = VU.fromList $ map snd $ sort $ zip (f ps) [0..]
    -- Step 3: decorate the forest with indices in the correct order. Keep
    -- the label in @snd@.
    us = map (fmap (\(k,l) -> (backp VG.! k,l))) $ addIndicesF 0 ts
    -- Step 4: add the correct relations (children, lrSibling, parents)
    pcs = parentChildrenF (-1) us
    -- A map with the left and right sibling
    lr  = lrSiblingF us



-- | Construct a pre-ordered forest.

forestPre :: (VG.Vector v a) => [T.Tree a] -> Forest Pre v a
forestPre = forestWith preorderF

-- | Construct a post-ordered forest.

forestPost :: (VG.Vector v a) => [T.Tree a] -> Forest Post v a
forestPost = forestWith postorderF

-- | Add @pre-ordered@ !!! indices. First argument is the starting index.

addIndices :: Int -> T.Tree a -> T.Tree (Int,a)
addIndices k = snd . mapAccumL (\i e -> (i+1, (i,e))) k

-- | Add @pre-ordered@ !!! indices, but to a forest.

addIndicesF :: Int -> [T.Tree a] -> [T.Tree (Int,a)]
addIndicesF k = snd . mapAccumL go k
  where go = mapAccumL (\i e -> (i+1, (i,e)))

-- | Add @pre-ordered@ !!! indices to a forest, but throw the label away as
-- well.

addIndicesF' :: Int -> [T.Tree a] -> [T.Tree Int]
addIndicesF' k = snd . mapAccumL go k
  where go = mapAccumL (\i e -> (i+1, i))

-- | Add parent + children information. Yields
-- @(Index,Parent,[Child],Label)@. Parent is @-1@ if root node.

parentChildrenF :: Int -> [T.Tree (Int,a)] -> [T.Tree (Int,Int,[Int],a)]
parentChildrenF k ts = [ T.Node (i,k,children sf,l) (parentChildrenF i sf)  | T.Node (i,l) sf <- ts ]
  where children sf = map (fst . T.rootLabel) sf

-- | Return a map with all the nearest siblings for each node, for a forest.

lrSiblingF :: [T.Tree (Int,a)] -> S.Map Int (Int,Int)
lrSiblingF = S.delete (-1) . lrSibling . T.Node (-1,error "laziness in lrSiblingF broken")

-- | Return a map with all the nearest siblings for each node, for a tree.

lrSibling :: T.Tree (Int,a) -> S.Map Int (Int,Int)
lrSibling = S.fromList . map splt . T.flatten . go ([]::[Int])
  where go sib (T.Node (k,lbl) frst) = let cs = [l | T.Node (l,_) _ <- frst] in T.Node (k,lbl,sib) [ go cs t | t <- frst]
        splt (k,_,[])  = (k,(-1,-1))
        splt (k,_,sbl) = let (ls,rs) = span (/=k) sbl in (k,(last $ (-1):ls,head $ tail rs ++ [-1]))

-- | Return the left-most leaf for each node.

leftMostLeaves :: Forest p v a -> VU.Vector Int
leftMostLeaves f = VG.map go $ VG.enumFromN 0 $ VG.length $ parent f
  where go k = let cs = children f VG.! k
               in if VG.null cs then k else go (VG.head cs)

-- | Return all left key roots. These are the nodes that have no (super-)
-- parent with the same left-most leaf.
--
-- This function is somewhat specialized for tree editing.
--
-- TODO group by

leftKeyRoots :: Forest Post v a -> VU.Vector Int
leftKeyRoots f = VU.fromList . sort . S.elems $ VU.foldl' go S.empty (VU.enumFromN (0::Int) $ VG.length $ parent f)
        -- Build a map from left-most leaf to most root-near node.
  where go s k = S.insertWith max (lml VU.! k) k s
        lml  = leftMostLeaves f

-- | Returns the list of all sorted subsets of subforests in the forest.
-- The subsets are returned in reversed pre-order.

sortedSubForests :: Forest Pre v a -> [VU.Vector Int]
sortedSubForests f =
  -- cleanup
  map VU.fromList
  . concat
  -- make sure that in our partial order we have smaller forests come
  -- first.
  . map (map unSrt . Set.toList . Set.fromList . map Srt)
  -- get all nonempty ordered subforests
  . map (concatMap (L.tail . L.subsequences))
  . map (L.permutations)
  . map VG.toList . VG.toList
  -- only nodes with children
  . VG.filter (not . VG.null)
  -- every node that has children in reverse order
  -- make sure that the roots are there, but come last
  $ VG.snoc (VG.reverse (children f)) (roots f)

newtype Srt = Srt { unSrt :: [Int] }
  deriving (Eq,Show)

instance Ord Srt where
  Srt xs <= Srt ys = length xs <= length ys

test :: [T.Tree Char]
test = [T.Node 'R' [T.Node 'a' [], T.Node 'b' []], T.Node 'S' [T.Node 'x' [], T.Node 'y' []]]

runtest = do
  print (forestPre test :: Forest Pre V.Vector Char)
  print (forestPost test :: Forest Post V.Vector Char)
  print (forestPost [T.Node 'R' [T.Node 'a' []]] :: Forest Post V.Vector Char)
  print (forestPost [T.Node 'R' [T.Node 'a' [], T.Node 'b' []]] :: Forest Post V.Vector Char)
  print (sortedSubForests (forestPre test :: Forest Pre V.Vector Char))

