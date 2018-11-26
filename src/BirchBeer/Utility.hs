{- BirchBeer.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module BirchBeer.Utility
    ( getMostFrequent
    , getFractions
    , isTo
    , branchToLeaf
    , branchToLeafDend
    , lengthElementsDend
    , lengthElementsTree
    , isLeaf
    , absLog2
    , dendrogramToGraph
    , getGraphLeaves
    , getGraphLeavesCycles
    , getGraphLeavesWithParents
    , getGraphLeafItems
    , degreeToRadian
    , minMaxNorm
    , mad
    , median
    , subsetLabelColorMap
    , getHighLowColors
    ) where

-- Remote
import Control.Monad (join)
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Data.Function (on)
import Data.Int (Int32)
import Data.List (genericLength, maximumBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Tree (Tree (..), flatten)
import Safe (atMay)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Diagrams.Prelude as D
import qualified Statistics.Quantile as S

-- Local
import BirchBeer.Types

-- | Get the most frequent element of a list.
getMostFrequent :: (Eq a, Ord a) => [a] -> a
getMostFrequent = fst
                . maximumBy (compare `on` snd)
                . Map.toAscList
                . Map.fromListWith (+)
                . flip zip ([1,1..] :: [Double])

-- | Get the fractions of each element in a list.
getFractions :: (Eq a, Ord a) => [a] -> [(a, Double)]
getFractions xs = Map.toAscList . Map.map (/ total) $ countMap
  where
    total    = Map.foldl' (+) 0 countMap
    countMap = Map.fromListWith (+) . flip zip ([1,1..] :: [Double]) $ xs

-- | Basic arithmetic to convert ratios. "x is to y as a is to b" in the order
-- of xIsToYAs x y a.
isTo :: Double -> Double -> Double -> Double
isTo x y a = a / (x / y)

-- | Convert a branch or leaf to a leaf of a dendrogram.
branchToLeafDend :: (Monoid a) => HC.Dendrogram a -> HC.Dendrogram a
branchToLeafDend (HC.Leaf x) = HC.Leaf x
branchToLeafDend (HC.Branch d l r)  =
    HC.Leaf . mconcat $ HC.elements l <> HC.elements r

-- | Convert a branch or leaf to a leaf.
branchToLeaf :: (Monoid a) => Tree a -> Tree a
branchToLeaf b@(Tree { subForest = [] }) = b
branchToLeaf b@(Tree { subForest = xs }) =
  b { subForest = concatMap flatten xs }

-- | Check if a Tree is a leaf.
isLeaf :: Tree a -> Bool
isLeaf (Tree { subForest = [] }) = True
isLeaf _                         = False

-- | Log 2 absolute transformation.
absLog2 :: Double -> Double
absLog2 = abs . logBase 2

-- | Get the generic length of the elements of the dendrogram.
lengthElementsDend
    :: (Monoid (t a), Traversable t, Num b)
    => HC.Dendrogram (t a) -> b
lengthElementsDend = fromIntegral . sum . fmap length . HC.elements

-- | Get the generic length of the elements of the tree.
lengthElementsTree :: (TreeNode a, Num b) => Tree a -> b
lengthElementsTree = fromIntegral . sum . fmap length . flatten
                   
-- | Convert a dendrogram to a tree.
dendToTree :: HC.Dendrogram a -> Tree (TreeNode a)
dendToTree (HC.Leaf x) =
  Tree { rootLabel = TreeNode { _modularity = Nothing, _item = Just x}
       , subForest = []
       }
dendToTree (HC.Branch d l r)  =
  Tree { rootLabel = TreeNode { _modularity = Just d, _item = Nothing }
       , subForest = [dendToTree l, dendToTree r]
       }

-- | Convert a dendrogram with height as Q values to a graph for
-- plotting with leaves containing all information. This also means that the
-- node must be in the label as well.
dendrogramToGraph
    :: (TreeItem a)
    => HC.Dendrogram (V.Vector a)
    -> ClusterGraph a
dendrogramToGraph =
    ClusterGraph
        . snd
        . flip execState (0, G.empty)
        . go
  where
    go :: (TreeItem a)
       => HC.Dendrogram (V.Vector a)
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq a)) HC.Distance) Int
    go (HC.Branch d l r) = do
        (n, gr) <- get
        modify (L.over L._1 (+ 1))

        l <- go l
        r <- go r

        let setGr = G.insEdge (n, r, d)
                  . G.insEdge (n, l, d)
                  . G.insNode (n, (n, Nothing))

        modify (L.over L._2 setGr)

        return n
    go (HC.Leaf items) = do
        (n, gr) <- get

        modify (L.over L._1 (+ 1) . L.over L._2 (G.insNode (n, (n, Just . Seq.fromList . V.toList $ items))))

        return n

-- | Convert a tree with height as Q values to a graph for
-- plotting with leaves containing all information. This also means that the
-- node must be in the label as well.
treeToGraph :: (TreeItem a) => Tree (TreeNode a) -> ClusterGraph a
treeToGraph =
    ClusterGraph
        . snd
        . flip execState (0, G.empty)
        . go
  where
    go :: (TreeItem a)
       => Tree (TreeNode a)
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq a)) Double) Int
    go (Tree { rootLabel = TreeNode { _item = items }, subForest = [] }) = do
        (n, gr) <- get

        modify (L.over L._1 (+ 1) . L.over L._2 (G.insNode (n, (n, Just . Seq.fromList . V.toList $ items))))

        return n
    go (Tree { rootLabel = TreeNode { modularity = d, _item = items }, subForest = children }) = do
        (n, gr) <- get
        modify (L.over L._1 (+ 1))

        children <- fmap go children

        let setGr a = foldl'
                        (\acc x -> G.insEdge (n, x, d) acc)
                        (G.insNode (n, (n, Nothing)) a)
                        children

        modify (L.over L._2 setGr)

        return n

-- | Get leaves of a tree graph given a node. Graph must not include cycles!
getGraphLeaves :: G.Graph gr => gr a b -> G.Node -> Seq.Seq a
getGraphLeaves gr n =
    case G.suc gr n of
        [] -> Seq.singleton
            . fromMaybe (error "Node is missing or has no label (are you using the right tree?).")
            . G.lab gr
            $ n
        xs -> mconcat . fmap (getGraphLeaves gr) $ xs

-- | Get leaves of a tree graph given a node. Allows for cycles.
getGraphLeavesCycles :: G.Graph gr => [G.Node] -> gr a b -> G.Node -> Seq.Seq a
getGraphLeavesCycles prev gr n =
    case noCycle n of
        [] -> Seq.singleton
            . fromMaybe (error "Node has no label.")
            . G.lab gr
            $ n
        xs -> mconcat . fmap (getGraphLeavesCycles (n:xs) gr) $ xs
  where
    noCycle 0 = G.suc gr 0
    noCycle x = filter (not . flip Set.member preSet) $ G.suc gr x
    preSet  = Set.fromList $ n : prev

-- | Get leaves of a tree graph given a node with a breadcrumb trail of parent
-- node IDs. The first element in the cluster list is the node the item belongs
-- to, all the way to the root (last element in the list). Graph must not
-- include cycles!
getGraphLeavesWithParents
    :: G.Graph gr
    => gr a b -> G.Node -> Seq.Seq ([G.Node], a)
getGraphLeavesWithParents gr root = go [] root
  where
    go !acc n =
        case G.suc gr n of
            [] -> Seq.singleton
                . (n:acc,)
                . fromMaybe (error "Node has no label.")
                . G.lab gr
                $ n
            xs -> mconcat . fmap (go (n:acc)) $ xs

-- | Degree to radian.
degreeToRadian :: Double -> Double
degreeToRadian x = x / pi * 180

-- | Min max normalization.
minMaxNorm :: Double -> Double -> Double -> Double
minMaxNorm mi ma v = (v - mi) / (ma - mi)

-- | From statistics github. O(/n/·log /n/). Estimate the median absolute
--   deviation (MAD) of a
--   sample /x/ using 'continuousBy'. It's robust estimate of
--   variability in sample and defined as:
--
--   \[
--   MAD = \operatorname{median}(| X_i - \operatorname{median}(X) |)
--   \]
mad :: S.ContParam  -- ^ Parameters /α/ and /β/.
    -> V.Vector Double   -- ^ /x/, the sample data.
    -> Double
mad p xs = median p $ V.map (abs . subtract med) xs
  where
    med = median p xs

-- | statistics github. O(/n/·log /n/) Estimate median of sample
median :: S.ContParam  -- ^ Parameters /α/ and /β/.
       -> V.Vector Double   -- ^ /x/, the sample data.
       -> Double
median p = S.continuousBy p 1 2

-- | Get the collection of items in a leaf.
getGraphLeafItems :: ClusterGraph a -> G.Node -> Seq.Seq a
getGraphLeafItems (ClusterGraph gr) =
    join . fmap (fromMaybe mempty . snd) . getGraphLeaves gr

-- | Subset the LabelColorMap based on the present items.
subsetLabelColorMap
    :: (TreeItem a)
    => ClusterGraph a -> LabelMap -> LabelColorMap -> LabelColorMap
subsetLabelColorMap gr (LabelMap lm) =
    LabelColorMap
        . Map.filterWithKey (\k _ -> Set.member k validLabels)
        . unLabelColorMap
  where
    validLabels = Set.fromList
                . catMaybes
                . fmap (flip Map.lookup lm . getId)
                . F.toList
                . getGraphLeafItems gr
                $ 0

-- | Get the high and low colors for a continuous color map.
getHighLowColors :: Maybe CustomColors -> (D.Colour Double, D.Colour Double)
getHighLowColors customColors = (highColor, lowColor)
  where
    highColor = fromMaybe D.red $ customColors >>= flip atMay 0 . unCustomColors
    lowColor  = fromMaybe (D.blend 0.2 D.black D.white)
              $ customColors >>= flip atMay 1 . unCustomColors
