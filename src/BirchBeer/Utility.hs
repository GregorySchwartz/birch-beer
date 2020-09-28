{- BirchBeer.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module BirchBeer.Utility
    ( getMostFrequent
    , getFractions
    , isTo
    , branchToLeaf
    , branchToLeafDend
    , clusteringTreeToTree
    , lengthElementsDend
    , lengthElementsTree
    , isLeaf
    , absLog2
    , dendToTree
    , dendrogramToGraph
    , treeToGraph
    , clusterGraphToTree
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
    , closestColor
    , getNodeAssignments
    , printNodeAssignments
    , smartValue
    ) where

-- Remote
import Control.Monad (join)
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Data.Function (on)
import Data.Int (Int32)
import Data.List (genericLength, maximumBy, minimumBy, foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Tree (Tree (..), flatten)
import Safe (atMay, headMay)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Csv as CSV
import qualified Math.Clustering.Hierarchical.Spectral.Types as HSC
import qualified Data.ByteString.Lazy.Char8 as B
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
branchToLeaf b@(Node { subForest = [] }) = b
branchToLeaf b@(Node { subForest = xs }) =
  b { rootLabel = mconcat . concatMap flatten $ xs, subForest = [] }

-- | Check if a Tree is a leaf.
isLeaf :: Tree a -> Bool
isLeaf (Node { subForest = [] }) = True
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
lengthElementsTree :: (Monoid (t a), Traversable t, Num b)
                   => Tree (TreeNode (t a)) -> b
lengthElementsTree =
  fromIntegral . sum . catMaybes . fmap (fmap length . L.view item) . flatten

-- | Convert a dendrogram to a tree.
dendToTree :: HC.Dendrogram a -> Tree (TreeNode a)
dendToTree (HC.Leaf x) =
  Node { rootLabel = TreeNode { _distance = Nothing
                              , _item = Just x
                              , _significance = Nothing
                              }
       , subForest = []
       }
dendToTree (HC.Branch d l r)  =
  Node { rootLabel = TreeNode { _distance = Just d
                              , _item = Nothing
                              , _significance = Nothing
                              }
       , subForest = [dendToTree l, dendToTree r]
       }

-- | Convert a ClusteringTree to a tree.
clusteringTreeToTree :: HSC.ClusteringTree a -> Tree (TreeNode (V.Vector a))
clusteringTreeToTree (Node { rootLabel = n, subForest = []}) =
  Node { rootLabel = TreeNode { _distance = Nothing
                              , _item = Just . HSC._clusteringItems $ n
                              , _significance = Nothing
                              }
       , subForest = []
       }
clusteringTreeToTree (Node { rootLabel = n, subForest = xs }) =
  Node { rootLabel = TreeNode { _distance = Just . HSC.unQ . HSC._ngMod $ n
                              , _item = Nothing
                              , _significance = HSC._pVal n
                              }
       , subForest = fmap clusteringTreeToTree xs
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
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq a)) ClusterEdge) Int
    go (HC.Branch d l r) = do
        (n, gr) <- get
        modify (L.over L._1 (+ 1))

        l <- go l
        r <- go r

        let setGr = G.insEdge (n, r, ClusterEdge (Just d) Nothing)
                  . G.insEdge (n, l, ClusterEdge (Just d) Nothing)
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
treeToGraph :: (TreeItem a) => Tree (TreeNode (V.Vector a)) -> ClusterGraph a
treeToGraph =
    ClusterGraph
        . snd
        . flip execState (0, G.empty)
        . go
  where
    go :: (TreeItem a)
       => Tree (TreeNode (V.Vector a))
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq a)) ClusterEdge) Int
    go (Node { rootLabel = TreeNode { _item = items }, subForest = [] }) = do
        (n, gr) <- get

        modify ( L.over L._1 (+ 1)
               . L.over L._2 ( G.insNode ( n
                                         , (n, Seq.fromList . V.toList <$> items)
                                         )
                             )
               )

        return n
    go (Node { rootLabel = TreeNode { _distance = d, _item = items, _significance = s }, subForest = children }) = do
        (n, gr) <- get
        modify (L.over L._1 (+ 1))

        children' <- mapM go children

        let setGr a = foldl'
                        (\acc x -> G.insEdge (n, x, ClusterEdge d s) acc)
                        (G.insNode (n, (n, Nothing)) a)
                        children'

        modify (L.over L._2 setGr)

        return n

-- | Convert a ClusterGraph to a tree, feed in the starting node. Must not
-- include cycles!
clusterGraphToTree
    :: (TreeItem a) => ClusterGraph a -> G.Node -> Tree (TreeNode (V.Vector a))
clusterGraphToTree (ClusterGraph gr) n | null $ G.suc gr n =
  Node { rootLabel =
          TreeNode { _distance = Nothing
                   , _item = fmap (V.fromList . F.toList)
                   . join
                   . fmap snd
                   . G.lab gr
                   $ n
                   , _significance = Nothing
                   }
       , subForest = []
       }
clusterGraphToTree (ClusterGraph gr) n | otherwise =
  Node { rootLabel =
          TreeNode { _distance = join
                               . fmap (L.view (L._3 . edgeDistance))
                               . headMay
                               . G.out gr
                               $ n
                   , _item = Nothing
                   , _significance = join
                                   . fmap (L.view (L._3 . edgeSignificance))
                                   . headMay
                                   . G.out gr
                                   $ n
                   }
       , subForest = fmap (clusterGraphToTree (ClusterGraph gr)) . G.suc gr $ n
       }

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
median p = S.median p

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

-- | Find the closest color from a list.
closestColor :: [D.Colour Double] -> D.Colour Double -> D.Colour Double
closestColor cs c = minimumBy (compare `on` colorDist c) cs

-- | Get the distance between two colors.
colorDist :: D.Colour Double -> D.Colour Double -> Double
colorDist c1 = euclideanDist (colorToList c1) . colorToList

-- | Convert color values to a list.
colorToList :: D.Colour Double -> [Double]
colorToList (D.toSRGB -> D.RGB r g b) =  [r, g, b]

-- | Euclidean distance between two lists.
euclideanDist :: [Double] -> [Double] -> Double
euclideanDist xs = sqrt . sum . zipWith (\x y -> (x - y) ** 2) xs

-- | Get a list of clusters for each item.
getNodeAssignments :: (TreeItem a) => ClusterGraph a -> [(a, [Cluster])]
getNodeAssignments =
  concatMap (\ (!ns, (_, !xs))
            -> zip (maybe [] F.toList xs) . repeat . fmap Cluster $ ns
            )
      . F.toList
      . flip getGraphLeavesWithParents 0
      . unClusterGraph

-- | Print the node assignments.
printNodeAssignments :: (TreeItem a) => [(a, [Cluster])] -> B.ByteString
printNodeAssignments cr = header <> "\n" <> body
  where
    header = "item,cluster,path"
    body = CSV.encode
         . fmap (\ (!ci, !(c:cs))
                -> ( unId . getId $ ci
                   , showt $ unCluster c
                   , T.intercalate "/" . fmap (showt . unCluster) $ c:cs
                   )
                 )
         $ cr

-- | Get a new value based on a collection of values and the desired number of
-- MADs from the median.
smartValue :: Double -> V.Vector Double -> Double
smartValue n xs = median S.s xs + (n * mad S.s xs)
