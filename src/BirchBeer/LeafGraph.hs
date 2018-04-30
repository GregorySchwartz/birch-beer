{- BirchBeer.LeafGraph
Gregory W. Schwartz

Functions pertaining to the generation of a graph for each leaf in the
hierarchy.
-}

{-# LANGUAGE BangPatterns #-}

module BirchBeer.LeafGraph
    ( leafToGraph
    , clusterGraphToLeafGraphMap
    ) where

-- Remote
import Data.Maybe (fromMaybe)
import Math.Clustering.Spectral.Sparse (getSimilarityFromB2, B2 (..))
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T

-- Local
import BirchBeer.Types
import BirchBeer.Utility

-- | Convert a ClusterGraph to a map of LeafGraphs.
clusterGraphToLeafGraphMap
    :: (TreeItem a, MatrixLike b)
    => ClusterGraph a -> SimMatrix b -> LeafGraphMap T.Text
clusterGraphToLeafGraphMap (ClusterGraph gr) simMat =
    LeafGraphMap
        . Map.fromList
        . zip (G.nodes gr)
        . fmap (leafToGraph (ClusterGraph gr) simMat)
        . G.nodes
        $ gr

-- | Generate a graph from a node in the ClusterGraph. Determines the edge from
-- either the similarity matrix (trivial) or the B2 matrix from a spectral
-- hierarchical clustering.
leafToGraph
    :: (TreeItem a, MatrixLike b)
    => ClusterGraph a -> SimMatrix b -> G.Node -> LeafGraph T.Text
leafToGraph gr simMat n =
    LeafGraph . G.mkGraph nodes . fmap normByMaxWeight $ edges -- Divide by the maximum weight
  where
    edges :: [G.LEdge Double]
    edges = filter (\(i, j, _) -> i /= j)
          $ (\(i, _) (j, _) -> getEdge simMat i j) <$> nodes <*> nodes -- Ignore self edges.
    nodes :: [G.LNode (G.Node, T.Text)]
    nodes = fmap (\ (!n, !l) -> (n, (n, l)))
          . flip zip items
          . fmap (fromMaybe (error "No row found for item."))
          . Fold.fold getIndices
          . getRowNames
          . getMat
          $ simMat
    normByMaxWeight = L.over L._3 (/ maxWeight)
    maxWeight = maximum . fmap (abs . L.view L._3) $ edges
    getIndices = sequenceA $ fmap Fold.elemIndex items
    items      = fmap (unId . getId) . F.toList $ getGraphLeafItems gr n
    getMat (SimilarityMatrix x) = getMatrix x
    getMat (B2Matrix x)         = getMatrix x

-- | Get an edge where the nodes are rows of a matrix.
getEdge :: (MatrixLike a) => SimMatrix a -> G.Node -> G.Node -> G.LEdge Double
getEdge (SimilarityMatrix !mat) !i !j =
    (i, j, fromMaybe 0 $ S.lookupSM (getMatrix mat) i j)
getEdge (B2Matrix !mat) !i !j         =
    (i, j, getSimilarityFromB2 (B2 $ getMatrix mat) i j)
