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
    => EdgeThreshold -> ClusterGraph a -> SimMatrix b -> LeafGraphMap T.Text
clusterGraphToLeafGraphMap edgeThresh (ClusterGraph gr) simMat =
    LeafGraphMap
        . Map.unions
        . fmap (\ (!x, _)
               -> Map.singleton x
                . leafToGraph edgeThresh (ClusterGraph gr) simMat
                $ x
               )
        . F.toList
        . getGraphLeaves gr
        $ 0

-- | Generate a graph from a node in the ClusterGraph. Determines the edge from
-- either the similarity matrix (trivial) or the B2 matrix from a spectral
-- hierarchical clustering.
leafToGraph
    :: (TreeItem a, MatrixLike b)
    => EdgeThreshold
    -> ClusterGraph a
    -> SimMatrix b
    -> G.Node
    -> LeafGraph T.Text
leafToGraph (EdgeThreshold edgeThresh) gr simMat n =
    LeafGraph . G.mkGraph nodes $ edges -- Divide by the maximum weight
  where
    edges :: [G.LEdge Double]
    edges = Fold.fold normByMaxWeight
          . filter (\(i, j, v) -> i /= j && v > edgeThresh)
          $ (\(i, _) (j, _) -> getEdge simMat i j) <$> nodes <*> nodes -- Ignore self edges.
    nodes :: [G.LNode (G.Node, T.Text)]
    nodes = fmap (\ (!n, !l) -> (n, (n, l)))
          . flip zip items
          . fmap (fromMaybe (error "No row in matrix found for item in tree."))
          . Fold.fold getIndices
          . getRowNames
          . getMat
          $ simMat
    normByMaxWeight = (\m xs -> fmap (L.over L._3 (/ fromMaybe 1 m)) xs)
                  <$> Fold.premap (abs . L.view L._3) Fold.maximum
                  <*> Fold.list
    getIndices = sequenceA $ fmap Fold.elemIndex items
    items      = fmap (unId . getId) . F.toList $ getGraphLeafItems gr n
    getMat (SimilarityMatrix x) = x
    getMat (B2Matrix x)         = x

-- | Get an edge where the nodes are rows of a matrix.
getEdge :: (MatrixLike a) => SimMatrix a -> G.Node -> G.Node -> G.LEdge Double
getEdge (SimilarityMatrix !mat) !i !j =
    (i, j, fromMaybe 0 $ S.lookupSM (getMatrix mat) i j)
getEdge (B2Matrix !mat) !i !j         =
    (i, j, getSimilarityFromB2 (B2 $ getMatrix mat) i j)
