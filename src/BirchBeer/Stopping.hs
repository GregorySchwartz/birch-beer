{- BirchBeer.Stopping
Gregory W. Schwartz

Collects helper functions used in the stopping criteria.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module BirchBeer.Stopping
    ( stepCutDendrogram
    , sizeCutDendrogram
    , proportionCutDendrogram
    , distanceCutDendrogram
    , getSize
    , getDistance
    , getProportion
    , smartCut
    ) where

-- Remote
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Data.Function (on)
import Data.Int (Int32)
import Data.List (genericLength, maximumBy)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Statistics.Quantile as S

-- Local
import BirchBeer.Types
import BirchBeer.Utility

-- | Cut a dendrogram based off of the number of steps from the root, combining
-- the results.
stepCutDendrogram :: (Monoid a) => Int -> HC.Dendrogram a -> HC.Dendrogram a
stepCutDendrogram _ b@(HC.Leaf x)       = branchToLeaf b
stepCutDendrogram 0 b@(HC.Branch d l r) = branchToLeaf b
stepCutDendrogram !n (HC.Branch d l r) =
    HC.Branch d (stepCutDendrogram (n - 1) l) (stepCutDendrogram (n - 1) r)

-- | Cut a dendrogram based off of the minimum size of a leaf.
sizeCutDendrogram
    :: (Monoid (t a), Traversable t)
    => Int -> HC.Dendrogram (t a) -> HC.Dendrogram (t a)
sizeCutDendrogram _ b@(HC.Leaf x) = branchToLeaf b
sizeCutDendrogram n b@(HC.Branch d l@(HC.Leaf ls) r@(HC.Leaf rs)) =
    if length ls < n || length rs < n
        then branchToLeaf b
        else HC.Branch d (sizeCutDendrogram n l) (sizeCutDendrogram n r)
sizeCutDendrogram n b@(HC.Branch d l@(HC.Leaf ls) r) =
    if length ls < n
        then branchToLeaf b
        else HC.Branch d l (sizeCutDendrogram n r)
sizeCutDendrogram n b@(HC.Branch d l r@(HC.Leaf rs)) =
    if length rs < n
        then branchToLeaf b
        else HC.Branch d (sizeCutDendrogram n l) (HC.Leaf rs)
sizeCutDendrogram n b@(HC.Branch d l r) =
    if lengthElements l + lengthElements r < n
        then branchToLeaf b
        else HC.Branch d (sizeCutDendrogram n l) (sizeCutDendrogram n r)

-- | Cut a dendrogram based off of the proportion size of a leaf. Will absolute
-- log2 transform before comparing, so if the cutoff size is 0.5 or 2 (twice as
-- big or half as big), the result is the same. Stops when the node proportion
-- is larger than the input, although leaving the children as well.
proportionCutDendrogram
    :: (Monoid (t a), Traversable t)
    => Double -> HC.Dendrogram (t a) -> HC.Dendrogram (t a)
proportionCutDendrogram _ (HC.Leaf x) = HC.Leaf x
proportionCutDendrogram _ b@(HC.Branch _ (HC.Leaf _) (HC.Leaf _)) = b
proportionCutDendrogram n b@(HC.Branch d l@(HC.Leaf ls) r) =
    if (absLog2 $ (lengthElements l) / (lengthElements r))
       > absLog2 n
        then HC.Branch d l (branchToLeaf r)
        else HC.Branch d l (proportionCutDendrogram n r)
proportionCutDendrogram n b@(HC.Branch d l r@(HC.Leaf rs)) =
    if (absLog2 $ (lengthElements l) / (lengthElements r))
       > absLog2 n
        then HC.Branch d (branchToLeaf l) r
        else HC.Branch d (proportionCutDendrogram n l) r
proportionCutDendrogram n b@(HC.Branch d l r) =
    if (absLog2 $ (lengthElements l) / (lengthElements r)) > absLog2 n
        then HC.Branch d (branchToLeaf l) (branchToLeaf r)
        else HC.Branch
                d
                (proportionCutDendrogram n l)
                (proportionCutDendrogram n r)

-- | Cut a dendrogram based off of the distance, keeping up to and including the
-- children of the stopping vertex. Stop is distance is less than the input
-- distance.
distanceCutDendrogram
    :: (Monoid (t a), Traversable t)
    => Double -> HC.Dendrogram (t a) -> HC.Dendrogram (t a)
distanceCutDendrogram _ b@(HC.Leaf _) = branchToLeaf b
distanceCutDendrogram _ b@(HC.Branch _ (HC.Leaf _) (HC.Leaf _)) = b
distanceCutDendrogram d (HC.Branch d' l@(HC.Leaf _) r) =
    if d' < d
        then HC.Branch d' l $ branchToLeaf r
        else HC.Branch d' l (distanceCutDendrogram d r)
distanceCutDendrogram d (HC.Branch d' l r@(HC.Leaf rs)) =
    if d' < d
        then HC.Branch d' (branchToLeaf l) r
        else HC.Branch d' (distanceCutDendrogram d l) r
distanceCutDendrogram d (HC.Branch d' l r) =
    if d' < d
        then HC.Branch d' (branchToLeaf l) (branchToLeaf r)
        else
            HC.Branch d' (distanceCutDendrogram d l) (distanceCutDendrogram d r)

-- | Get a property about each node in a dendrogram.
getNodeInfo :: (HC.Dendrogram a -> b) -> HC.Dendrogram a -> [b]
getNodeInfo f b@(HC.Leaf _)       = [f b]
getNodeInfo f b@(HC.Branch d l r) = f b : (getNodeInfo f l <> getNodeInfo f r)

getSize :: (Monoid (t a), Traversable t, Num b) => HC.Dendrogram (t a) -> b
getSize = lengthElements

getDistance :: HC.Dendrogram a -> Maybe Double
getDistance (HC.Leaf _) = Nothing
getDistance (HC.Branch d _ _) = Just d

getProportion
    :: (Monoid (t a), Traversable t)
    => HC.Dendrogram (t a) -> Maybe Double
getProportion (HC.Leaf _) = Nothing
getProportion (HC.Branch _ l r) =
    Just . absLog2 $ fromIntegral (getSize l) / fromIntegral (getSize r)

smartCut
    :: (Monoid (t a), Traversable t)
    => Double
    -> (HC.Dendrogram (t a) -> Maybe Double)
    -> HC.Dendrogram (t a)
    -> Double
smartCut n f dend = median S.s xs + (n * mad S.s xs)
  where
    xs = V.fromList . catMaybes . getNodeInfo f $ dend
