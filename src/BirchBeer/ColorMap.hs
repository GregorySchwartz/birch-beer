{- BirchBeer.ColorMap
Gregory W. Schwartz

Functions to derive different color maps for items and labels.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module BirchBeer.ColorMap
    ( lchPalette
    , getLabelColorMap
    , getLabelCustomColorMap
    , getLabelMapThresholdContinuous
    , labelToItemColorMap
    , getItemColorMapContinuous
    , getItemColorMapSumContinuous
    , getMarkColorMap
    ) where

-- Remote
import Data.Function (on)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Diagrams.Prelude
import Language.R as R
import Language.R.QQ (r)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.CIE.Illuminant as Colour
import qualified Data.Colour.Palette.BrewerSet as Brewer
import qualified Data.Colour.SRGB as Colour
import qualified Data.Graph.Inductive as G
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import BirchBeer.Types
import BirchBeer.Utility

-- | Convert CIE-LCH(uv) to Luv.
lchToKolor :: L -> C -> H -> Colour.Colour Double
lchToKolor (L l) (C c) (H h) = Colour.cieLAB Colour.d65 l a b
  where
    a = cos (degreeToRadian h) * c
    b = sin (degreeToRadian h) * c

-- | LCH color palette. Equally spaced hues starting from 30.
lchPalette :: Int -> [Colour.Colour Double]
lchPalette n = fmap
                (\h -> lchToKolor (L 65) (C 100) (H h))
                [30, 30 + (360 / fromIntegral (n - 1)) .. fromIntegral 390]

-- | Get the colors of each label using R to interpolate additional colors.
getLabelColorMap :: Palette -> LabelMap -> R.R s LabelColorMap
getLabelColorMap palette (LabelMap lm) = do
    let labels    = Set.toAscList . Set.fromList . Map.elems $ lm
        labelsLen = if odd $ List.genericLength labels
                        then List.genericLength labels :: Int32
                        else List.genericLength labels + 1 :: Int32

    colorsHex <-
        case palette of
            -- From https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
            Hcl  -> [r| hues = seq(15, 375, length = labelsLen_hs + 1)
                        hcl(h = hues, l = 65, c = 100)[1:labelsLen_hs]
                    |]
            Set1 ->
                if labelsLen > 9
                    then
                        [r| library(RColorBrewer)
                            colorRampPalette(brewer.pal(9, "Set1"))(labelsLen_hs)
                        |]
                    else
                        [r| library(RColorBrewer)
                            brewer.pal(labelsLen_hs, "Set1")
                        |]

    let colors = fmap Colour.sRGB24read . R.dynSEXP $ colorsHex

    return
        . LabelColorMap
        . Map.fromList
        . flip zip colors
        $ labels

-- | Get the colors of each label.
getLabelColorMap9 :: LabelMap -> LabelColorMap
getLabelColorMap9 (LabelMap lm) =
    LabelColorMap
        . Map.fromList
        . flip zip (cycle (Brewer.brewerSet Brewer.Set1 9))
        $ labels
  where
    labels = Set.toAscList . Set.fromList . Map.elems $ lm


-- | Get the colors of each label using custom colors.
getLabelCustomColorMap :: CustomColors -> LabelMap -> LabelColorMap
getLabelCustomColorMap (CustomColors cs) (LabelMap lm) =
    LabelColorMap
        . Map.fromList
        . flip zip cs
        . Set.toAscList
        . Set.fromList
        . Map.elems
        $ lm

-- | Get the colors of each item from a label.
labelToItemColorMap :: LabelColorMap -> LabelMap -> ItemColorMap
labelToItemColorMap (LabelColorMap lm) =
    ItemColorMap . Map.map (\x -> Map.findWithDefault black x lm) . unLabelMap

-- | Get the colors from a list of expressions.
getContinuousColor :: [Double] -> [Colour.Colour Double]
getContinuousColor =
    fmap (\x -> Colour.sRGB 1 (1 - x) (1 - x))
        . Fold.fold
            ( (\xs mi ma -> fmap (minMaxNorm (getExist mi) (getExist ma)) xs)
                    <$> Fold.list
                    <*> Fold.minimum
                    <*> Fold.maximum
            )
  where
    getExist = fromMaybe (error "Feature does not exist or no cells found.") 

-- | Get the colors of each item, where the color is determined by expression.
getItemColorMapContinuous :: (MatrixLike a) => Feature -> a -> ItemColorMap
getItemColorMapContinuous g mat =
    ItemColorMap
        . Map.fromList
        . zip (fmap Id . V.toList . getRowNames $ mat)
        . getContinuousColor
        . S.toDenseListSV
        . flip S.extractCol col
        . getMatrix
        $ mat
  where
    col = fromMaybe (error $ "Feature " <> T.unpack (unFeature g) <> " does not exist.")
        . V.elemIndex g
        . fmap Feature
        . getColNames
        $ mat

-- | Get the labels of each item, where the label is determined by a binary high
-- / low expression determined by a threshold. Multiple expressions can be used
-- for combinatorical labeling..
getLabelMapThresholdContinuous
    :: (MatrixLike a)
    => [(Feature, Double)] -> a -> LabelMap
getLabelMapThresholdContinuous gs mat =
    LabelMap
        . Map.fromList
        . zip (fmap Id . V.toList . getRowNames $ mat)
        . getCombinatoricLabels
        $ gs'
  where
    getCombinatoricLabels :: [(Feature, Double)] -> [Label]
    getCombinatoricLabels =
        fmap (Label . List.foldl1' (\acc x -> acc <> " " <> x))
            . List.transpose
            . fmap (uncurry getCombinatoricLabelFeature)
    getCombinatoricLabelFeature g v =
        fmap (\x -> unFeature g <> " " <> if x > v then "high" else "low")
            . S.toDenseListSV
            . flip S.extractCol (getCol g)
            . getMatrix
            $ mat
    gs' = List.sortBy (compare `on` fst) gs
    getCol g = fromMaybe (error $ "Feature " <> T.unpack (unFeature g) <> " does not exist.")
             . V.elemIndex g
             . fmap Feature
             . getColNames
             $ mat

-- | Get the colors of each item, where the color is determined by the sum of
-- features in that cell.
getItemColorMapSumContinuous :: (MatrixLike a) => a -> ItemColorMap
getItemColorMapSumContinuous mat =
    ItemColorMap
        . Map.fromList
        . zip (fmap Id . V.toList . getRowNames $ mat)
        . getContinuousColor
        . fmap sum
        . S.toRowsL
        . getMatrix
        $ mat

-- | Use the outgoing edges of a node to define the mark around the node.
-- Min max normalization.
getMarkColorMap :: ClusterGraph a -> MarkColorMap
getMarkColorMap g =
    MarkColorMap . Map.map (withOpacity black) $ valMap
  where
    valMap   = Map.map (minMaxNorm minVal maxVal) . Map.fromList $ valAssoc
    minVal   = minimum . fmap snd $ valAssoc
    maxVal   = maximum . fmap snd $ valAssoc
    valAssoc = fmap nodeValue . G.labEdges . unClusterGraph $ g
    nodeValue (n1, n2, v) = (n1, v)
