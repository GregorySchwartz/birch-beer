{- BirchBeer.MainDiagram
Gregory W. Schwartz

Main function for generating the diagram, processing the preliminary arguments.
-}

{-# LANGUAGE OverloadedStrings #-}

module BirchBeer.MainDiagram
    ( mainDiagram
    ) where

-- Remote
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D

-- Local
import BirchBeer.ColorMap
import BirchBeer.LeafGraph
import BirchBeer.Load
import BirchBeer.Plot
import BirchBeer.Stopping
import BirchBeer.Types
import BirchBeer.Utility

-- | Main function for generating the diagram and all needed arguments.
mainDiagram
    :: (Eq a, Ord a, TreeItem a, MatrixLike b)
    => Config a b
    -> IO (D.Diagram D.B, Maybe LabelColorMap, Maybe ItemColorMap, Maybe MarkColorMap, HC.Dendrogram (V.Vector a), ClusterGraph a)
mainDiagram config = do
    let dend              = _birchDend config
        labelMap'         = _birchLabelMap config
        smartCutoff'      = _birchSmartCutoff config
        maxStep'          = _birchMaxStep config
        minSize'          =
            case (fmap unSmartCutoff smartCutoff', _birchMinSize config) of
                (Just x, Just _)   -> Just
                                    . MinClusterSize
                                    . round
                                    . (2 **)
                                    . smartCut x (Just . logBase 2 . getSize)
                                    $ dend
                otherwise          -> _birchMinSize config
        maxProportion'    =
            case (fmap unSmartCutoff smartCutoff', _birchMaxProportion config) of
                (Just x, Just _)   -> Just
                                    . MaxProportion
                                    . (2 **)
                                    $ smartCut x getProportion dend
                otherwise          -> _birchMaxProportion config
        minDistance'      =
            case (fmap unSmartCutoff smartCutoff', _birchMinDistance config) of
                (Just x, Just _)   ->
                    Just . MinDistance $ smartCut x getDistance dend
                otherwise          -> _birchMinDistance config
        drawLeaf'         = _birchDrawLeaf config
        drawCollection'   = _birchDrawCollection config
        drawMark'         = _birchDrawMark config
        drawNodeNumber'   = _birchDrawNodeNumber config
        drawMaxNodeSize'  = _birchDrawMaxNodeSize config
        drawNoScaleNodes' = _birchDrawNoScaleNodes config
        drawColors'       = _birchDrawColors config
        order'            = fromMaybe (Order 1) $ _birchOrder config
        mat               = return $ _birchMat config
        simMat            = _birchSimMat config

        -- Prune dendrogram.
        dend' = foldl' (\acc f -> f acc) dend
              $ [ (\x -> maybe x (flip proportionCutDendrogram x . unMaxProportion) maxProportion')
                , (\x -> maybe x (flip distanceCutDendrogram x . unMinDistance) minDistance')
                , (\x -> maybe x (flip sizeCutDendrogram x . unMinClusterSize) minSize')
                , (\x -> maybe x (flip stepCutDendrogram x . unMaxStep) maxStep')
                ]
        -- Load graph.
        gr    = dendrogramToGraph dend'

        -- Load draw configurations.
        drawConfig        = DrawConfig
                                drawLeaf'
                                drawCollection'
                                drawNodeNumber'
                                drawMaxNodeSize'
                                drawNoScaleNodes'

    -- Get the color of each label.
    let labelColorMap =
            case drawColors' of
                Nothing   -> fmap (getLabelColorMap Set1) labelMap'
                (Just cs) -> fmap (getLabelCustomColorMap cs) labelMap'
        -- | Get the mark color map.
        markColorMap = case drawMark' of
                        MarkModularity -> Just $ getMarkColorMap gr
                        _ -> Nothing
        defaultGetItemColorMap :: Maybe ItemColorMap
        defaultGetItemColorMap = do
            lcm <- labelColorMap
            lm  <- labelMap'
            return $ labelToItemColorMap lcm lm

    -- | Get the item color map.
    itemColorMap <-
        case drawLeaf' of
            DrawItem (DrawContinuous x) ->
                fmap (fmap (getItemColorMapContinuous (Feature x))) mat
            DrawItem DrawSumContinuous  ->
                fmap (fmap getItemColorMapSumContinuous) mat
            _                           -> return defaultGetItemColorMap

    -- | Get the node color map.
    let nodeColorMap =
            case drawLeaf' of
                (DrawItem DrawDiversity) ->
                    fmap (getNodeColorMapFromDiversity order' gr) itemColorMap
                _ -> fmap (getNodeColorMapFromItems gr) $ itemColorMap
    -- | Get the graph at each leaf (if applicable).
        leafGraphMap = fmap (clusterGraphToLeafGraphMap gr) simMat

    -- | Get the legend of the diagram.
    legend <- case drawLeaf' of
                (DrawItem (DrawContinuous x)) ->
                    fmap
                        (fmap (plotContinuousLegend (Feature x)))
                        mat
                (DrawItem DrawSumContinuous) ->
                    fmap (fmap plotSumContinuousLegend) mat
                _ -> return $ fmap plotLabelLegend labelColorMap

    -- | Get the entire diagram.
    plot <- plotGraph
                legend
                drawConfig
                itemColorMap
                nodeColorMap
                markColorMap
                leafGraphMap
                gr

    return (plot, labelColorMap, itemColorMap, markColorMap, dend', gr)
