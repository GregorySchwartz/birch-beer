{- BirchBeer.MainDiagram
Gregory W. Schwartz

Main function for generating the diagram, processing the preliminary arguments.
-}

{-# LANGUAGE OverloadedStrings #-}

module BirchBeer.MainDiagram
    ( mainDiagram
    ) where

-- Remote
import Data.Bool (bool)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import Data.Tree (Tree (..))
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Foldable as F
import qualified Data.Set as Set
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
    -> IO (D.Diagram D.B, Maybe LabelColorMap, Maybe ItemColorMap, Maybe MarkColorMap, Tree (TreeNode (V.Vector a)), ClusterGraph a)
mainDiagram config = do
    let tree              = _birchTree config
        labelMap'         = _birchLabelMap config
        smartCutoff'      = _birchSmartCutoff config
        maxStep'          = _birchMaxStep config
        minSize'          =
            case (fmap unSmartCutoff smartCutoff', _birchMinSize config) of
                (Just x, Just _)   ->
                  Just
                    . MinClusterSize
                    . round
                    . (2 **)
                    . smartCut x (Just . logBase 2 . getSize)
                    $ tree
                otherwise          -> _birchMinSize config
        maxProportion'    =
            case (fmap unSmartCutoff smartCutoff', _birchMaxProportion config) of
                (Just x, Just _)   -> Just
                                    . MaxProportion
                                    . (2 **)
                                    $ smartCut x getProportion tree
                otherwise          -> _birchMaxProportion config
        minDistance'      =
            case (fmap unSmartCutoff smartCutoff', _birchMinDistance config) of
                (Just x, Just _)   ->
                    Just . MinDistance $ smartCut x getDistance tree
                otherwise          -> _birchMinDistance config
        drawLeaf'         = _birchDrawLeaf config
        drawCollection'   = _birchDrawCollection config
        drawMark'         = _birchDrawMark config
        drawNodeNumber'   = _birchDrawNodeNumber config
        drawMaxNodeSize'  = _birchDrawMaxNodeSize config
        drawMaxLeafNodeSize' = _birchDrawMaxLeafNodeSize config
        drawNoScaleNodes' = _birchDrawNoScaleNodes config
        drawLegendAllLabels' = _birchDrawLegendAllLabels config
        drawLegendSep'    = _birchDrawLegendSep config
        drawPalette'      = _birchDrawPalette config
        drawColors'       = _birchDrawColors config
        drawScaleSaturation' = _birchDrawScaleSaturation config
        order'            = fromMaybe (Order 1) $ _birchOrder config
        mat               = return $ _birchMat config
        simMat            = _birchSimMat config

        -- Prune tree.
        tree' = foldl' (\acc f -> f acc) tree
              $ [ (\x -> maybe x (flip proportionCut x . unMaxProportion) maxProportion')
                , (\x -> maybe x (flip distanceCut x . unMinDistance) minDistance')
                , (\x -> maybe x (flip sizeCut x . unMinClusterSize) minSize')
                , (\x -> maybe x (flip stepCut x . unMaxStep) maxStep')
                ]
        -- Load graph.
        gr    = treeToGraph tree'

        -- Load draw configurations.
        drawConfig        = DrawConfig
                                drawLeaf'
                                drawCollection'
                                drawNodeNumber'
                                drawMaxNodeSize'
                                drawMaxLeafNodeSize'
                                drawNoScaleNodes'
                                drawLegendSep'

    -- Get the color of each label.
    let labelColorMapRaw =
            case drawColors' of
                Nothing   -> fmap (getLabelColorMap drawPalette') labelMap'
                (Just cs) -> fmap (getLabelCustomColorMap cs) labelMap'
        labelColorMap = fmap
                          (maybe id saturateLabelColorMap drawScaleSaturation')
                          labelColorMapRaw
        -- | Get the mark color map.
        markColorMap = case drawMark' of
                        MarkModularity -> Just $ getMarkColorMap gr
                        _ -> Nothing
        defaultGetItemColorMap :: Maybe ItemColorMap
        defaultGetItemColorMap = do
            lcm <- labelColorMapRaw
            lm  <- labelMap'
            return $ labelToItemColorMap lcm lm

    -- | Get the item color map.
    itemColorMapRaw <-
        case drawLeaf' of
            DrawItem (DrawContinuous x) ->
                fmap
                    (fmap (getItemColorMapContinuous drawColors' (Feature x)))
                    mat
            DrawItem DrawSumContinuous  ->
                fmap (fmap (getItemColorMapSumContinuous drawColors')) mat
            _                           -> return defaultGetItemColorMap

    -- | Get the node color map.
    let itemColorMap = fmap
                        (maybe id saturateItemColorMap drawScaleSaturation')
                        itemColorMapRaw
        nodeColorMap =
          fmap (maybe id saturateNodeColorMap drawScaleSaturation') $
            case drawLeaf' of
                (DrawItem DrawDiversity) ->
                    fmap
                        (getNodeColorMapFromDiversity drawColors' order' gr)
                        itemColorMapRaw
                _ -> fmap (getNodeColorMapFromItems gr) $ itemColorMapRaw
    -- | Get the graph at each leaf (if applicable).
        getAllLeafNodesSet = Set.fromList
                           . F.toList
                           . fmap fst
                           . mconcat
                           . fmap (getGraphLeaves (unClusterGraph gr))
        (maxWeight, edgeThreshold, leafGraphNodes) =
            case drawCollection' of
              (CollectionGraph m e l) ->
                    ( MaxWeight m
                    , EdgeThreshold e
                    , LeafGraphNodes . getAllLeafNodesSet $ l
                    )
              _ -> (MaxWeight 1, EdgeThreshold 0, LeafGraphNodes Set.empty)
        leafGraphMap =
            fmap
                ( clusterGraphToLeafGraphMap
                    maxWeight
                    edgeThreshold
                    leafGraphNodes
                    gr
                )
                simMat

    -- | Get the legend of the diagram.
    legend <- case drawLeaf' of
                (DrawItem (DrawContinuous x)) ->
                    fmap
                        ( fmap ( plotContinuousLegend
                                  drawColors'
                                  (fromMaybe (DrawScaleSaturation 1) drawScaleSaturation')
                                  (Feature x)
                               )
                        )
                        mat
                (DrawItem DrawSumContinuous) ->
                    fmap
                      ( fmap
                          ( plotSumContinuousLegend
                            drawColors'
                            (fromMaybe (DrawScaleSaturation 1) drawScaleSaturation')
                          )
                      )
                      mat
                (DrawItem DrawDiversity) -> return mempty
                _ -> return $ do
                    lm <- labelMap'
                    lcm <- labelColorMap
                    return
                        . plotLabelLegend
                        . bool
                              (subsetLabelColorMap gr lm)
                              id
                              (unDrawLegendAllLabels drawLegendAllLabels')
                        $ lcm

    -- | Get the entire diagram.
    plot <- plotGraph
                legend
                drawConfig
                itemColorMap
                nodeColorMap
                markColorMap
                leafGraphMap
                gr

    return (plot, labelColorMap, itemColorMap, markColorMap, tree', gr)
