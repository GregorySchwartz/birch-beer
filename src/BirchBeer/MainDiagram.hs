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
import qualified Data.Map.Strict as Map
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
        elbowCutoff'      = _birchElbowCutoff config
        customCut'        = _birchCustomCut config
        rootCut'          = _birchRootCut config
        maxStep'          = _birchMaxStep config
        minSize'          =
            case (fmap unSmartCutoff smartCutoff', fmap unElbowCutoff elbowCutoff', _birchMinSize config) of
                (_, Just x, Just _)   ->
                  Just
                    . MinClusterSize
                    . round
                    . (2 **)
                    . elbowCut x (Just . logBase 2 . getSize)
                    $ tree
                (Just x, Nothing, Just _)   ->
                  Just
                    . MinClusterSize
                    . round
                    . (2 **)
                    . smartCut x (Just . logBase 2 . getSize)
                    $ tree
                otherwise          -> _birchMinSize config
        maxProportion'    =
            case (fmap unSmartCutoff smartCutoff', fmap unElbowCutoff elbowCutoff', _birchMaxProportion config) of
                (_, Just x, Just _)       -> Just
                                           . MaxProportion
                                           . (2 **)
                                           $ elbowCut x getProportion tree
                (Just x, Nothing, Just _) -> Just
                                           . MaxProportion
                                           . (2 **)
                                           $ smartCut x getProportion tree
                otherwise                 -> _birchMaxProportion config
        minDistance'      =
            case (fmap unSmartCutoff smartCutoff', fmap unElbowCutoff elbowCutoff', _birchMinDistance config) of
                (_, Just x, Just _)       ->
                    Just . MinDistance $ elbowCut x getDistance tree
                (Just x, Nothing, Just _) ->
                    Just . MinDistance $ smartCut x getDistance tree
                otherwise                 -> _birchMinDistance config
        minDistanceSearch'      =
            case (fmap unSmartCutoff smartCutoff', fmap unElbowCutoff elbowCutoff', _birchMinDistanceSearch config) of
                (_, Just x, Just _)       ->
                    Just . MinDistanceSearch $ elbowCut x getDistance tree
                (Just x, Nothing, Just _) ->
                    Just . MinDistanceSearch $ smartCut x getDistance tree
                otherwise                 -> _birchMinDistanceSearch config
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
        drawDiscretize'   = _birchDrawDiscretize config
        drawScaleSaturation' = _birchDrawScaleSaturation config
        drawFont'         = fromMaybe (DrawFont "Arial") $ _birchDrawFont config
        drawItemLineWeight' = fromMaybe (DrawItemLineWeight 0.1)
                            $ _birchDrawItemLineWeight config
        drawBarBounds'    = _birchDrawBarBounds config
        order'            = fromMaybe (Order 1) $ _birchOrder config
        mat               = return $ _birchMat config
        simMat            = _birchSimMat config

        -- Prune tree.
        tree' = foldl' (\acc f -> f acc) tree
              $ [ (\x -> bool x (flip clusterGraphToTree 0 . flip customCut (treeToGraph x) . unCustomCut $ customCut')
                       . not
                       . Set.null
                       . unCustomCut
                       $ customCut'
                  )
                , (\x -> maybe x (clusterGraphToTree (treeToGraph x) . unRootCut) rootCut')
                , (\x -> maybe x (flip proportionCut x . unMaxProportion) maxProportion')
                , (\x -> maybe x (flip distanceCut x . unMinDistance) minDistance')
                , (\x -> maybe x (flip distanceSearchCut x . unMinDistanceSearch) minDistanceSearch')
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
                                drawItemLineWeight'
                                drawBarBounds'

    -- Get the color of each label.
    let labelColorMapRaw =
            case drawColors' of
                Nothing   -> fmap (getLabelColorMap drawPalette') labelMap'
                (Just cs) -> fmap (getLabelCustomColorMap cs) labelMap'
        labelColorMap = fmap
                          (maybe id saturateLabelColorMap drawScaleSaturation')
                          labelColorMapRaw
        -- Get the mark color map.
        markColorMap = case drawMark' of
                        MarkNone -> Nothing
                        _ -> Just $ getMarkColorMap drawMark' gr
        defaultGetItemColorMap :: Maybe ItemColorMap
        defaultGetItemColorMap = do
            lcm <- labelColorMapRaw
            lm  <- labelMap'
            return $ labelToItemColorMap lcm lm
        -- Get the discrete color list.
        discreteColors = drawDiscretize'
                     >>= getDiscreteColorMap drawLeaf' drawColors' labelColorMap
        -- This function will decide whether to update a map or not based on
        -- user input.
        discretizeColorMapHelper :: (Eq a, Ord a)
                                 => Map.Map a (D.Colour Double)
                                 -> Map.Map a (D.Colour Double)
        discretizeColorMapHelper cm =
          maybe cm (flip discretizeColorMap cm) discreteColors

    -- Get the item color map.
    itemColorMapRaw <-
        case drawLeaf' of
            DrawItem (DrawContinuous x) ->
                fmap
                    ( fmap ( either error id
                           . getItemColorMapContinuous
                              drawColors'
                              (fmap Feature x)
                           )
                    )
                    mat
            DrawItem DrawSumContinuous  ->
                fmap (fmap (getItemColorMapSumContinuous drawColors')) mat
            _                           -> return defaultGetItemColorMap

    -- Get the item value map.
    itemValueMap <-
        case drawLeaf' of
            DrawItem (DrawContinuous x) ->
                fmap
                    ( fmap ( either error id
                           . getItemValueMap
                              (fmap Feature x)
                           )
                    )
                    mat
            DrawItem DrawSumContinuous  ->
                fmap (fmap getItemValueMapSum) mat
            _                           -> return Nothing

    -- Get the node color map.
    let itemColorMap = fmap ( ItemColorMap
                            . discretizeColorMapHelper
                            . unItemColorMap
                            . (maybe id saturateItemColorMap drawScaleSaturation')
                            )
                        itemColorMapRaw
        nodeColorMap =
          fmap ( NodeColorMap
               . discretizeColorMapHelper
               . unNodeColorMap
               . maybe id saturateNodeColorMap drawScaleSaturation'
               ) $
            case drawLeaf' of
                (DrawItem DrawDiversity) ->
                    fmap
                        (getNodeColorMapFromDiversity drawColors' order' gr)
                        itemColorMapRaw
                _ -> fmap (getNodeColorMapFromItems gr) $ itemColorMapRaw
    -- Get the graph at each leaf (if applicable).
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

    -- Get the legend of the diagram.
    legend <- case drawLeaf' of
                (DrawItem (DrawContinuous x)) ->
                    fmap
                        ( fmap ( plotContinuousLegend
                                  drawFont'
                                  drawBarBounds'
                                  drawColors'
                                  discreteColors
                                  (fromMaybe (DrawScaleSaturation 1) drawScaleSaturation')
                                  (fmap Feature x)
                               )
                        )
                        mat
                (DrawItem DrawSumContinuous) ->
                    fmap
                      ( fmap
                          ( plotSumContinuousLegend
                            drawFont'
                            drawBarBounds'
                            drawColors'
                            discreteColors
                            (fromMaybe (DrawScaleSaturation 1) drawScaleSaturation')
                          )
                      )
                      mat
                (DrawItem DrawDiversity) -> return mempty
                _ -> return $ do
                    lm <- labelMap'
                    lcm <- labelColorMap
                    return
                        . plotLabelLegend drawFont'
                        . bool
                              (subsetLabelColorMap gr lm)
                              id
                              (unDrawLegendAllLabels drawLegendAllLabels')
                        $ lcm

    -- Get the entire diagram.
    plot <- plotGraph
                legend
                drawConfig
                drawFont'
                itemColorMap
                itemValueMap
                nodeColorMap
                markColorMap
                leafGraphMap
                gr

    return (plot, labelColorMap, itemColorMap, markColorMap, tree', gr)
