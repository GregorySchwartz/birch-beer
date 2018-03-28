{- BirchBeer.MainDiagram
Gregory W. Schwartz

Main function for generating the diagram, processing the preliminary arguments.
-}

{-# LANGUAGE OverloadedStrings #-}

module BirchBeer.MainDiagram
    ( mainDiagram
    ) where

-- Remote
import Data.Maybe (fromMaybe)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H

-- Local
import BirchBeer.ColorMap
import BirchBeer.Load
import BirchBeer.Plot
import BirchBeer.Types
import BirchBeer.Utility

-- | Main function for generating the diagram and all needed arguments. For R,
-- requires withEmbeddedR to start an instance before calling this function.
mainDiagram
    :: (Eq a, Ord a, TreeItem a, MatrixLike b)
    => Config a b -> IO (D.Diagram D.B)
mainDiagram config = do
    let delimiter'        = _birchDelimiter config
        labelsFile'       = _birchLabelsFile config
        minSize'          = _birchMinStep config
        maxStep'          = _birchMaxStep config
        drawLeaf'         = _birchDrawLeaf config
        drawPie'          = _birchDrawPie config
        drawMark'         = _birchDrawMark config
        drawNodeNumber'   = _birchDrawNodeNumber config
        drawMaxNodeSize'  = _birchDrawMaxNodeSize config
        drawNoScaleNodes' = _birchDrawNoScaleNodes config
        drawColors'       = _birchDrawColors config
        dend              = _birchDend config
        mat               = return $ _birchMat config

        -- Prune dendrogram.
        dend' = (\ y
                -> maybe
                    y
                    (flip sizeCutDendrogramV y . unMinClusterSize)
                    minSize'
                )
                . maybe dend (flip stepCutDendrogram dend . unMaxStep)
                $ maxStep'
        -- Load graph.
        gr    = dendrogramToGraph dend'

        -- Load draw configurations.
        drawConfig        = DrawConfig
                                drawLeaf'
                                drawPie'
                                drawNodeNumber'
                                drawMaxNodeSize'
                                drawNoScaleNodes'

    -- Get the label map from either a file or from expression thresholds.
    labelMap <- case drawLeaf' of
                    (DrawItem (DrawThresholdContinuous gs)) ->
                        fmap
                            ( Just
                            . getLabelMapThresholdContinuous
                                (fmap (L.over L._1 Feature) gs)
                            . fromMaybe (error "Requires matrix.")
                            )
                            mat
                    _ -> sequence . fmap (loadLabelData delimiter') $ labelsFile'

    -- Get the color of each label.
    labelColorMap <- H.runRegion $ do
        case drawColors' of
            Nothing   ->
                sequence $ fmap (getLabelColorMap Set1) labelMap
            (Just cs) ->
                return
                    $ fmap (getLabelCustomColorMap cs) labelMap

        -- | Get the mark color map.
    let markColorMap = case drawMark' of
                        MarkModularity -> Just $ getMarkColorMap gr
                        _ -> Nothing
        defaultGetItemColorMap :: Maybe ItemColorMap
        defaultGetItemColorMap = do
            lcm <- labelColorMap
            lm  <- labelMap
            return $ labelToItemColorMap lcm lm
                        
    -- | Get the item color map.
    itemColorMap <-
        case drawLeaf' of
            DrawItem (DrawContinuous x) ->
                fmap (fmap (getItemColorMapContinuous (Feature x))) mat
            DrawItem DrawSumContinuous  ->
                fmap (fmap getItemColorMapSumContinuous) mat
            _                           -> return defaultGetItemColorMap
                        
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
    plotGraph legend drawConfig itemColorMap markColorMap gr
