{- birch-beer
Gregory W. Schwartz

Displays a hierarchical tree of clusters with colors, scaling, and more.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Control.Monad (when)
import Data.Char (ord)
import Data.Colour.SRGB (sRGB24read)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Math.Clustering.Hierarchical.Spectral.Load (readSparseAdjMatrix)
import Options.Generic
import System.IO (openFile, hClose, IOMode (..))
import Text.Read (readMaybe, readEither)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D

-- Local
import BirchBeer.ColorMap
import BirchBeer.Interactive
import BirchBeer.LeafGraph
import BirchBeer.Load
import BirchBeer.MainDiagram
import BirchBeer.Options
import BirchBeer.Types
import BirchBeer.Utility

main :: IO ()
main = do
    opts <- getRecord "birch-beer, Gregory W. Schwartz.\
                      \ Displays a hierarchical tree of clusters with colors,\
                      \ scaling, and more."

    let input'            = unHelpful . input $ opts
        inputMatrix'      = unHelpful . inputMatrix $ opts
        delimiter'        =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        labelsFile'       = fmap LabelFile . unHelpful . labelsFile $ opts
        minSize'          = fmap MinClusterSize . unHelpful . minSize $ opts
        maxStep'          = fmap MaxStep . unHelpful . maxStep $ opts
        maxProportion'    = fmap MaxProportion . unHelpful . maxProportion $ opts
        minDistance'      = fmap MinDistance . unHelpful . minDistance $ opts
        minDistanceSearch' = fmap MinDistanceSearch . unHelpful . minDistanceSearch $ opts
        smartCutoff'      = fmap SmartCutoff . unHelpful . smartCutoff $ opts
        elbowCutoff'      =
          fmap ( ElbowCutoff
               . fromMaybe (error "Cannot read --elbow-cutoff.")
               . readMaybe
               )
            . unHelpful
            . elbowCutoff
            $ opts
        customCut'        = CustomCut . Set.fromList . unHelpful . customCut $ opts
        rootCut'          = fmap RootCut . unHelpful . rootCut $ opts
        order'            = fmap Order . unHelpful . order $ opts
        drawLeaf'            =
            maybe
              (maybe DrawText (const (DrawItem DrawLabel)) labelsFile')
              (fromMaybe (error "Cannot read --draw-leaf.") . readMaybe)
                . unHelpful
                . drawLeaf
                $ opts
        drawCollection'      = maybe
                                 PieChart
                                 (fromMaybe (error "Cannot read --draw-collection.") . readMaybe)
                             . unHelpful
                             . drawCollection
                             $ opts
        drawMark'            = maybe
                                   MarkNone
                                   (fromMaybe (error "Cannot read --draw-mark.") . readMaybe)
                             . unHelpful
                             . drawMark
                             $ opts
        drawNodeNumber'      = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        drawMaxNodeSize'     =
            DrawMaxNodeSize . fromMaybe 72 . unHelpful . drawMaxNodeSize $ opts
        drawMaxLeafNodeSize' = DrawMaxLeafNodeSize
                             . fromMaybe (unDrawMaxNodeSize drawMaxNodeSize')
                             . unHelpful
                             . drawMaxLeafNodeSize
                             $ opts
        drawNoScaleNodes' =
            DrawNoScaleNodesFlag . unHelpful . drawNoScaleNodes $ opts
        drawLegendSep'    = DrawLegendSep
                          . fromMaybe 1
                          . unHelpful
                          . drawLegendSep
                          $ opts
        drawLegendAllLabels' =
            DrawLegendAllLabels . unHelpful . drawLegendAllLabels $ opts
        drawPalette'    = maybe
                           Set1
                           (fromMaybe (error "Cannot read --palette") . readMaybe)
                        . unHelpful
                        . drawPalette
                        $ opts
        drawColors'     = fmap ( CustomColors
                               . fmap sRGB24read
                               . (\x -> read x :: [String])
                               )
                        . unHelpful
                        . drawColors
                        $ opts
        drawDiscretize' = (=<<) (\x -> either error Just
                                . either
                                    (\ err -> either
                                                (\y -> Left $ finalError err y)
                                                (Right . SegmentColorMap)
                                              (readEither x :: Either String Int)
                                    )
                                    (Right . CustomColorMap . fmap sRGB24read)
                                $ (readEither x :: Either String [String])
                                )
                        . unHelpful
                        . drawDiscretize
                        $ opts
          where
            finalError err x = "Error in draw-discretize: " <> err <> " " <> x
        drawScaleSaturation' =
            fmap DrawScaleSaturation . unHelpful . drawScaleSaturation $ opts
        drawFont' = fmap DrawFont . unHelpful . drawFont $ opts
        drawItemLineWeight' = fmap DrawItemLineWeight
                            . unHelpful
                            . drawItemLineWeight
                            $ opts
        drawBarBounds'    = DrawBarBounds . unHelpful . drawBarBounds $ opts
        output'           =
            fromMaybe "dendrogram.svg" . unHelpful . output $ opts

    tree <- loadTreeOrDendFromFile input'

    -- Get the label map from either a file or from expression thresholds.
    labelMap <- case drawLeaf' of
                    (DrawItem (DrawThresholdContinuous gs)) ->
                        error "Threshold not supported here."
                        -- fmap
                        --     ( Just
                        --     . getLabelMapThresholdContinuous
                        --         (fmap (L.over L._1 Feature) gs)
                        --     . fromMaybe (error "Requires matrix.")
                        --     )
                        --     mat
                    (DrawItem (DrawProximity gs)) ->
                        error "Proximity not supported here."
                    _ -> sequence . fmap (loadLabelData delimiter') $ labelsFile'

    let readMat file =
            if isSuffixOf ".mtx" file
                then do
                    mat <- loadMatrix file

                    let items = V.fromList . fmap showt $ [1..S.nrows mat]

                    return . SimilarityMatrix $ NamedMatrix mat items items
                else do
                    let decodeOpt = CSV.defaultDecodeOptions
                                    { CSV.decDelimiter =
                                        fromIntegral (ord . unDelimiter $ delimiter')
                                    }

                    h <- openFile file ReadMode
                    (items, mat) <- readSparseAdjMatrix decodeOpt h
                    hClose h

                    return . SimilarityMatrix $ NamedMatrix mat items items

    simMat <- sequence . fmap readMat $ inputMatrix'

    let config :: Config T.Text NamedMatrix
        config = Config { _birchLabelMap            = labelMap
                        , _birchMinSize             = minSize'
                        , _birchMaxStep             = maxStep'
                        , _birchMaxProportion       = maxProportion'
                        , _birchMinDistance         = minDistance'
                        , _birchMinDistanceSearch   = minDistanceSearch'
                        , _birchSmartCutoff         = smartCutoff'
                        , _birchElbowCutoff         = elbowCutoff'
                        , _birchCustomCut           = customCut'
                        , _birchRootCut             = rootCut'
                        , _birchOrder               = order'
                        , _birchDrawLeaf            = drawLeaf'
                        , _birchDrawCollection      = drawCollection'
                        , _birchDrawMark            = drawMark'
                        , _birchDrawNodeNumber      = drawNodeNumber'
                        , _birchDrawMaxNodeSize     = drawMaxNodeSize'
                        , _birchDrawMaxLeafNodeSize = drawMaxLeafNodeSize'
                        , _birchDrawNoScaleNodes    = drawNoScaleNodes'
                        , _birchDrawLegendSep       = drawLegendSep'
                        , _birchDrawLegendAllLabels = drawLegendAllLabels'
                        , _birchDrawPalette         = drawPalette'
                        , _birchDrawColors          = drawColors'
                        , _birchDrawDiscretize      = drawDiscretize'
                        , _birchDrawScaleSaturation = drawScaleSaturation'
                        , _birchDrawFont            = drawFont'
                        , _birchDrawItemLineWeight  = drawItemLineWeight'
                        , _birchDrawBarBounds       = drawBarBounds'
                        , _birchTree                = tree
                        , _birchMat                 = Nothing
                        , _birchSimMat              = simMat
                        }

    (plot, _, _, _, tree', gr) <- mainDiagram config

    -- Plot tree.
    D.renderCairo
            output'
            (D.mkHeight 1000)
            plot

    -- Write new tree if necessary.
    mapM_ (\x -> B.writeFile x . A.encode $ tree') . unHelpful . jsonOutput $ opts

    -- Write the csv if necessary.
    mapM_ (\x -> B.writeFile x . printNodeAssignments . getNodeAssignments $ gr)
      . unHelpful
      . csvOutput
      $ opts

    when (unHelpful . interactive $ opts) $ interactiveDiagram tree labelMap (Nothing :: Maybe NamedMatrix) simMat

    return ()
