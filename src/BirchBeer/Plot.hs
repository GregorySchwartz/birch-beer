{- BirchBeer.Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the clusterings.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module BirchBeer.Plot
    ( plotDendrogram
    , plotGraph
    , plotLabelLegend
    , plotContinuousLegend
    , plotSumContinuousLegend
    ) where

-- Remote
import Control.Monad (forM, mapM, join)
import Control.Monad.State (State (..))
import Data.Colour (AffineSpace (..), withOpacity)
import Data.Colour.Names (black)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..), Kolor)
import qualified Data.Colour.Palette.BrewerSet as Brewer
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.Function (on)
import Data.List (nub, sort, sortBy, foldl1', transpose)
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (swap)
import Diagrams.Backend.Cairo
import Diagrams.Dendrogram (dendrogramCustom, Width(..))
import Diagrams.Prelude
import Diagrams.TwoD.Arrow
import Graphics.SVGFonts
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend)
import Plots
import Plots.Axis.ColourBar
import Plots.Legend
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Diagrams.TwoD.GraphViz as G hiding (mkGraph)

-- Local
import BirchBeer.Types
import BirchBeer.Utility
import BirchBeer.ColorMap

-- | Plot a dendrogram.
plotDendrogram
    :: (TreeItem a)
    => Maybe (Diagram B)
    -> DrawLeaf
    -> Maybe ItemColorMap
    -> HC.Dendrogram (V.Vector a)
    -> Diagram B
plotDendrogram Nothing drawLeaf cm dend =
    pad 1
        . center
        . dendrogramCustom
            Variable
            (whichLeaf drawLeaf $ cm)
            (dendrogramPathLabel drawLeaf cm)
            ((\tree items -> lw 0.3 . scaleToY (3 * height items) $ tree), curry snd)
        $ dend
  where
    whichLeaf DrawText     = dendrogramLeafLabel
    whichLeaf (DrawItem _) = dendrogramLeafItem
plotDendrogram (Just legend) drawLeaf cm dend =
    pad 1
        . hsep 1
        $   [ alignT
            . center
            . dendrogramCustom
                Variable
                (whichLeaf drawLeaf $ cm)
                (dendrogramPathLabel drawLeaf cm)
                ((\tree items -> lw 0.3 . scaleToY (2 * height items) $ tree), curry snd)
            $ dend
            , pad 2 . alignT . lw 0.3 center . scaleUToY (height tree / 6) $ legend
            ]
  where
    tree = alignT
         . center
         . dendrogramCustom
             Variable
             (whichLeaf drawLeaf $ cm)
             (dendrogramPathLabel drawLeaf cm)
             ((\tree items -> lw 0.1 . scaleToY (3 * height items) $ tree), curry snd)
         $ dend
    whichLeaf DrawText     = dendrogramLeafLabel
    whichLeaf (DrawItem _) = dendrogramLeafItem

-- | Get the most frequent color of a dendrogram.
getMostFrequentColorDend
    :: (TreeItem a)
    => ItemColorMap -> HC.Dendrogram (V.Vector a) -> Kolor
getMostFrequentColorDend (ItemColorMap cm) (HC.Leaf leaf) =
    getMostFrequent
        . fmap (flip (Map.findWithDefault black) cm . getId)
        . V.toList
        $ leaf
getMostFrequentColorDend (ItemColorMap cm) dend =
    getMostFrequent
        . concatMap ( fmap (flip (Map.findWithDefault black) cm . getId)
                    . V.toList
                    )
        . getClusterItemsDend
        $ dend

-- | Get the most frequent color of a list.
getMostFrequentColorList
    :: (TreeItem a)
    => Maybe ItemColorMap -> [a] -> Kolor
getMostFrequentColorList Nothing                  = const black
getMostFrequentColorList (Just (ItemColorMap cm)) =
    getMostFrequent . fmap (flip (Map.findWithDefault black) cm . getId)

-- | How to draw the path at each junction.
dendrogramPathLabel
    :: (TreeItem a)
    => DrawLeaf
    -> Maybe ItemColorMap
    -> HC.Dendrogram (V.Vector a)
    -> Diagram B
    -> Diagram B
dendrogramPathLabel _ Nothing _ d                       = d
dendrogramPathLabel (DrawItem (DrawContinuous _)) _ _ d = d
dendrogramPathLabel (DrawItem DrawSumContinuous) _ _ d  = d
dendrogramPathLabel _ (Just cm) dend d                  = lc color d
  where
    color = getMostFrequentColorDend cm dend

-- | Plot the leaf of a dendrogram as a text label.
dendrogramLeafLabel
    :: (TreeItem a)
    => Maybe ItemColorMap -> V.Vector a -> Diagram B
dendrogramLeafLabel Nothing leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unId . getId . V.head $ leaf) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
dendrogramLeafLabel cm leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unId . getId . V.head $ leaf) 1) # rotateBy (1/4) # fc black # lw none # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc color # lw none # centerX # pad 1.3 # alignT
  where
    color = getMostFrequentColorList cm . V.toList $ leaf

-- | Plot the leaf of a dendrogram as a collection of items.
dendrogramLeafItem
    :: (TreeItem a)
    => Maybe ItemColorMap -> V.Vector a -> Diagram B
dendrogramLeafItem Nothing leaf = getItem black # centerX # pad 1.3 # alignT
dendrogramLeafItem (Just (ItemColorMap cm)) leaf =
    (vcat . fmap hcat . Split.chunksOf 10 $ items) # centerX # pad 1.3 # alignT
  where
    items  = fmap getItem colors
    colors = sort
           . fmap (flip (Map.findWithDefault black) cm . getId)
           . V.toList
           $ leaf

-- | Plot the node of a graph as a text label.
drawGraphLabel
    :: (TreeItem a)
    => Maybe ItemColorMap -> Seq.Seq a -> Diagram B
drawGraphLabel Nothing items =
    case Seq.length items of
        1 -> stroke (textSVG (T.unpack . unId . getId . flip Seq.index 0 $ items) 1) # fc black # lw none # center
        s -> stroke (textSVG (show s) 1) # fc black # lw none # center
drawGraphLabel cm items =
    case Seq.length items of
        1 -> stroke (textSVG (T.unpack . unId . getId . flip Seq.index 0 $ items) 1) # fc black # lw none # center
        s -> stroke (textSVG (show s) 1) # fc color # lw none # center
  where
    color = getMostFrequentColorList cm . F.toList $ items

-- | Plot the node of a graph as a collection of items.
drawGraphItem :: (TreeItem a) => Maybe ItemColorMap -> Seq.Seq a -> Diagram B
drawGraphItem Nothing _ = getItem black # centerX # pad 1.3 # center
drawGraphItem (Just (ItemColorMap cm)) node =
    (vcat . fmap hcat . Split.chunksOf boxSize $ items) # center
  where
    boxSize = floor . sqrt . fromIntegral . Seq.length $ node
    items  = fmap getItem colors
    colors = sort
           . fmap (flip (Map.findWithDefault black) cm . getId)
           . F.toList
           $ node

-- | Plot a collection of items.
drawCollectionItem
    :: (TreeItem a)
    => DrawCollection
    -> Maybe ItemColorMap
    -> Maybe LeafGraphDiaMap
    -> G.Node
    -> Seq.Seq a
    -> Diagram B
drawCollectionItem (CollectionGraph{}) _ Nothing _ _ = mempty
drawCollectionItem (CollectionGraph{}) _ (Just (LeafGraphDiaMap lgdm)) n _ =
    fromMaybe mempty . Map.lookup n $ lgdm
drawCollectionItem _ Nothing _ _ _ = mempty
drawCollectionItem drawCollection (Just (ItemColorMap cm)) _ _ items =
    renderAxis $ polarAxis &~ do
        let colorWedge :: (Kolor, Double) -> State (Plot (Wedge Double) b) ()
            colorWedge colorPair = do
                plotColor .= fst colorPair
                areaStyle . _lw .= none
            colorPairs = sortBy (compare `on` snd)
                       . getFractions
                       . fmap (flip (Map.findWithDefault black) cm . getId)
                       . F.toList
                       $ items

        piePlot colorPairs snd $ onWedges colorWedge
        case drawCollection of
            PieChart -> return ()
            _        -> wedgeInnerRadius .= 0.9
        hide (axes . traversed)

-- | Draw a single item.
getItem :: Kolor -> Diagram B
getItem color = circle 1 # lc black # fc color # lwL 0.1 -- Unfortunately cannot change to lwL or they disappear with certain scalings.

plotLabelLegend :: LabelColorMap -> Diagram B
plotLabelLegend = flip (drawLegend emptyBox) legendOpts
                . fmap plotLabel
                . Map.toAscList
                . unLabelColorMap
  where
    legendOpts :: Legend B Double
    legendOpts = over legendTextStyle (const (mempty # font "Arial" # fontSize (local size)))
               . over legendStyle (lw none)
               $ def
    plotLabel :: (Label, Colour Double) -> (Diagram B, String)
    plotLabel (!l, !c) =
        ( circle size # lc black # fc c # lw (local $ 0.1 * size)
        , T.unpack . unLabel $ l
        )
    size :: Double
    size = (/ 2.5) $ (def :: Legend B Double) ^. legendSpacing

-- | Get the legend for a feature. Bar from
-- https://archives.haskell.org/projects.haskell.org/diagrams/blog/2013-12-03-Palette1.html
plotContinuousLegend :: (MatrixLike a) => Feature -> a -> Diagram B
plotContinuousLegend g mat
    | isNothing col = mempty
    | otherwise = rotateBy (3 / 4)
                $ renderColourBar
                    cbOpts cm (fromMaybe 0 minVal, fromMaybe 0 maxVal) 100
  where
    cbOpts :: ColourBar B Double
    cbOpts = over tickLabelStyle (font "Arial")
           . set visible True
           $ defColourBar
    cm =
        colourMap . zip [1..] . fmap (\x -> sRGB 1 (1 - x) (1 - x)) $ [0,0.1..1]
    (minVal, maxVal) = Fold.fold ((,) <$> Fold.minimum <*> Fold.maximum)
                     . flip S.extractCol (colErr col)
                     . getMatrix
                     $ mat
    colErr = fromMaybe (error $ "Feature " <> T.unpack (unFeature g) <> " does not exist.")
    col = V.elemIndex g
        . fmap Feature
        . getColNames
        $ mat

-- | Get the legend for the sum of all features. Bar from
-- https://archives.haskell.org/projects.haskell.org/diagrams/blog/2013-12-03-Palette1.html
plotSumContinuousLegend :: (MatrixLike a) => a -> Diagram B
plotSumContinuousLegend mat =
    rotateBy (3 / 4)
        $ renderColourBar cbOpts cm (fromMaybe 0 minVal, fromMaybe 0 maxVal) 100
  where
    cbOpts :: ColourBar B Double
    cbOpts = over tickLabelStyle (font "Arial")
           . set visible True
           $ defColourBar
    cm =
        colourMap . zip [1..] . fmap (\x -> sRGB 1 (1 - x) (1 - x)) $ [0,0.1..1]
    (minVal, maxVal) = Fold.fold ((,) <$> Fold.minimum <*> Fold.maximum)
                     . fmap sum
                     . S.toRowsL
                     . getMatrix
                     $ mat

-- | Get the maximum cluster size from a graph.
maxClusterSize :: G.Gr (G.Node, Maybe (Seq.Seq a)) e -> Int
maxClusterSize = maximum
               . fmap (maybe 0 Seq.length . snd)
               . F.toList
               . flip getGraphLeaves 0

-- | Get the size of a leaf for graph plotting. Determines the size based on the
-- diameter of each node (36) and the maximum size of a cluster.
getScaledLeafSize :: Int -> Double -> Seq.Seq a -> Double
getScaledLeafSize maxLen maxSize =
    isTo (fromIntegral . floor . sqrt . fromIntegral $ maxLen) (maxSize / 2)
        . fromIntegral
        . floor
        . sqrt
        . fromIntegral
        . Seq.length

-- | Get the trail making the edge.
getEdgeTrail
    :: P2 Double -> P2 Double -> Double -> Double -> Trail' Loop V2 Double
getEdgeTrail p1 p2 h1 h2 = fromVertices
                                  [ perpendicular p1 p2 d1
                                  , perpendicular p1 p2 (-d1)
                                  , perpendicular p2 p1 d2
                                  , perpendicular p2 p1 (-d2)
                                  ]
                               # closeLine
  where
    d1 = h1 / 2
    d2 = h2 / 2

-- | Distance d away from p on perpendicular line from p to q, by byorgey.
perpendicular :: P2 Double -> P2 Double -> Double -> P2 Double
perpendicular p q d = p .+^ ((q .-. p) # perp # normalize # scale d)

-- | Get the linear gradient of an edge.
getEdgeGrad :: P2 Double
            -> Kolor
            -> P2 Double
            -> Kolor
            -> Double
            -> Double
            -> Texture Double
getEdgeGrad p1 c1 p2 c2 h1 h2 =
    mkLinearGradient (mkStops [(c1, 0, 1), (c2, 1, 1)]) start end GradPad
  where
    start  = perpendicular startPoint ((0 ^& 0) :: P2 Double) (- d1)  -- Start gradient after node.
    end  = endPoint .+^ ((startPoint .-. endPoint) # normalize # scale d2) -- End gradient before node.
    startPoint = p1 ^-^ origin
    endPoint    = p2 ^-^ origin
    origin = perpendicular p1 p2 d1
    d1     = h1 / 2
    d2     = h2 / 2

-- | The function to draw a path connection two nodes in a graph.
drawGraphPath
    :: (TreeItem a)
    => DrawConfig
    -> Maybe NodeColorMap
    -> ClusterGraph a
    -> (G.Node, Maybe (Seq.Seq a))
    -> P2 Double
    -> (G.Node, Maybe (Seq.Seq a))
    -> P2 Double
    -> HC.Distance
    -> Path V2 Double
    -> Diagram B
drawGraphPath opts ncm gr (n1, _) p1 (n2, _) p2 _ _ =
    strokeLoop trail
        # lw none
        # fillTexture gradient
        # flip place (perpendicular p1 p2 d1)
  where
    gradient = getEdgeGrad p1 c1 p2 c2 (height draw1) (height draw2)
    d1           = height draw1 / 2
    draw1        = drawNode n1 p1
    draw2        = drawNode n2 p2
    c1           = getNodeColor ncm n1
    c2           = getNodeColor ncm n2
    trail        = getEdgeTrail p1 p2 (height draw1) (height draw2)
    drawNode n p = drawGraphNode opts Nothing ncm Nothing Nothing gr (n, Nothing) p -- DrawText is irrelevant here.

-- | Determine the smallest axis.
smallestAxis dia = min (width dia) (height dia)

-- | Determine the axis to scale on.
scaleAxis dia = if width dia > height dia
                    then scaleUToX
                    else scaleUToY

-- | Draw the final node of a graph.
drawGraphNode :: (TreeItem a)
              => DrawConfig
              -> Maybe ItemColorMap
              -> Maybe NodeColorMap
              -> Maybe MarkColorMap
              -> Maybe LeafGraphDiaMap
              -> ClusterGraph a
              -> (G.Node, Maybe (Seq.Seq a))
              -> P2 Double
              -> Diagram B
drawGraphNode opts@(DrawConfig { _drawLeaf = DrawText }) cm _ _ _ gr (n, Just items) pos =
    ((textDia dnn # fontSize (local $ smallestAxis dia)) <> dia <> background)
        # scaleUToX scaleVal
        # moveTo pos
  where
    dia = itemDia # scaleAxis itemDia scaleVal
    itemDia = drawGraphLabel cm items
    background = circle 1 # fc white # lw none # scaleUToY scaleVal
    maxNodeSize = unDrawMaxNodeSize . _drawMaxNodeSize $ opts
    textDia True  = text (show n) # fc black
    textDia False = mempty
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
    scaleVal = if unDrawNoScaleNodesFlag . _drawNoScaleNodesFlag $ opts
                then maxNodeSize
                else getScaledLeafSize maxClusterSize' maxNodeSize items
    maxClusterSize' = maxClusterSize . unClusterGraph $ gr
drawGraphNode opts@(DrawConfig { _drawLeaf = (DrawItem drawType) }) cm ncm _ lgdm gr (n, Just items) pos =
    ((textDia dnn # fontSize (local $ smallestAxis dia)) <> dia) # moveTo pos
  where
    dia = drawLeafDia drawType
    drawLeafDia DrawDiversity =
        ( (circle 1 # fc (getNodeColor ncm n) # lw none # scaleUToY scaleVal)
            <> background PieChart
        )
    drawLeafDia _ = ( collectionDia (_drawCollection opts)
                   <> itemsDia
                   <> background (_drawCollection opts)
                    )
    background PieNone = roundedRect (width itemsDia) (height itemsDia) 1 # fc white # lw none # scaleUToY (scaleVal * 1.1)
    background x@(CollectionGraph{}) = roundedRect (width (collectionDia x)) (height (collectionDia x)) 1 # fc white # lw none # scaleUToY (scaleVal * 1.1)
    background _       = circle 1 # fc white # lw none # scaleUToY scaleVal
    itemsDia              = getItemsDia $ _drawCollection opts
    getItemsDia PieNone   = scaleUToY scaleVal $ drawGraphItem cm items
    getItemsDia PieChart  = mempty
    getItemsDia (CollectionGraph{}) = mempty
    getItemsDia _         = scaleUToY (0.5 * scaleVal) $ drawGraphItem cm items
    collectionDia PieNone = mempty
    collectionDia x       =
        scaleUToY scaleVal $ drawCollectionItem x cm lgdm n items
    scaleVal = if unDrawNoScaleNodesFlag . _drawNoScaleNodesFlag $ opts
                then maxNodeSize
                else getScaledLeafSize maxClusterSize' maxNodeSize items
    maxNodeSize = unDrawMaxNodeSize . _drawMaxNodeSize $ opts
    maxClusterSize' = maxClusterSize . unClusterGraph $ gr
    textDia True  = text (show n) # fc black
    textDia False = mempty
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
drawGraphNode opts cm ncm mcm _ gr (n, Nothing) pos =
    (mark mcm <> mainNode) # moveTo pos
  where
    mainNode = ((textDia dnn # fontSize (local $ 1)) <> mainDia)
             # scaleUToY (getNodeSize (_drawNoScaleNodesFlag opts) items)
    mainDia = circle 1 # fc color # rootDiffer n
    mark :: Maybe MarkColorMap -> Diagram B
    mark Nothing = mempty
    mark (Just (MarkColorMap cm)) =
        circle 1
            # fcA transparent
            # lcA (Map.findWithDefault transparent n cm)
            # lw 0.4
            # scaleUToY (unDrawMaxNodeSize . _drawMaxNodeSize $ opts)
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
    textDia True  = text (show n) # fc black
    textDia False = mempty
    rootDiffer 0 = lw none
    rootDiffer n = lw none
    color = getNodeColor ncm n
    items = getGraphLeafItems gr n
    getNodeSize :: DrawNoScaleNodesFlag -> Seq.Seq a -> Double
    getNodeSize (DrawNoScaleNodesFlag False) =
        isTo totalItems (unDrawMaxNodeSize . _drawMaxNodeSize $ opts)
            . fromIntegral
            . Seq.length
    getNodeSize (DrawNoScaleNodesFlag True) =
        const (unDrawMaxNodeSize . _drawMaxNodeSize $ opts)
    totalItems = fromIntegral . Seq.length . getGraphLeafItems gr $ 0

-- | Plot the graph of a leaf.
plotLeafGraph
    :: (Ord a, TreeItem a)
    => Maybe ItemColorMap -> LeafGraph a -> IO (Diagram B)
plotLeafGraph cm (LeafGraph gr) = do
    let params :: (TreeItem a) => G.GraphvizParams Int (G.Node, a) Double () (G.Node, a)
        params = G.defaultDiaParams
            { G.fmtEdge = (\(_, _, w) -> [G.Weight . G.Int . round $ 100 * w])
            -- , G.globalAttributes = [G.GraphAttrs { G.attrs = [G.Sep $ G.DVal 36] }]
            }

    layout <- G.layoutGraph G.Neato gr

    let drawNode (_, x) pos =
            ( getItem
                . maybe black ( Map.findWithDefault black (getId x)
                              . unItemColorMap
                              )
                $ cm
            )
                # scaleUToY 36
                # moveTo pos
        drawEdge _ p1 _ p2 w p = arrowBetween' (opts p) p1 p2
                               # lc (blend w black white)
                               # lwL 5
        opts p = with
               & gaps .~ 0
               & arrowHead .~ noHead
               & arrowShaft .~ (unLoc . head $ pathTrails p)
        treeDia =
            G.drawGraph'
                drawNode
                drawEdge
                layout

    return $ treeDia # center

-- | Plot a graph rather than a traditional tree. Uses only info in leaves
-- rather than a tree which stores all leaves.
plotGraph
    :: (Ord a, TreeItem a)
    => Maybe (Diagram B)
    -> DrawConfig
    -> Maybe ItemColorMap
    -> Maybe NodeColorMap
    -> Maybe MarkColorMap
    -> Maybe (LeafGraphMap T.Text)
    -> ClusterGraph a
    -> IO (Diagram B)
plotGraph legend opts cm ncm mcm lgm (ClusterGraph gr) = do
    let numClusters :: Double
        numClusters = fromIntegral . Seq.length $ getGraphLeaves gr 0
        params :: (TreeItem a) => G.GraphvizParams Int (G.Node, Maybe (Seq.Seq a)) HC.Distance () (G.Node, Maybe (Seq.Seq a))
        params = G.defaultDiaParams
            { G.fmtEdge = (\(_, _, w) -> [G.Len w])
            , G.globalAttributes = [G.GraphAttrs { G.attrs = [G.Sep $ G.DVal 36] }]
            }

    layout <- G.layoutGraph' params G.TwoPi gr

    -- Get the graphs at each leaf (if applicable).
    lgdm <- sequence
          . fmap ( fmap LeafGraphDiaMap
                 . sequence
                 . Map.map (plotLeafGraph cm)
                 . unLeafGraphMap
                 )
          $ lgm

    let treeDia =
            G.drawGraph'
                (drawGraphNode opts cm ncm mcm lgdm (ClusterGraph gr))
                (drawGraphPath opts ncm (ClusterGraph gr))
                layout
        dia = case legend of
                Nothing  -> pad 1 . center $ treeDia
                (Just l) ->
                    pad 1
                        . hsep
                            1
                        $   [ alignY 1.5 . lw 0.3 . center . scaleUToX (width treeDia / 8) $ l
                            , alignT . center $ treeDia
                            ]

    return dia
