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
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.Csv as CSV
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
import BirchBeer.Types
import BirchBeer.Utility

-- | Command line arguments
data Options = Options
    { input :: String <?> "(FILE) The input JSON file."
    , inputMatrix :: Maybe String <?> "([Nothing] | FILE) The input adjacency matrix file for CollectionGraph (matrix market format if ends in .mtx, \"i,j,value\" without header otherwise and text labels will be sorted when converting indices)."
    , output :: Maybe String <?> "([dendrogram.svg] | FILE) The output file."
    , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for csv files."
    , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each item, with \"item,label\" header."
    , minSize :: Maybe Int <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
    , maxStep :: Maybe Int <?> "([Nothing] | INT) Only keep clusters that are INT steps from the root. Defaults to all steps."
    , maxProportion :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE proportion split. So a node N with L and R children will stop with this criteria at 0.5 if |L| / |R| < 0.5 or > 2 (absolute log2 transformed), that is, if one child has over twice as many items as the other child. Includes L and R in the final result."
    , minDistance :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE distance. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Includes L and R in the final result."
    , smartCutoff :: Maybe Double <?> "([Nothing] | DOUBLE) Whether to set the cutoffs for --min-size, --max-proportion, and --min-distance based off of the distributions (median + (DOUBLE * MAD)) of all nodes. To use smart cutoffs, use this argument and then set one of the three arguments to an arbitrary number, whichever cutoff type you want to use. --min-size distribution is log2 transformed."
    , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity for DrawItem DrawDiversity."
    , drawLeaf :: Maybe String <?> "([DrawText] | DrawItem DrawItemType) How to draw leaves in the dendrogram. DrawText is the number of items in that leaf. DrawItem is the collection of items represented by circles, consisting of: DrawItem DrawLabel, where each item is colored by its label, DrawItem (DrawContinuous FEATURE), where each item is colored by the expression of FEATURE (corresponding to a feature name in the input matrix), DrawItem (DrawThresholdContinuous [(FEATURE, DOUBLE)], where each item is colored by the binary high / low expression of FEATURE based on DOUBLE and multiple FEATUREs can be used to combinatorically label items (FEATURE1 high / FEATURE2 low, etc.), DrawItem DrawSumContinuous, where each item is colored by the sum of the post-normalized columns (use --normalization NoneNorm for UMI counts, default), and DrawItem DrawDiversity, where each node is colored by the diversity based on the labels of each item and the color is normalized separately for the leaves and the inner nodes. The default is DrawText, unless --labels-file is provided, in which DrawItem DrawLabel is the default."
    , drawCollection :: Maybe String <?> "([PieRing] | PieChart | PieNone | CollectionGraph MAXWEIGHT THRESHOLD [NODE]) How to draw item leaves in the dendrogram. PieRing draws a pie chart ring around the items. PieChart only draws a pie chart instead of items. PieNone only draws items, no pie rings or charts. (CollectionGraph MAXWEIGHT THRESHOLD [NODE]) draws the nodes and edges within leaves that are descendents of NODE (empty list [] indicates draw all leaf networks) based on the input matrix, normalizes edges based on the MAXWEIGHT, and removes edges for display less than THRESHOLD (after normalization, so for CollectionGraph 2 0.5 [26], draw the leaf graphs for all leaves under node 26, then a edge of 0.7 would be removed because (0.7 / 2) < 0.5)."
    , drawMark :: Maybe String <?> "([MarkNone] | MarkModularity) How to draw annotations around each inner node in the tree. MarkNone draws nothing and MarkModularity draws a black circle representing the modularity at that node, darker black means higher modularity for that next split."
    , drawNodeNumber :: Bool <?> "Draw the node numbers on top of each node in the graph."
    , drawMaxNodeSize :: Maybe Double <?> "([72] | DOUBLE) The max node size when drawing the graph. 36 is the theoretical default, but here 72 makes for thicker branches."
    , drawNoScaleNodes :: Bool <?> "Do not scale inner node size when drawing the graph. Instead, uses draw-max-node-size as the size of each node and is highly recommended to change as the default may be too large for this option."
    , drawColors :: Maybe String <?> "([Nothing] | COLORS) Custom colors for the labels. Will repeat if more labels than provided colors. For instance: --draw-colors \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\""
    , interactive :: Bool <?> "Display interactive tree."
    } deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "inputMatrix"          = Just 'X'
    short "minSize"              = Just 'M'
    short "maxStep"              = Just 'S'
    short "maxProportion"        = Just 'P'
    short "maxDistance"          = Just 'T'
    short "drawLeaf"             = Just 'L'
    short "drawCollection"       = Just 'D'
    short "drawNodeNumber"       = Just 'N'
    short "drawMark"             = Just 'K'
    short "drawColors"           = Just 'R'
    short "drawNoScaleNodes"     = Just 'O'
    short "drawMaxNodeSize"      = Just 'A'
    short "interactive"          = Just 'I'
    short x                      = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

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
        smartCutoff'      = fmap SmartCutoff . unHelpful . smartCutoff $ opts
        order'            = fmap Order . unHelpful . order $ opts
        drawLeaf'         =
            maybe (maybe DrawText (const (DrawItem DrawLabel)) labelsFile') read
                . unHelpful
                . drawLeaf
                $ opts
        drawCollection'   = maybe PieRing read . unHelpful . drawCollection $ opts
        drawMark'         = maybe MarkNone read . unHelpful . drawMark $ opts
        drawNodeNumber'   = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        drawMaxNodeSize'  =
            DrawMaxNodeSize . fromMaybe 72 . unHelpful . drawMaxNodeSize $ opts
        drawNoScaleNodes' =
            DrawNoScaleNodesFlag . unHelpful . drawNoScaleNodes $ opts
        drawColors'       = fmap ( CustomColors
                                 . fmap sRGB24read
                                 . (\x -> read x :: [String])
                                 )
                          . unHelpful
                          . drawColors
                          $ opts
        output'           =
            fromMaybe "dendrogram.svg" . unHelpful . output $ opts

    dend <- loadDend input'

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
        config = Config { _birchLabelMap         = labelMap
                        , _birchMinSize          = minSize'
                        , _birchMaxStep          = maxStep'
                        , _birchMaxProportion    = maxProportion'
                        , _birchMinDistance      = minDistance'
                        , _birchSmartCutoff      = smartCutoff'
                        , _birchOrder            = order'
                        , _birchDrawLeaf         = drawLeaf'
                        , _birchDrawCollection   = drawCollection'
                        , _birchDrawMark         = drawMark'
                        , _birchDrawNodeNumber   = drawNodeNumber'
                        , _birchDrawMaxNodeSize  = drawMaxNodeSize'
                        , _birchDrawNoScaleNodes = drawNoScaleNodes'
                        , _birchDrawColors       = drawColors'
                        , _birchDend             = dend
                        , _birchMat              = Nothing
                        , _birchSimMat           = simMat
                        }

    (plot, _, _, _, _, _) <- mainDiagram config

    D.renderCairo
            output'
            (D.mkHeight 1000)
            plot

    when (unHelpful . interactive $ opts) $ interactiveDiagram dend labelMap (Nothing :: Maybe NamedMatrix) simMat

    return ()
