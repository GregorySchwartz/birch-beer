{- BirchBeer.Options
Gregory W. Schwartz

The options type for the program.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module BirchBeer.Options where

-- Remote
import Options.Generic

-- | Command line arguments
data Options = Options
    { input :: String <?> "(FILE) The input JSON file."
    , inputMatrix :: Maybe String <?> "([Nothing] | FILE) The input adjacency matrix file for CollectionGraph (matrix market format if ends in .mtx, \"i,j,value\" without header otherwise and text labels will be sorted when converting indices)."
    , output :: Maybe String <?> "([dendrogram.svg] | FILE) The filename for the dendrogram. Supported formats are PNG, PS, PDF, and SVG."
    , jsonOutput :: Maybe String <?> "([Nothing] | FILE) The filename for the output json tree. The input tree can change based on pruning, so this option provides a way to output the new tree as a json."
    , csvOutput :: Maybe String <?> "([Nothing] | FILE) The filename for the output csv containing node assignment for each item. The input tree can change based on pruning, so this option provides a way to view the new assignments as a csv."
    , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for csv files."
    , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each item, with \"item,label\" header."
    , minSize :: Maybe Int <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
    , maxStep :: Maybe Int <?> "([Nothing] | INT) Only keep clusters that are INT steps from the root. Defaults to all steps."
    , maxProportion :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE proportion split. So a node N with L and R children will stop with this criteria at 0.5 if |L| / |R| < 0.5 or > 2 (absolute log2 transformed), that is, if one child has over twice as many items as the other child. Includes L and R in the final result."
    , minDistance :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE distance. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Includes L and R in the final result."
    , minDistanceSearch :: Maybe Double <?> "([Nothing] | DOUBLE) Similar to --min-distance, but searches from the leaves to the root -- if a path from a subtree contains a distance of at least DOUBLE, keep that path, otherwise prune it. This argument assists in finding distant nodes."
    , smartCutoff :: Maybe Double <?> "([Nothing] | DOUBLE) Whether to set the cutoffs for --min-size, --max-proportion, --min-distance, and --min-distance-search based off of the distributions (median + (DOUBLE * MAD)) of all nodes. To use smart cutoffs, use this argument and then set one of the three arguments to an arbitrary number, whichever cutoff type you want to use. --max-proportion and --min-size distributions are log2 transformed."
    , elbowCutoff :: Maybe String <?> "(Max | Min) Whether to set the cutoffs for --min-size, --max-proportion, --min-distance, and --min-distance-search based off of the elbow point of distributions of all nodes. For a distribution in positive x and y on a graph, the top left hump would be Max and the bottom right dip would be Min. To use elbow cutoffs, use this argument and then set one of the three arguments to an arbitrary number, whichever cutoff type you want to use. --max-proportion and --min-size distributions are log2 transformed. Conflicts with --smart-cutoff, so this argument takes precedent."
    , customCut :: [Int] <?> "([Nothing] | NODE) List of nodes to prune (make these nodes leaves). Invoked by --custom-cut 34 --custom-cut 65 etc."
    , rootCut :: Maybe Int <?> "([Nothing] | NODE) Assign a new root to the tree, removing all nodes outside of the subtree."
    , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity for DrawItem DrawDiversity."
    , drawLeaf :: Maybe String <?> "([DrawText] | DrawItem DrawItemType) How to draw leaves in the dendrogram. DrawText is the number of items in that leaf. DrawItem is the collection of items represented by circles, consisting of: DrawItem DrawLabel, where each item is colored by its label, DrawItem (DrawContinuous [FEATURE]), where each item is colored by the expression of FEATURE (corresponding to a feature name in the input matrix, [FEATURE] is a list, so if more than one FEATURE is listed, uses the average of the feature values), DrawItem (DrawThresholdContinuous [(FEATURE, THRESHOLD)]), where each item is colored by the binary high / low expression of FEATURE based on THRESHOLD (either `Exact DOUBLE` or `MadMedian DOUBLE`, where Exact just uses the DOUBLE as a cutoff value while MadMedian uses the DOUBLE as the number of MADs away from the median value of the feature) and multiple FEATUREs can be used to combinatorically label items (FEATURE1 high / FEATURE2 low, etc.), DrawItem DrawSumContinuous, where each item is colored by the sum of the post-normalized columns (use --normalization NoneNorm for UMI counts, default), DrawItem (DrawProximity ([NODE], DISTANCE)), where each item is colored by its proximity in Euclidean distance to a set of items drawn from a list of NODE, and DrawItem DrawDiversity, where each node is colored by the diversity based on the labels of each item and the color is normalized separately for the leaves and the inner nodes. The default is DrawText, unless --labels-file is provided, in which DrawItem DrawLabel is the default. If the label or feature cannot be found, the default color will be black (check your spelling!)."
    , drawCollection :: Maybe String <?> "([PieChart] | PieRing | IndividualItems | Histogram | NoLeaf | CollectionGraph MAXWEIGHT THRESHOLD [NODE]) How to draw item leaves in the dendrogram. PieRing draws a pie chart ring around the items. PieChart only draws a pie chart instead of items. IndividualItems only draws items, no pie rings or charts. Histogram plots a histogram of the features requested. NoLeaf has no leaf, useful if there are so many items the tree takes very long to render. (CollectionGraph MAXWEIGHT THRESHOLD [NODE]) draws the nodes and edges within leaves that are descendents of NODE (empty list [] indicates draw all leaf networks) based on the input matrix, normalizes edges based on the MAXWEIGHT, and removes edges for display less than THRESHOLD (after normalization, so for CollectionGraph 2 0.5 [26], draw the leaf graphs for all leaves under node 26, then a edge of 0.7 would be removed because (0.7 / 2) < 0.5). For CollectionGraph with no colors, use --draw-leaf \"DrawItem DrawLabel\" and all nodes will be black. If you don't specify this option, DrawText from --draw-leaf overrides this argument and only the number of cells will be plotted."
    , drawMark :: Maybe String <?> "([MarkNone] | MarkModularity | MarkSignificance ) How to draw annotations around each inner node in the tree. MarkNone draws nothing and MarkModularity draws a black circle representing the modularity at that node, darker black means higher modularity for that next split. MarkSignificance is for significance, i.e. p-value, darker means higher value."
    , drawNodeNumber :: Bool <?> "Draw the node numbers on top of each node in the graph."
    , drawMaxNodeSize :: Maybe Double <?> "([72] | DOUBLE) The max node size when drawing the graph. 36 is the theoretical default, but here 72 makes for thicker branches."
    , drawMaxLeafNodeSize :: Maybe Double <?> "([--draw-max-node-size] | DOUBLE) The max leaf node size when drawing the graph. Defaults to the value of --draw-max-node-size."
    , drawNoScaleNodes :: Bool <?> "Do not scale inner node size when drawing the graph. Instead, uses draw-max-node-size as the size of each node and is highly recommended to change as the default may be too large for this option."
    , drawLegendSep :: Maybe Double <?> "([1] | DOUBLE) The amount of space between the legend and the tree."
    , drawLegendAllLabels :: Bool <?> "Whether to show all the labels in the label file instead of only showing labels within the current tree. The program generates colors from all labels in the label file first in order to keep consistent colors. By default, this value is false, meaning that only the labels present in the tree are shown (even though the colors are the same). The subset process occurs after --draw-colors, so when using that argument make sure to account for all labels."
    , drawPalette :: Maybe String <?> "([Set1] | Hsv | Ryb) Palette to use for legend colors. With high saturation in --draw-scale-saturation, consider using Hsv to better differentiate colors."
    , drawColors :: Maybe String <?> "([Nothing] | COLORS) Custom colors for the labels or continuous features. Will repeat if more labels than provided colors. For continuous feature plots, uses first two colors [high, low], defaults to [red, gray]. For instance: --draw-colors \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\""
    , drawDiscretize :: Maybe String <?> "([Nothing] | COLORS | INT) Discretize colors by finding the nearest color for each item and node. For instance, --draw-discretize \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\" will change all node and item colors to one of those two colors, based on Euclidean distance. If using \"--draw-discretize INT\", will instead take the default map and segment (or interpolate) it into INT colors, rather than a more continuous color scheme. May have unintended results when used with --draw-scale-saturation."
    , drawScaleSaturation :: Maybe Double <?> "([Nothing] | DOUBLE) Multiply the saturation value all nodes by this number in the HSV model. Useful for seeing more visibly the continuous colors by making the colors deeper against a gray scale."
    , drawItemLineWeight :: Maybe Double <?> "([0.1] | DOUBLE) The line weight for items in the leaves if shown. Supplied as if there are too many items, the collection may look like a black box. Set to 0 to disable outlines of items to avoid this."
    , drawFont :: Maybe String <?> "([Arial] | FONT) Specify the font to use for the labels when plotting."
    , drawBarBounds :: Bool <?> "Whether to plot only the minimum and maximum ticks for the color bars."
    , interactive :: Bool <?> "Display interactive tree."
    } deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "customCut"            = Nothing
    short "csvOutput"            = Nothing
    short "drawCollection"       = Just 'D'
    short "drawColors"           = Just 'R'
    short "drawDiscretize"       = Nothing
    short "drawFont"             = Nothing
    short "drawLeaf"             = Just 'L'
    short "drawLegendAllLabels"  = Just 'J'
    short "drawLegendSep"        = Just 'Q'
    short "drawMark"             = Just 'K'
    short "drawMaxLeafNodeSize"  = Nothing
    short "drawMaxNodeSize"      = Just 'A'
    short "drawNoScaleNodes"     = Just 'W'
    short "drawNodeNumber"       = Just 'N'
    short "drawPalette"          = Just 'Y'
    short "drawScaleSaturation"  = Just 'V'
    short "drawBarBounds"        = Nothing
    short "elbowCut"             = Nothing
    short "inputMatrix"          = Just 'X'
    short "interactive"          = Just 'I'
    short "maxProportion"        = Just 'P'
    short "maxStep"              = Just 'S'
    short "minDistance"          = Just 'T'
    short "minDistanceSearch"    = Nothing
    short "minSize"              = Just 'M'
    short "order"                = Just 'O'
    short "rootCut"              = Nothing
    short x                      = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers
