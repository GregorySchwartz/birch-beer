{- BirchBeer.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module BirchBeer.Types where

-- Remote
import Control.DeepSeq
import Data.Colour (AlphaColour)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (Colour (..), RGB (..))
import Data.Colour.SRGB.Linear (toRGB)
import Data.Map.Strict (Map)
import Data.Tree (Tree (..))
import Diagrams.Backend.Cairo
import Diagrams.Prelude hiding (distance)
import GHC.Generics (Generic)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local

class TreeItem a where
    getId :: a -> Id

instance TreeItem T.Text where
    getId = Id

class MatrixLike a where
    getMatrix   :: a -> S.SpMatrix Double
    getRowNames :: a -> V.Vector T.Text
    getColNames :: a -> V.Vector T.Text

instance MatrixLike (S.SpMatrix Double) where
    getMatrix       = id
    getRowNames mat = V.fromList . fmap showt $ [1..S.nrows mat]
    getColNames mat = V.fromList . fmap showt $ [1..S.nrows mat]

instance MatrixLike NamedMatrix where
    getMatrix   = _namedMat
    getRowNames = _namedRows
    getColNames = _namedCols

-- instance Generic (Colour a)
-- instance NFData a => NFData (Colour a)
instance NFData (Colour a) where rnf x = seq x ()

deriving instance (Eq a, Ord a, Fractional a) => Ord (RGB a)

instance (Eq a, Ord a, Fractional a) => Ord (Colour a) where
    compare x y = toRGB y `compare` toRGB x

-- Basic
newtype Delimiter = Delimiter
    { unDelimiter :: Char
    } deriving (Eq,Ord,Read,Show)
newtype LabelFile = LabelFile
    { unLabelFile :: String
    } deriving (Eq,Ord,Read,Show)
newtype Label = Label
    { unLabel :: T.Text
    } deriving (Eq,Ord,Read,Show,Generic,NFData)
newtype Id = Id
    { unId :: T.Text
    } deriving (Eq,Ord,Read,Show)
newtype Feature = Feature
    { unFeature :: T.Text
    } deriving (Eq,Ord,Read,Show)
newtype Cluster = Cluster
    { unCluster :: Int
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype MinClusterSize = MinClusterSize
    { unMinClusterSize :: Int
    } deriving (Read,Show)
newtype MaxStep = MaxStep
    { unMaxStep :: Int
    } deriving (Read,Show)
newtype MaxProportion = MaxProportion
    { unMaxProportion :: Double
    } deriving (Read,Show)
newtype MinDistance = MinDistance
    { unMinDistance :: Double
    } deriving (Read,Show)
newtype CustomCut = CustomCut
    { unCustomCut :: Set.Set Int
    } deriving (Read,Show)
newtype SmartCutoff = SmartCutoff
    { unSmartCutoff :: Double
    } deriving (Read,Show)
newtype Order = Order
    { unOrder :: Double
    } deriving (Read,Show)
newtype EdgeThreshold = EdgeThreshold
    { unEdgeThreshold :: Double
    } deriving (Read,Show)
newtype MaxWeight = MaxWeight
    { unMaxWeight :: Double
    } deriving (Read,Show)
newtype LeafGraphNodes = LeafGraphNodes
    { unLeafGraphNodes :: Set.Set G.Node
    } deriving (Read,Show)
newtype DrawMaxNodeSize = DrawMaxNodeSize
    { unDrawMaxNodeSize :: Double
    } deriving (Read,Show)
newtype DrawMaxLeafNodeSize = DrawMaxLeafNodeSize
    { unDrawMaxLeafNodeSize :: Double
    } deriving (Read,Show)
newtype DrawNodeNumber = DrawNodeNumber
    { unDrawNodeNumber :: Bool
    } deriving (Read,Show)
newtype IsLeaf = IsLeaf {unIsLeaf :: Bool} deriving (Eq, Ord, Read, Show)
newtype DrawNoScaleNodesFlag = DrawNoScaleNodesFlag
    { unDrawNoScaleNodesFlag :: Bool
    } deriving (Read,Show)
newtype DrawLegendSep = DrawLegendSep
    { unDrawLegendSep :: Double
    } deriving (Read,Show)
newtype DrawLegendAllLabels = DrawLegendAllLabels
    { unDrawLegendAllLabels :: Bool
    } deriving (Read,Show)
newtype LabelMap = LabelMap
    { unLabelMap :: Map Id Label
    } deriving (Read,Show)
newtype LabelColorMap = LabelColorMap
    { unLabelColorMap :: Map Label Kolor
    } deriving (Read, Show, Generic, NFData)
newtype ItemColorMap = ItemColorMap
    { unItemColorMap :: Map Id Kolor
    } deriving (Read,Show)
newtype MarkColorMap = MarkColorMap
    { unMarkColorMap :: Map G.Node (AlphaColour Double)
    } deriving (Read,Show)
newtype NodeColorMap = NodeColorMap
    { unNodeColorMap :: Map G.Node Kolor
    } deriving (Read,Show)
newtype DrawScaleSaturation = DrawScaleSaturation
    { unDrawScaleSaturation :: Double
    } deriving (Read,Show)
newtype ClusterGraph a = ClusterGraph
    { unClusterGraph :: G.Gr (G.Node, Maybe (Seq.Seq a)) Double
    } deriving (Read, Show)
newtype LeafGraph a = LeafGraph
    { unLeafGraph :: G.Gr (G.Node, a) Double
    } deriving (Read, Show)
newtype LeafGraphMap a = LeafGraphMap
    { unLeafGraphMap :: Map G.Node (LeafGraph a)
    } deriving (Read, Show)
newtype LeafGraphDiaMap = LeafGraphDiaMap
    { unLeafGraphDiaMap :: Map G.Node (Diagram B) }
newtype CustomColors = CustomColors
    { unCustomColors :: [Kolor]
    } deriving (Read, Show)
newtype L = L Double
newtype C = C Double
newtype H = H Double
newtype U = U Double
newtype V = V Double

data (MatrixLike a) => SimMatrix a = SimilarityMatrix a | B2Matrix a

-- Advanced
data DrawItemType
    = DrawLabel
    | DrawContinuous [T.Text]
    | DrawThresholdContinuous [(T.Text, Double)]
    | DrawSumContinuous
    | DrawDiversity
    deriving (Read,Show)
data DrawLeaf       = DrawItem DrawItemType | DrawText deriving (Read, Show)
data DrawCollection = CollectionGraph Double Double [Int] | PieRing | PieChart | PieNone deriving (Read, Show)
data DrawNodeMark   = MarkModularity | MarkNone deriving (Read, Show)

data Palette = Set1 | Ryb | Hsv | Hcl deriving (Read, Show)

data DrawConfig = DrawConfig
    { _drawLeaf             :: DrawLeaf
    , _drawCollection       :: DrawCollection
    , _drawNodeNumber       :: DrawNodeNumber
    , _drawMaxNodeSize      :: DrawMaxNodeSize
    , _drawMaxLeafNodeSize  :: DrawMaxLeafNodeSize
    , _drawNoScaleNodesFlag :: DrawNoScaleNodesFlag
    , _drawLegendSep        :: DrawLegendSep
    } deriving (Read,Show)

data Config a b = Config
    { _birchLabelMap            :: Maybe LabelMap
    , _birchSmartCutoff         :: Maybe SmartCutoff
    , _birchCustomCut           :: CustomCut
    , _birchMinSize             :: Maybe MinClusterSize
    , _birchMaxStep             :: Maybe MaxStep
    , _birchMaxProportion       :: Maybe MaxProportion
    , _birchMinDistance         :: Maybe MinDistance
    , _birchOrder               :: Maybe Order
    , _birchDrawLeaf            :: DrawLeaf
    , _birchDrawCollection      :: DrawCollection
    , _birchDrawMark            :: DrawNodeMark
    , _birchDrawNodeNumber      :: DrawNodeNumber
    , _birchDrawMaxNodeSize     :: DrawMaxNodeSize
    , _birchDrawMaxLeafNodeSize :: DrawMaxLeafNodeSize
    , _birchDrawNoScaleNodes    :: DrawNoScaleNodesFlag
    , _birchDrawLegendAllLabels :: DrawLegendAllLabels
    , _birchDrawLegendSep       :: DrawLegendSep
    , _birchDrawPalette         :: Palette
    , _birchDrawColors          :: Maybe CustomColors
    , _birchDrawScaleSaturation :: Maybe DrawScaleSaturation
    , _birchTree                :: Tree (TreeNode (V.Vector a))
    , _birchMat                 :: Maybe b
    , _birchSimMat              :: Maybe (SimMatrix b)
    }

data NamedMatrix = NamedMatrix
    { _namedMat :: S.SpMatrix Double
    , _namedRows :: V.Vector T.Text
    , _namedCols :: V.Vector T.Text
    }

data TreeNode a = TreeNode
    { _distance :: Maybe Double
    , _item :: Maybe a
    } deriving (A.FromJSON, A.ToJSON, Generic, Read, Show)
makeLenses ''TreeNode

instance (Semigroup a) => Semigroup (TreeNode a) where
  (<>) x y  = TreeNode { _distance = L.view distance x
                       , _item = (<>) (L.view item x) (L.view item y)
                       }

instance (Semigroup a) => Monoid (TreeNode a) where
  mempty = TreeNode { _distance = Nothing, _item = mempty }
