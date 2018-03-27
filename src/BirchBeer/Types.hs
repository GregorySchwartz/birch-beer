{- BirchBeer.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module BirchBeer.Types where

-- Remote
import Data.Colour (AlphaColour)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (Colour (..), RGB (..), toSRGB)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S

-- Local

-- Basic
newtype Label = Label
    { unLabel :: Text
    } deriving (Eq,Ord,Read,Show)
newtype Id = Id
    { unId :: Text
    } deriving (Eq,Ord,Read,Show)
newtype Feature = Feature
    { unFeature :: Text
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
newtype DrawMaxNodeSize = DrawMaxNodeSize
    { unDrawMaxNodeSize :: Double
    } deriving (Read,Show)
newtype DrawNodeNumber = DrawNodeNumber
    { unDrawNodeNumber :: Bool
    } deriving (Read,Show)
newtype IsLeaf = IsLeaf {unIsLeaf :: Bool} deriving (Eq, Ord, Read, Show)
newtype DrawNoScaleNodesFlag = DrawNoScaleNodesFlag
    { unDrawNoScaleNodesFlag :: Bool
    } deriving (Read,Show)
newtype LabelMap = LabelMap
    { unLabelMap :: Map Id Label
    } deriving (Read,Show)
newtype LabelColorMap = LabelColorMap
    { unLabelColorMap :: Map Label Kolor
    } deriving (Read,Show)
newtype ItemColorMap = ItemColorMap
    { unItemColorMap :: Map Id Kolor
    } deriving (Read,Show)
newtype MarkColorMap = MarkColorMap
    { unMarkColorMap :: Map G.Node (AlphaColour Double)
    } deriving (Read,Show)
newtype ClusterGraph a = ClusterGraph
    { unClusterGraph :: G.Gr (G.Node, Maybe (Seq.Seq a)) Double
    } deriving (Read, Show)
newtype CustomColors = CustomColors
    { unCustomColors :: [Kolor]
    } deriving (Read, Show)
newtype L = L Double
newtype C = C Double
newtype H = H Double
newtype U = U Double
newtype V = V Double

-- Advanced
data DrawItemType
    = DrawLabel
    | DrawContinuous Text
    | DrawThresholdContinuous [(Text, Double)]
    | DrawSumContinuous
    deriving (Read,Show)
data DrawLeaf = DrawItem DrawItemType | DrawText deriving (Read, Show)
data DrawPie  = PieRing | PieChart | PieNone deriving (Read, Show)
data DrawNodeMark = MarkModularity | MarkNone deriving (Read, Show)

data Palette = Set1 | Hcl

data DrawConfig = DrawConfig
    { _drawLeaf :: DrawLeaf
    , _drawPie :: DrawPie
    , _drawNodeNumber :: DrawNodeNumber
    , _drawMaxNodeSize :: DrawMaxNodeSize
    , _drawNoScaleNodesFlag :: DrawNoScaleNodesFlag
    } deriving (Read,Show)

class TreeItem a where
    getId :: a -> Id

class MatrixLike a where
    getMatrix   :: a -> S.SpMatrix Double
    getRowNames :: a -> Vector Text
    getColNames :: a -> Vector Text

deriving instance (Read a) => Read (HC.Dendrogram a)
deriving instance (Generic a) => Generic (HC.Dendrogram a)

instance (A.ToJSON a, Generic a) => A.ToJSON (HC.Dendrogram a) where
    toEncoding = A.genericToEncoding A.defaultOptions
instance (A.FromJSON a, Generic a) => A.FromJSON (HC.Dendrogram a)

instance (Ord a, Floating a) => Ord (Colour a) where
    compare x y = ((\a -> (channelRed a, channelGreen a, channelBlue a)) . toSRGB $ y)
        `compare` ((\a -> (channelRed a, channelGreen a, channelBlue a)) . toSRGB $ x)
