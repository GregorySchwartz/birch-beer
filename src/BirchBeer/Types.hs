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
import GHC.Generics (Generic)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Types as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local

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
    } deriving (Read, Show, Generic, NFData)
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
    | DrawContinuous T.Text
    | DrawThresholdContinuous [(T.Text, Double)]
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

data Config a b = Config
    { _birchLabelMap         :: Maybe LabelMap
    , _birchMinStep          :: Maybe MinClusterSize
    , _birchMaxStep          :: Maybe MaxStep
    , _birchDrawLeaf         :: DrawLeaf
    , _birchDrawPie          :: DrawPie
    , _birchDrawMark         :: DrawNodeMark
    , _birchDrawNodeNumber   :: DrawNodeNumber
    , _birchDrawMaxNodeSize  :: DrawMaxNodeSize
    , _birchDrawNoScaleNodes :: DrawNoScaleNodesFlag
    , _birchDrawColors       :: Maybe CustomColors
    , _birchDend             :: HC.Dendrogram (V.Vector a)
    , _birchMat              :: Maybe b
    } deriving (Read, Show)

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
    getRowNames mat = V.fromList . fmap showt $ [0..S.nrows mat - 1]
    getColNames mat = V.fromList . fmap showt $ [0..S.nrows mat - 1]

deriving instance (Read a) => Read (HC.Dendrogram a)
deriving instance Generic (HC.Dendrogram a)

-- instance Generic (Colour a)
-- instance NFData a => NFData (Colour a)
instance NFData (Colour a) where rnf x = seq x ()

instance (A.ToJSON a) => A.ToJSON (HC.Dendrogram a) where
    toEncoding = A.genericToEncoding A.defaultOptions
instance (A.FromJSON a) => A.FromJSON (HC.Dendrogram a)

deriving instance (Eq a, Ord a, Fractional a) => Ord (RGB a)

instance (Eq a, Ord a, Fractional a) => Ord (Colour a) where
    compare x y = toRGB y `compare` toRGB x
