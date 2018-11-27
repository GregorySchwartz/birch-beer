{- BirchBeer.Load
Gregory W. Schwartz

Collects the functions pertaining to loading labels for the tree.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module BirchBeer.Load
    ( loadLabelData
    , loadDendFromFile
    , loadTreeFromFile
    , loadTreeOrDendFromFile
    , loadMatrix
    ) where

-- Remote
import Control.Applicative ((<|>))
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix, Matrix(RMatrix, IntMatrix), Structure (..))
import Data.Scientific (toRealFloat, Scientific)
import Data.Tree (Tree (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Csv as CSV
import qualified Data.Map as Map
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Math.Clustering.Hierarchical.Spectral.Types ()

-- Local
import BirchBeer.Types
import BirchBeer.Utility

-- | Load a CSV containing the label of each cell.
loadLabelData :: Delimiter -> LabelFile -> IO LabelMap
loadLabelData (Delimiter delim) (LabelFile file) = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    rows <- fmap (\ x -> either error snd ( CSV.decodeByNameWith csvOpts x
                                        :: Either String (CSV.Header, V.Vector (Map.Map T.Text T.Text))
                                         )
                 )
          . B.readFile
          $ file

    let toLabelMap :: Map.Map T.Text T.Text -> Map.Map Id Label
        toLabelMap m =
            Map.singleton
                (Id $ Map.findWithDefault (error "No \"item\" column in label file.") "item" m)
                (Label $ Map.findWithDefault (error "No \"label\" column in label file.") "label" m)

    return . LabelMap . Map.unions . fmap toLabelMap . V.toList $ rows

-- | Load a dendrogram from a file.
loadDendFromFile :: FilePath -> IO (Either String (HC.Dendrogram (V.Vector T.Text)))
loadDendFromFile file = fmap A.eitherDecode . B.readFile $ file

-- | Load a tree from a file.
loadTreeFromFile :: FilePath -> IO (Either String (Tree (TreeNode (V.Vector T.Text))))
loadTreeFromFile file = fmap A.eitherDecode . B.readFile $ file

-- | Load a format for the tree from a file.
loadTreeOrDendFromFile :: FilePath -> IO (Tree (TreeNode (V.Vector T.Text)))
loadTreeOrDendFromFile file = do
  dend <- (fmap . fmap) dendToTree $ loadDendFromFile file
  tree <- loadTreeFromFile file

  return . either error id $ dend <> tree

-- | Convert a Matrix to a sparse matrix.
matToSpMat :: Matrix Scientific -> S.SpMatrix Double
matToSpMat (RMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, toRealFloat z))
        $ xs
matToSpMat (IntMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, fromIntegral z))
        $ xs
matToSpMat _ = error "Input matrix is not a Real matrix."

loadMatrix :: FilePath -> IO (S.SpMatrix Double)
loadMatrix = fmap matToSpMat . readMatrix
