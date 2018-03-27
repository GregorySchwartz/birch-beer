{- BirchBeer.Load
Gregory W. Schwartz

Collects the functions pertaining to loading labels for the tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module BirchBeer.Load
    ( loadLabelData
    ) where

-- Remote
import Data.Char (ord)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

-- Local
import BirchBeer.File.Types

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
                (Id $ Map.findWithDefault (error "No \"item\" column in label file.") "label" m)
                (Label $ Map.findWithDefault (error "No \"label\" column in label file.") "label" m)

    return . LabelMap . Map.unions . fmap toLabelMap . V.toList $ rows
