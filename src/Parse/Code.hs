{-# LANGUAGE OverloadedStrings #-}

module Parse.Code where

import Data.Code
import Parse.Util

import Data.Yaml

import Data.Maybe (catMaybes)

parseCode :: Value -> Parser [CodePart]
parseCode (Object v) = fmap catMaybes . sequenceA $
    [ fmap Rows'   <$> v .:? "cell_rows_bottom"
    , fmap Cols    <$> v .:? "cell_cols"
    , fmap RowsN'  <$> v .:? "node_rows_bottom"
    , fmap ColsN   <$> v .:? "node_cols"
    , fmap (LabelsN . fmap (fmap unAlpha . blankToMaybe)) <$> (do
        v' <- v .:? "node_labels"
        sequenceA (parseGrid <$> v'))
    , fmap LRows'   <$> (v .:? "cell_rows_bottom_labeled" >>= traverse parseCharMap)
    , fmap LCols    <$> (v .:? "cell_cols_labeled" >>= traverse parseCharMap)
    , fmap LRowsN'  <$> (v .:? "node_rows_bottom_labeled" >>= traverse parseCharMap)
    , fmap LColsN   <$> (v .:? "node_cols_labeled" >>= traverse parseCharMap)
    ] 
parseCode _          = fail "expected object"
