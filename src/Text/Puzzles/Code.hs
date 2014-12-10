{-# LANGUAGE OverloadedStrings #-}

module Text.Puzzles.Code where

import Data.Puzzles.Code
import Text.Puzzles.Util

import Data.Yaml

import Control.Applicative
import Data.Traversable (sequenceA)
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
    ] 
parseCode _          = fail "expected object"
