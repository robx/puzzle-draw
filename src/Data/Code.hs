module Data.Code where

import Data.Grid
import Data.GridShape
import Data.Map.Strict (Map)

type Code = [CodePart]

data CodePart
  = -- | Rows of cells, counted from the bottom.
    Rows' [Int]
  | -- | Cols of cells, counted from the left.
    Cols [Int]
  | -- | Rows of nodes, counted from the bottom.
    RowsN' [Int]
  | -- | Cols of nodes, counted from the left.
    ColsN [Int]
  | -- | Nodes, labeld by letters.
    LabelsN (Grid N (Maybe Char))
  | -- | Rows of cells, counted from the bottom.
    LRows' (Map Char Int)
  | -- | Cols of cells, counted from the left.
    LCols (Map Char Int)
  | -- | Rows of nodes, counted from the bottom.
    LRowsN' (Map Char Int)
  | -- | Cols of nodes, counted from the left.
    LColsN (Map Char Int)
