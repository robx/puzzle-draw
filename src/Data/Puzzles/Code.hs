module Data.Puzzles.Code where

import Data.Puzzles.Grid
import Data.Puzzles.GridShape

type Code = [CodePart]

data CodePart =
      Rows'  [Int] -- ^ Rows of cells, counted from the bottom.
    | Cols   [Int] -- ^ Cols of cells, counted from the left.
    | RowsN' [Int] -- ^ Rows of nodes, counted from the bottom.
    | ColsN  [Int] -- ^ Cols of nodes, counted from the left.
    | LabelsN (Grid N (Maybe Char)) -- ^ Nodes, labeld by letters.
