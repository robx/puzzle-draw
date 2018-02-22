module Data.Code where

import qualified Data.Map as M

import Data.Grid
import Data.GridShape

type Code = [CodePart]

data CodePart =
      Rows'  [Int] -- ^ Rows of cells, counted from the bottom.
    | Cols   [Int] -- ^ Cols of cells, counted from the left.
    | RowsN' [Int] -- ^ Rows of nodes, counted from the bottom.
    | ColsN  [Int] -- ^ Cols of nodes, counted from the left.
    | LabelsN (Grid N (Maybe Char)) -- ^ Nodes, labeld by letters.
    | LRows'  (M.Map Char Int) -- ^ Rows of cells, counted from the bottom.
    | LCols   (M.Map Char Int) -- ^ Cols of cells, counted from the left.
    | LRowsN' (M.Map Char Int) -- ^ Rows of nodes, counted from the bottom.
    | LColsN  (M.Map Char Int) -- ^ Cols of nodes, counted from the left.
