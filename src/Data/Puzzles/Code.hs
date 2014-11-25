module Data.Puzzles.Code where

type Code = [CodePart]

data CodePart =
      Rows'  [Int] -- ^ Rows of cells, counted from the bottom.
    | Cols   [Int] -- ^ Cols of cells, counted from the left.
    | RowsN' [Int] -- ^ Rows of nodes, counted from the bottom.
    | ColsN  [Int] -- ^ Cols of nodes, counted from the left.
