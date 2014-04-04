{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- |
-- Helpers to string together parser and renderer by puzzle type.

module Puzzles.PuzzleTypes (
    PuzzleType(..),
    lookupType,
  ) where

import Data.Tuple (swap)

data PuzzleType = LITS
                | LITSPlus
                | Geradeweg
                | Fillomino
                | Masyu
                | Nurikabe
                | LatinTapa
                | Sudoku
                | ThermoSudoku
                | Pyramid
                | RowKropkiPyramid
                | SlitherLink
                | SlitherLinkLiar
                | TightfitSkyscrapers
                | WordLoop
                | WordSearch
                | CurveData
                | DoubleBack
                | Slalom
                | Compass
                | BoxOf2Or3
                | AfternoonSkyscrapers
                | CountNumbers
    deriving (Show, Eq)

typeNames :: [(PuzzleType, String)]
typeNames = [ (LITS, "lits")
            , (LITSPlus, "litsplus")
            , (Geradeweg, "geradeweg")
            , (Fillomino, "fillomino")
            , (Masyu, "masyu")
            , (Nurikabe, "nurikabe")
            , (LatinTapa, "latintapa")
            , (Sudoku, "sudoku")
            , (ThermoSudoku, "thermosudoku")
            , (Pyramid, "pyramid")
            , (RowKropkiPyramid, "rowkropkipyramid")
            , (SlitherLink, "slitherlink")
            , (SlitherLinkLiar, "slitherlinkliar")
            , (TightfitSkyscrapers, "skyscrapers-tightfit")
            , (WordLoop, "wordloop")
            , (WordSearch, "wordsearch")
            , (CurveData, "curvedata")
            , (DoubleBack, "doubleback")
            , (Slalom, "slalom")
            , (Compass, "compass")
            , (BoxOf2Or3, "boxof2or3")
            , (AfternoonSkyscrapers, "afternoonskyscrapers")
            , (CountNumbers, "countnumbers")
            ]

lookupType :: String -> Maybe PuzzleType
lookupType t = lookup t (map swap typeNames)
