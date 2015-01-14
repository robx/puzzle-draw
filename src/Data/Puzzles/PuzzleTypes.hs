{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- |
-- List of specific puzzle types.

module Data.Puzzles.PuzzleTypes (
    PuzzleType(..)
  , lookupType
  , typeNames
  ) where

import Data.Tuple (swap)

-- | The list of specific puzzle types we can handle.
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
                | MeanderingNumbers
                | Tapa
                | JapaneseSums
                | Coral
                | MaximalLengths
                | PrimePlace
                | Labyrinth
                | Bahnhof
                | Cave
                | AngleLoop
                | Shikaku
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
            , (MeanderingNumbers, "meanderingnumbers")
            , (Tapa, "tapa")
            , (JapaneseSums, "japanesesums")
            , (Coral, "coral")
            , (MaximalLengths, "maximallengths")
            , (PrimePlace, "primeplace")
            , (Labyrinth, "magiclabyrinth")
            , (Bahnhof, "bahnhof")
            , (Cave, "cave")
            , (AngleLoop, "angleloop")
            , (Shikaku, "shikaku")
            ]

-- | Look up a puzzle type by name.
lookupType :: String -> Maybe PuzzleType
lookupType t = lookup t (map swap typeNames)
