{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- |
-- List of specific puzzle types.

module Data.Puzzles.PuzzleTypes (
    PuzzleType(..),
    lookupType,
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
                | CountNumbers
                | Tapa
                | JapaneseSums
                | Coral
                | MaximalLengths
                | PrimePlace
                | Labyrinth
                | Bahnhof
                | BlackoutDominos
                | TwilightTapa
                | TapaCave
                | DominoPillen
                | SlitherLinkMulti
                | AngleLoop
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
            , (Tapa, "tapa")
            , (JapaneseSums, "japanesesums")
            , (Coral, "coral")
            , (MaximalLengths, "maximallengths")
            , (PrimePlace, "primeplace")
            , (Labyrinth, "magiclabyrinth")
            , (Bahnhof, "bahnhof")
            , (BlackoutDominos, "blackout-dominos")
            , (TwilightTapa, "twilight-tapa")
            , (TapaCave, "tapa-cave")
            , (DominoPillen, "domino-pillen")
            , (SlitherLinkMulti, "slitherlink-multi")
            , (AngleLoop, "angleloop")
            ]

-- | Look up a puzzle type by name.
lookupType :: String -> Maybe PuzzleType
lookupType t = lookup t (map swap typeNames)
