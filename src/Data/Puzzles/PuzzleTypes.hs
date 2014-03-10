module Data.Puzzles.PuzzleTypes where

import Data.Puzzles.Grid
import Data.Puzzles.Read
import Data.Puzzles.Things
import qualified Data.Puzzles.Pyramid as Pyr

data PuzzleDef a b = PD { pzl :: a
                        , sol :: b
                        }
    deriving Show

type LITS = PuzzleDef AreaGrid ShadedGrid
type Geradeweg = PuzzleDef IntGrid Loop
type Fillomino = PuzzleDef IntGrid IntGrid
type Masyu = PuzzleDef MasyuGrid Loop
type Nurikabe = PuzzleDef IntGrid ShadedGrid
type LatinTapa = PuzzleDef (Grid (Clue [String])) CharClueGrid
type Sudoku = PuzzleDef IntGrid IntGrid
type ThermoSudoku = PuzzleDef (IntGrid, [Thermometer]) IntGrid
type Pyramid = PuzzleDef Pyr.Pyramid Pyr.Pyramid
type RowKropkiPyramid = PuzzleDef Pyr.RowKropkiPyramid Pyr.Pyramid
type SlitherLink = PuzzleDef IntGrid Loop
type LiarSlitherLink = PuzzleDef IntGrid (Loop, Grid (Maybe ()))
type TightfitSkyscrapers = PuzzleDef
    (OutsideClues (Maybe Int), Grid (Tightfit ()))
    (Grid (Tightfit Int))
type Wordloop = PuzzleDef (CharClueGrid, [String]) CharClueGrid
type Wordsearch = PuzzleDef
    (CharClueGrid, [String])
    (CharClueGrid, [MarkedWord])
type CurveData = PuzzleDef (Grid (Maybe [Edge])) [Edge]
type DoubleBack = PuzzleDef AreaGrid Loop
type Slalom = PuzzleDef IntGrid CharGrid
type Compass = PuzzleDef (Grid CompassClue) AreaGrid