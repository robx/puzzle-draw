{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Puzzle
import Diagrams.Backend.Cairo.CmdLineSized

import Data.Puzzles.Grid
import Data.Puzzles.Puzzle

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import System.FilePath

import qualified Data.Yaml as Y
import Data.Aeson (Result(..))

import Data.Puzzles.Grid
import Diagrams.TwoD.Puzzles.Draw

fromResult (Success r) = r

drawPuzzleGen p parse dp ds = M $ drawExample (fromResult . parse $ p) dp ds

drawPuzzle :: Puzzle -> M
drawPuzzle p = case puzzleType p of
    "lits" -> drawPuzzleGen p parseLITS drawLITS drawLITSsol
    "litsplus" -> drawPuzzleGen p parseLITSPlus drawLITS drawLITSsol
    "geradeweg" -> drawPuzzleGen p parseGeradeweg drawGeradeweg drawGeradewegsol
    "fillomino" -> drawPuzzleGen p parseFillomino drawFillomino drawFillominosol

readDrawPuzzle :: FilePath -> IO M
readDrawPuzzle fp = do
    Just p <- Y.decodeFile fp
    return (drawPuzzle p)

main = mainWith readDrawPuzzle
