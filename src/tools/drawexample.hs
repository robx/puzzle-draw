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

drawPuzzle :: Puzzle -> (Diagram B R2, Diagram B R2)
drawPuzzle p = case puzzleType p of
    "lits" ->      f p parseLITS drawLITS drawLITSsol
    "litsplus" ->  f p parseLITSPlus drawLITS drawLITSsol
    "geradeweg" -> f p parseGeradeweg drawGeradeweg drawGeradewegsol
    "fillomino" -> f p parseFillomino drawFillomino drawFillominosol
    where f q parse draw drawsol = let Success x = parse q in (draw x, drawsol x)

readDrawPuzzle :: FilePath -> IO M
readDrawPuzzle fp = do
    Just p <- Y.decodeFile fp
    return . M . uncurry drawExample . drawPuzzle $ p

main = mainWith readDrawPuzzle
