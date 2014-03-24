{-# LANGUAGE FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.PuzzleDraw (
    drawPuzzle
    ) where

import Data.Puzzles.Read (
        TypedPuzzle, puzzleType, dropType, RawPuzzle(..),
        ParsePuzzle
    )
import Diagrams.TwoD.Puzzles.Puzzle (RenderPuzzle)
import qualified Data.Puzzles.Read as R
import qualified Diagrams.TwoD.Puzzles.Puzzle as D
import Diagrams.Prelude
import Data.Yaml (Parser)

rd :: ParsePuzzle p q -> RenderPuzzle b p q ->
      RawPuzzle -> Parser (Diagram b R2, Diagram b R2)
rd (pp, ps) (dp, ds) (RP p s) = do
    p' <- pp p
    s' <- ps s
    return (dp p', ds (p', s'))

rdtype :: (Backend b R2, Renderable (Path R2) b) =>
          String -> RawPuzzle -> Parser (Diagram b R2, Diagram b R2)
rdtype "lits"                 = rd R.lits                D.lits
rdtype "litsplus"             = rd R.litsplus            D.litsplus
rdtype "geradeweg"            = rd R.geradeweg           D.geradeweg
rdtype "fillomino"            = rd R.fillomino           D.fillomino
rdtype "masyu"                = rd R.masyu               D.masyu
rdtype "nurikabe"             = rd R.nurikabe            D.nurikabe
rdtype "latintapa"            = rd R.latintapa           D.latintapa
rdtype "sudoku"               = rd R.sudoku              D.sudoku
rdtype "thermosudoku"         = rd R.thermosudoku        D.thermosudoku
rdtype "pyramid"              = rd R.pyramid             D.pyramid
rdtype "rowkropkipyramid"     = rd R.kpyramid            D.kpyramid
rdtype "slitherlink"          = rd R.slither             D.slither
rdtype "slitherlinkliar"      = rd R.liarslither         D.liarslither
rdtype "skyscrapers-tightfit" = rd R.tightfitskyscrapers D.tightfitskyscrapers
rdtype "wordloop"             = rd R.wordloop            D.wordloop
rdtype "wordsearch"           = rd R.wordsearch          D.wordsearch
rdtype "curvedata"            = rd R.curvedata           D.curvedata
rdtype "doubleback"           = rd R.doubleback          D.doubleback
rdtype "slalom"               = rd R.slalom              D.slalom
rdtype "compass"              = rd R.compass             D.compass
rdtype t                      = fail $ "unknown puzzle type: " ++ t

drawPuzzle :: (Backend b R2, Renderable (Path R2) b) =>
              TypedPuzzle -> Parser (Diagram b R2, Diagram b R2)
drawPuzzle p = rdtype (puzzleType p) (dropType p)
