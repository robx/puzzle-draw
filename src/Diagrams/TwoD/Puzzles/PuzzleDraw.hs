{-# LANGUAGE FlexibleContexts, RankNTypes #-}

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
import Data.Yaml (Parser, Value)

type PuzzleHandler b a = forall p q. ParsePuzzle p q -> RenderPuzzle b p q -> a

--puzzle :: PuzzleHandler b (Value -> Parser (Diagram b R2))
--puzzle (pp, _) (dp, _) p = do
--    p' <- pp p
--    return $ dp p'

puzzleSol :: PuzzleHandler b ((Value, Value) -> Parser (Diagram b R2, Diagram b R2))
puzzleSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- ps s
    return (dp p', ds (p', s'))

join :: (Backend b R2, Renderable (Path R2) b) =>
        PuzzleHandler b a -> (String -> a) -> String -> a
join f _ "lits"                 = f R.lits                D.lits
join f _ "litsplus"             = f R.litsplus            D.litsplus
join f _ "geradeweg"            = f R.geradeweg           D.geradeweg
join f _ "fillomino"            = f R.fillomino           D.fillomino
join f _ "masyu"                = f R.masyu               D.masyu
join f _ "nurikabe"             = f R.nurikabe            D.nurikabe
join f _ "latintapa"            = f R.latintapa           D.latintapa
join f _ "sudoku"               = f R.sudoku              D.sudoku
join f _ "thermosudoku"         = f R.thermosudoku        D.thermosudoku
join f _ "pyramid"              = f R.pyramid             D.pyramid
join f _ "rowkropkipyramid"     = f R.kpyramid            D.kpyramid
join f _ "slitherlink"          = f R.slither             D.slither
join f _ "slitherlinkliar"      = f R.liarslither         D.liarslither
join f _ "skyscrapers-tightfit" = f R.tightfitskyscrapers D.tightfitskyscrapers
join f _ "wordloop"             = f R.wordloop            D.wordloop
join f _ "wordsearch"           = f R.wordsearch          D.wordsearch
join f _ "curvedata"            = f R.curvedata           D.curvedata
join f _ "doubleback"           = f R.doubleback          D.doubleback
join f _ "slalom"               = f R.slalom              D.slalom
join f _ "compass"              = f R.compass             D.compass
join _ e t                      = e $ "unknown puzzle type: " ++ t

drawPuzzle :: (Backend b R2, Renderable (Path R2) b) =>
              TypedPuzzle -> Parser (Diagram b R2, Diagram b R2)
drawPuzzle p = join puzzleSol fail (puzzleType p) (pv, sv)
  where
    (RP pv sv) = dropType p
