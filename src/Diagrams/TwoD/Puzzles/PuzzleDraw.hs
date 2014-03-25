{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Diagrams.TwoD.Puzzles.PuzzleDraw (
    drawPuzzle,
    drawPuzzleSol,
    drawPuzzleMaybeSol,
    handle
    ) where

import Data.Puzzles.Read (ParsePuzzle)
import Diagrams.TwoD.Puzzles.Puzzle (RenderPuzzle)
import qualified Data.Puzzles.Read as R
import qualified Diagrams.TwoD.Puzzles.Puzzle as D
import Diagrams.Prelude
import Data.Yaml (Parser, Value)
import Data.Traversable (traverse)

type PuzzleHandler b a = forall p q. ParsePuzzle p q -> RenderPuzzle b p q -> a

drawPuzzle :: PuzzleHandler b (Value -> Parser (Diagram b R2))
drawPuzzle (pp, _) (dp, _) p = do
    p' <- pp p
    return $ dp p'

drawPuzzleSol :: PuzzleHandler b ((Value, Value) -> Parser (Diagram b R2, Diagram b R2))
drawPuzzleSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- ps s
    return (dp p', ds (p', s'))

drawPuzzleMaybeSol :: PuzzleHandler b ((Value, Maybe Value)
                      -> Parser (Diagram b R2, Maybe (Diagram b R2)))
drawPuzzleMaybeSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- traverse ps $ s
    let mps = case s' of Nothing  -> Nothing
                         Just s'' -> Just (p', s'')
    return (dp p', ds <$> mps)

handle :: (Backend b R2, Renderable (Path R2) b) =>
        PuzzleHandler b a -> (String -> a) -> String -> a
handle f _ "lits"                 = f R.lits                D.lits
handle f _ "litsplus"             = f R.litsplus            D.litsplus
handle f _ "geradeweg"            = f R.geradeweg           D.geradeweg
handle f _ "fillomino"            = f R.fillomino           D.fillomino
handle f _ "masyu"                = f R.masyu               D.masyu
handle f _ "nurikabe"             = f R.nurikabe            D.nurikabe
handle f _ "latintapa"            = f R.latintapa           D.latintapa
handle f _ "sudoku"               = f R.sudoku              D.sudoku
handle f _ "thermosudoku"         = f R.thermosudoku        D.thermosudoku
handle f _ "pyramid"              = f R.pyramid             D.pyramid
handle f _ "rowkropkipyramid"     = f R.kpyramid            D.kpyramid
handle f _ "slitherlink"          = f R.slither             D.slither
handle f _ "slitherlinkliar"      = f R.liarslither         D.liarslither
handle f _ "skyscrapers-tightfit" = f R.tightfitskyscrapers D.tightfitskyscrapers
handle f _ "wordloop"             = f R.wordloop            D.wordloop
handle f _ "wordsearch"           = f R.wordsearch          D.wordsearch
handle f _ "curvedata"            = f R.curvedata           D.curvedata
handle f _ "doubleback"           = f R.doubleback          D.doubleback
handle f _ "slalom"               = f R.slalom              D.slalom
handle f _ "compass"              = f R.compass             D.compass
handle f _ "boxof2or3"            = f R.boxof2or3           D.boxof2or3
handle _ e t                      = e $ "unknown puzzle type: " ++ t
