{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- |
-- Helpers to string together parser and renderer by puzzle type.

module Puzzles.Compose (
    PuzzleHandler,
    handle,
    -- * Handlers
    drawPuzzle,
    drawPuzzleSol,
    drawPuzzleMaybeSol,
    drawPuzzle',
    drawSolution',
    drawExample'
  ) where

import Data.Maybe
import Puzzles.Parse.Puzzle
import Puzzles.Diagrams.Draw
import qualified Puzzles.Parse.PuzzleTypes as R
import qualified Puzzles.Diagrams.PuzzleTypes as D
import Diagrams.Prelude
import Data.Yaml (Parser, Value)
import Data.Traversable (traverse)

-- | A function to compose an arbitrary matching pair of parser and renderer.
--   In @PuzzleHandler b a@, @b@ is the rendering backend type, while @a@ is
--   the result type of the composition.
type PuzzleHandler b a = forall p q.
                         ParsePuzzle p q -> RenderPuzzle b p q -> a

-- | @handle h e t@ composes the parser and renderer for the puzzle
--   type @t@ with the handler @h@. Calls @e@ with an error message
--   for unknown puzzle types.
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
handle f _ "afternoonskyscrapers" = f R.afternoonskyscrapers D.afternoonskyscrapers
handle f _ "countnumbers"         = f R.countnumbers        D.countnumbers
handle _ e t                      = e $ "unknown puzzle type: " ++ t

-- | Handler that parses a puzzle from a YAML value, and renders.
drawPuzzle :: PuzzleHandler b (Value -> Parser (Diagram b R2))
drawPuzzle (pp, _) (dp, _) p = do
    p' <- pp p
    return $ dp p'

-- | Handler that parses puzzle and solution from a pair of corresponding
--   YAML values, and renders both individually.
drawPuzzleSol :: PuzzleHandler b ((Value, Value)
                 -> Parser (Diagram b R2, Diagram b R2))
drawPuzzleSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- ps s
    return (dp p', ds (p', s'))

-- | Handler that parses puzzle and an optional solution from a pair of
--   corresponding YAML values, and renders both individually, optionally
--   for the solution.
drawPuzzleMaybeSol :: PuzzleHandler b ((Value, Maybe Value)
                      -> Parser (Diagram b R2, Maybe (Diagram b R2)))
drawPuzzleMaybeSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- traverse ps $ s
    let mps = case s' of Nothing  -> Nothing
                         Just s'' -> Just (p', s'')
    return (dp p', ds <$> mps)

-- | Variant of 'drawPuzzle' that accepts a pair of puzzle YAML value and
--   optional solution YAML value.
drawPuzzle' :: PuzzleHandler b ((Value, Maybe Value) -> Parser (Diagram b R2))
drawPuzzle' (pp, _) (dp, _) (p, _) = do
    p' <- pp p
    return $ dp p'

-- | Handler that accepts a pair of puzzle YAML value and optional solution
--   YAML value, and renders the solution, failing if the solution is not
--   provided.
drawSolution' :: PuzzleHandler b ((Value, Maybe Value) -> Parser (Diagram b R2))
drawSolution' (pp, ps) (_, ds) (p, ms) = do
    p' <- pp p
    s' <- maybe (fail "no solution provided") ps ms
    return $ ds (p', s')

-- | Like 'drawSolution'', but renders puzzle and solution in example layout.
drawExample' :: (Backend b R2, Renderable (Path R2) b) =>
                PuzzleHandler b ((Value, Maybe Value) -> Parser (Diagram b R2))
drawExample' (pp, ps) (dp, ds) (p, ms) = do
    p' <- pp p
    s' <- maybe (fail "no solution provided") ps ms
    return . fromJust $ draw (dp p', Just $ ds (p', s')) DrawExample
