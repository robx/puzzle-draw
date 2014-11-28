{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Helpers to string together parser and renderer by puzzle type.

module Data.Puzzles.Compose (
    PuzzleHandler,
    handle,
    -- * Handlers
    drawPuzzleMaybeSol,
  ) where

import Diagrams.Prelude
import Data.Yaml (Parser, Value)
import Data.Traversable (traverse)

import Text.Puzzles.Puzzle
import Diagrams.Puzzles.Draw
import Diagrams.Puzzles.Lib
import Data.Puzzles.PuzzleTypes

import qualified Text.Puzzles.PuzzleTypes as R
import qualified Diagrams.Puzzles.PuzzleTypes as D

-- | A function to compose an arbitrary matching pair of parser and renderer.
--   In @PuzzleHandler b a@, @b@ is the rendering backend type, while @a@ is
--   the result type of the composition.
type PuzzleHandler b a = forall p q.
                         ParsePuzzle p q -> RenderPuzzle b p q -> a

-- | @handle h t@ composes the parser and renderer for the puzzle
--   type @t@ with the handler @h@.
handle :: Backend' b =>
        PuzzleHandler b a -> PuzzleType -> a
handle f LITS                 = f R.lits                D.lits
handle f LITSPlus             = f R.litsplus            D.litsplus
handle f Geradeweg            = f R.geradeweg           D.geradeweg
handle f Fillomino            = f R.fillomino           D.fillomino
handle f Masyu                = f R.masyu               D.masyu
handle f Nurikabe             = f R.nurikabe            D.nurikabe
handle f LatinTapa            = f R.latintapa           D.latintapa
handle f Sudoku               = f R.sudoku              D.sudoku
handle f ThermoSudoku         = f R.thermosudoku        D.thermosudoku
handle f Pyramid              = f R.pyramid             D.pyramid
handle f RowKropkiPyramid     = f R.kpyramid            D.kpyramid
handle f SlitherLink          = f R.slither             D.slither
handle f SlitherLinkLiar      = f R.liarslither         D.liarslither
handle f TightfitSkyscrapers  = f R.tightfitskyscrapers D.tightfitskyscrapers
handle f WordLoop             = f R.wordloop            D.wordloop
handle f WordSearch           = f R.wordsearch          D.wordsearch
handle f CurveData            = f R.curvedata           D.curvedata
handle f DoubleBack           = f R.doubleback          D.doubleback
handle f Slalom               = f R.slalom              D.slalom
handle f Compass              = f R.compass             D.compass
handle f BoxOf2Or3            = f R.boxof2or3           D.boxof2or3
handle f AfternoonSkyscrapers = f R.afternoonskyscrapers D.afternoonskyscrapers
handle f MeanderingNumbers    = f R.meanderingnumbers   D.meanderingnumbers
handle f Tapa                 = f R.tapa                D.tapa
handle f JapaneseSums         = f R.japanesesums        D.japanesesums
handle f Coral                = f R.coral               D.coral
handle f MaximalLengths       = f R.maximallengths      D.maximallengths
handle f PrimePlace           = f R.primeplace          D.primeplace
handle f Labyrinth            = f R.labyrinth           D.labyrinth
handle f Bahnhof              = f R.bahnhof             D.bahnhof
handle f Cave                 = f R.cave                D.cave
handle f AngleLoop            = f R.angleLoop           D.angleLoop

-- | Handler that parses puzzle and an optional solution from a pair of
--   corresponding YAML values, and renders both individually, optionally
--   for the solution.
drawPuzzleMaybeSol :: PuzzleHandler b ((Value, Maybe Value)
                      -> Parser (Diagram b R2, Maybe (Diagram b R2)))
drawPuzzleMaybeSol (pp, ps) (dp, ds) (p, s) = do
    p' <- pp p
    s' <- traverse ps s
    let mps = case s' of Nothing  -> Nothing
                         Just s'' -> Just (p', s'')
    return (dp p', ds <$> mps)

