{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Puzzles.Draw (
    PuzzleSol,
    RenderPuzzle,
    OutputChoice(..),
    draw
  ) where

import Diagrams.Prelude

type RenderPuzzle b p s = (p -> Diagram b R2, (p, s) -> Diagram b R2)

type PuzzleSol b = (Diagram b R2, Maybe (Diagram b R2))

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
    deriving Show

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
draw :: (Backend b R2, Renderable (Path R2) b) =>
        PuzzleSol b -> OutputChoice -> Maybe (Diagram b R2)
draw (p, ms) = fmap (bg white) . d
    where d DrawPuzzle   = Just p
          d DrawSolution = ms
          d DrawExample  = ms >>= \s -> return $ p ||| strutX 2.0 ||| s
