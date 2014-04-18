{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Diagrams.Puzzles.Draw (
    PuzzleSol,
    RenderPuzzle,
    OutputChoice(..),
    draw,
    Unit(..),
    diagramWidth,
    toOutputWidth,
  ) where

import Diagrams.Prelude
import Diagrams.BoundingBox

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

data Unit = Pixels | Points

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

diagramWidth :: Diagram b R2 -> Double
diagramWidth = fst . unr2 . boxExtents . boundingBox

toOutputWidth :: Unit -> Double -> Double
toOutputWidth u w = case u of Pixels -> fromIntegral wpix
                              Points -> wpt
  where
    wpix = round (40 * w) :: Int  -- grid square size 40px
    wpt = cmtopoint (0.8 * w)     -- grid square size 0.8cm
