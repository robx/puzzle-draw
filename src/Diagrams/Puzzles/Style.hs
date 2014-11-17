module Diagrams.Puzzles.Style
    (
      GridStyle(..)
    ) where

data GridStyle =
      GridNormal      -- ^ Normal grid with thin lines and thick border
    | GridDashed      -- ^ dashed lines, thick border
    | GridPlain       -- ^ thin lines, thin border
    | GridPlainDashed -- ^ dashed lines, including border
    | GridSlither     -- ^ Slitherlink grid (dots)
