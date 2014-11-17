module Diagrams.Puzzles.Style
    ( GridLineStyle   (..)
    , GridBorderStyle (..)
    , GridVertexStyle (..)
    ) where

import Diagrams.Prelude

data GridLineStyle =
      GridLineThin
    | GridLineDashed
    | GridLineNone

data GridBorderStyle =
      GridBorderNone
    | GridBorderThin
    | GridBorderDashed
    | GridBorderThick
    | GridBorderFrame Double (Colour Double)

data GridVertexStyle =
      GridVertexNone
    | GridVertexDot
