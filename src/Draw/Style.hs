module Draw.Style
  ( LineStyle (..),
    FrameStyle (..),
    VertexStyle (..),
    GridStyle (..),
    gDefault,
    gDefaultIrreg,
    gDashed,
    gDashedThick,
    gPlain,
    gPlainDashed,
    gSlither,
  )
where

import Diagrams.Prelude
import Draw.Widths

data LineStyle
  = LineNone
  | LineThin
  | LineDashed
  | LineThick

data FrameStyle
  = FrameStyle
      { _fWidthFactor :: Double,
        _fColour :: Colour Double
      }

data VertexStyle
  = VertexNone
  | VertexDot

data GridStyle
  = GridStyle
      { _line :: LineStyle,
        _border :: LineStyle,
        _frame :: Maybe FrameStyle,
        _vertex :: VertexStyle
      }

gDefault,
  gDefaultIrreg,
  gSlither,
  gDashed,
  gDashedThick,
  gPlain,
  gPlainDashed ::
    GridStyle
gDefault =
  GridStyle
    LineThin
    LineThin
    (Just (FrameStyle framewidthfactor black))
    VertexNone
gDefaultIrreg = GridStyle LineThin LineThick Nothing VertexNone
gSlither = GridStyle LineNone LineNone Nothing VertexDot
gDashed =
  GridStyle
    LineDashed
    LineThin
    (Just (FrameStyle framewidthfactor black))
    VertexNone
gDashedThick = GridStyle LineDashed LineThick Nothing VertexNone
gPlain = GridStyle LineThin LineThin Nothing VertexNone
gPlainDashed = GridStyle LineDashed LineDashed Nothing VertexNone
