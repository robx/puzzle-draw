module Diagrams.Puzzles.Style
    ( LineStyle (..)
    , FrameStyle (..)
    , VertexStyle (..)
    , GridStyle (..)

    , gDefault
    , gDashed
    , gDashedThick
    , gPlain
    , gPlainDashed
    , gSlither
    ) where

import Diagrams.Puzzles.Widths

import Diagrams.Prelude

data LineStyle =
      LineNone
    | LineThin
    | LineDashed
    | LineThick

data FrameStyle = FrameStyle
    { _fWidthFactor :: Double
    , _fColour      :: Colour Double
    }

data VertexStyle =
      VertexNone
    | VertexDot

data GridStyle = GridStyle
    { _line    :: LineStyle
    , _border  :: LineStyle
    , _frame   :: Maybe FrameStyle
    , _vertex  :: VertexStyle
    }

gDefault, gSlither, gDashed, gDashedThick, gPlain, gPlainDashed :: GridStyle
gDefault = GridStyle LineThin LineThin
                     (Just (FrameStyle framewidthfactor black)) VertexNone
gSlither = GridStyle LineNone LineNone Nothing VertexDot
gDashed  = GridStyle LineDashed LineThin
                     (Just (FrameStyle framewidthfactor black)) VertexNone
gDashedThick  = GridStyle LineDashed LineThick
                          (Just (FrameStyle framewidthfactor black)) VertexNone
gPlain   = GridStyle LineThin LineThin Nothing VertexNone
gPlainDashed = GridStyle LineDashed LineDashed Nothing VertexNone
