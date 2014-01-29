{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Graphics.SVGFonts.ReadFont

-- prettier with a different font
--font' = outlMap "../fonts/SourceSansPro-Light.svg"
font' = lin

text' t = stroke (textSVG' $ TextOpts t font' INSIDE_H KERN False 1 1)
          # fc black # scale 0.3

interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs

spread v things = cat v . interleave (repeat (strut vgap)) $ things
  where ds = map (diameter v) things
        gap = (magnitude v - sum ds) / fromIntegral ((length things) + 1)
        vgap = (gap / magnitude v) *^ v

clues = [ ((0, 4), ["DR", "OR"])
        , ((5, 5), ["WOW"])
        , ((2, 2), ["WORD", "OW"])
        , ((3, 3), ["WDRO", "DW"])
        ]

drawclue (pt, ws) = spread unitY (map (centerXY . text') ws)
                  # centerY
                  # translate ((r2 pt) + (0.5 ^& 0.5))

puzzle = mconcat (map drawclue clues) `atop` grid 6 6

main = defaultMain (frame puzzle # bg white)
