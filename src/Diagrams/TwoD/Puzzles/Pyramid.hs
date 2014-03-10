{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.TwoD.Puzzles.Pyramid where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Pyramid
import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Grid (frame)

pgray = blend 0.6 white black

cell s = square 1 # lw onepix # if s then fc pgray else id

clue Nothing = mempty
clue (Just c) = text' (show c)
cellc s c = clue c `atop` cell s

row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid p = phantom (frame (s, s)) <> (alignBL . vcat . map row . unPyr $ p)
    where s = psize p

kropki None = mempty
kropki c = circle 0.1 # lw 0.03 # fc (col c) # smash
    where col White = white
          col Black = blend 0.98 black white

krow (KR cs s ks) = ccat dots `atop` ccat clues
    where ccat = centerX . hcat
          clues = map (cellc s) cs
          dots = interleave (map phantom (clues :: [D R2])) (map kropki ks)

kpyramid p = phantom (frame (s, s)) <> (alignBL . vcat . map krow . unKP $ p)
    where s = psize (plainpyramid p)
