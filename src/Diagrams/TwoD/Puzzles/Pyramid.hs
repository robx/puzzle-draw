{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.TwoD.Puzzles.Pyramid where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Pyramid

pgray = blend 0.6 white black

cell s = square 1 # lw 0.03 # if s then fc pgray else id

clue Nothing = mempty
clue (Just c) = text (show c) # fontSize 0.7 # font "Helvetica"
cellc s c = clue c `atop` cell s

row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid = vcat . map row . unP

kropki None = mempty
kropki White = circle 0.1 # lw 0.03 # withEnvelope (vrule 0 :: D R2)
kropki Black = kropki White # fc black

interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

krow (KR cs s ks) = ccat dots `atop` ccat clues
    where ccat = centerX . hcat
          clues = map (cellc s) cs
          dots = interleave (map phantom (clues :: [D R2])) (map kropki ks)

kpyramid = vcat . map krow . unKP
