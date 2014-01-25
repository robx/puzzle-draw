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
