{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.TwoD.Puzzles.Pyramid where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Pyramid

cell s = square 1 # if s then fc gray else id

clue Nothing = mempty
clue (Just c) = text (show c)

cellc s c = clue c `atop` cell s

row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid = vcat . map row . unP
