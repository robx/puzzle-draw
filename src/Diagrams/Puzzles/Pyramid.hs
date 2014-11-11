{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Pyramid where

import Diagrams.Prelude

import Data.Puzzles.Elements
import Data.Puzzles.Pyramid
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

pgray :: Colour Double
pgray = blend 0.6 white black

cell :: Backend' b => Bool -> Diagram b R2
cell s = square 1 # lwG onepix # if s then fc pgray else id

clue :: Backend' b => Maybe Int -> Diagram b R2
clue Nothing = mempty
clue (Just c) = text' (show c)

cellc :: Backend' b => Bool -> Maybe Int -> Diagram b R2
cellc s c = clue c `atop` cell s

row :: Backend' b => Row -> Diagram b R2
row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid :: Backend' b => Pyramid -> Diagram b R2
pyramid = alignBL . vcat . map row . unPyr

kropki :: Backend' b => KropkiDot -> Diagram b R2
kropki None = mempty
kropki c = circle 0.1 # lwG 0.03 # fc (col c) # smash
    where col White = white
          col Black = blend 0.98 black white
          col None  = error "can't reach"

krow :: Backend' b => KropkiRow -> Diagram b R2
krow (KR cs s ks) = ccat dots <> ccat clues
    where ccat = centerX . hcat
          clues = map (cellc s) cs
          dots = interleave (map phantom' clues) (map kropki ks)

kpyramid :: Backend' b => RowKropkiPyramid -> Diagram b R2
kpyramid = alignBL . vcat . map krow . unKP
