{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams.Puzzles.Pyramid where

import Diagrams.Prelude

import Data.Puzzles.Elements
import Data.Puzzles.Pyramid
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

pgray :: Colour Double
pgray = blend 0.6 white black

cell :: Backend' b => Bool -> Diagram b
cell s = square 1 # lwG onepix # if s then fc pgray else id

clue :: Backend' b => Maybe Int -> Diagram b
clue Nothing = mempty
clue (Just c) = text' (show c)

cellc :: Backend' b => Bool -> Maybe Int -> Diagram b
cellc s c = clue c `atop` cell s

row :: Backend' b => Row -> Diagram b
row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid :: Backend' b => Pyramid -> Diagram b
pyramid = alignBL . vcat . map row . unPyr

kropki :: Backend' b => KropkiDot -> Diagram b
kropki KNone = mempty
kropki c = circle 0.1 # lwG 0.03 # fc (col c) # smash
    where col KWhite = white
          col KBlack = blend 0.98 black white
          col KNone  = error "can't reach"

krow :: Backend' b => KropkiRow -> Diagram b
krow (KR cs s ks) = ccat dots <> ccat clues
    where ccat = centerX . hcat
          clues = map (cellc s) cs
          dots = interleave (map phantom clues) (map kropki ks)

kpyramid :: Backend' b => RowKropkiPyramid -> Diagram b
kpyramid = alignBL . vcat . map krow . unKP
