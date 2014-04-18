{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Puzzles.Pyramid where

import Diagrams.Prelude

import Data.Puzzles.Elements
import Data.Puzzles.Pyramid
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

pgray :: Colour Double
pgray = blend 0.6 white black

cell :: (Backend b R2, Renderable (Path R2) b) => Bool -> Diagram b R2
cell s = square 1 # lw onepix # if s then fc pgray else id

clue :: (Backend b R2, Renderable (Path R2) b) => Maybe Int -> Diagram b R2
clue Nothing = mempty
clue (Just c) = text' (show c)

cellc :: (Backend b R2, Renderable (Path R2) b) => Bool -> Maybe Int -> Diagram b R2
cellc s c = clue c `atop` cell s

row :: (Backend b R2, Renderable (Path R2) b) => Row -> Diagram b R2
row (R cs s) = centerX . hcat . map (cellc s) $ cs

pyramid :: (Backend b R2, Renderable (Path R2) b) => Pyramid -> Diagram b R2
pyramid = alignBL . vcat . map row . unPyr

kropki :: (Backend b R2, Renderable (Path R2) b) => KropkiDot -> Diagram b R2
kropki None = mempty
kropki c = circle 0.1 # lw 0.03 # fc (col c) # smash
    where col White = white
          col Black = blend 0.98 black white
          col None  = error "can't reach"

krow :: (Backend b R2, Renderable (Path R2) b) => KropkiRow -> Diagram b R2
krow (KR cs s ks) = ccat dots <> ccat clues
    where ccat = centerX . hcat
          clues = map (cellc s) cs
          clues' = map (cellc s) cs :: [D R2]
          dots = interleave (map phantom' clues') (map kropki ks)

kpyramid :: (Backend b R2, Renderable (Path R2) b) => RowKropkiPyramid -> Diagram b R2
kpyramid = alignBL . vcat . map krow . unKP
