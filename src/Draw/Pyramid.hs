{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Pyramid where

import           Diagrams.Prelude

import           Data.Pyramid
import           Draw.Draw
import           Draw.Elements
import           Draw.Lib
import           Draw.Grid

pgray :: Colour Double
pgray = blend 0.6 white black

clue :: Backend' b => Maybe Int -> Drawing b
clue Nothing  = mempty
clue (Just c) = text' (show c)

cellc :: Backend' b => Bool -> Maybe Int -> Drawing b
cellc s c = clue c <> (gridCell # if s then fc pgray else id)

row :: Backend' b => Row -> Drawing b
row (R cs s) = centerX' . hcat . map (cellc s) $ cs

pyramid :: Backend' b => Pyramid -> Drawing b
pyramid = alignBL' . vcat . map row . unPyr

krow :: Backend' b => KropkiRow -> Drawing b
krow (KR cs s ks) = ccat dots <> ccat clues
 where
  ccat  = centerX' . hcat
  clues = map (cellc s) cs
  dots  = interleave (map phantom'' clues) (map kropkiDot ks)

kpyramid :: Backend' b => RowKropkiPyramid -> Drawing b
kpyramid = alignBL' . vcat . map krow . unKP
