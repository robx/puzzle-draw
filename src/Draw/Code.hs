{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Code
    (
      CodeDiagrams (..)
    , drawCode
    ) where

import Data.Code
import Data.GridShape
import Data.Grid
import Draw.Draw
import Draw.Lib
import Draw.Grid
import Draw.Elements

import Diagrams.Prelude

import qualified Data.Map.Strict as Map

drawCode :: Backend' b => Code -> CodeDiagrams (Drawing b)
drawCode cs = mconcat (map drawCodePart cs)

drawCodePart :: Backend' b => CodePart -> CodeDiagrams (Drawing b)
drawCodePart (Rows'  rs) = CodeDiagrams (placeGrid g # centerX') mempty mempty
  where
    g = Map.fromList [ (C 0 r, arrowRight) | r <- rs ]
drawCodePart (Cols   cs) = CodeDiagrams mempty (placeGrid g # centerY') mempty
  where
    g = Map.fromList [ (C c 0, arrowDown)  | c <- cs ]
drawCodePart (RowsN' rs) = CodeDiagrams (placeGrid g # centerX') mempty mempty
  where
    g = Map.fromList [ (N 0 r, arrowRight) | r <- rs ]
drawCodePart (ColsN  cs) = CodeDiagrams mempty (placeGrid g # centerY') mempty
  where
    g = Map.fromList [ (N c 0, arrowDown)  | c <- cs ]
drawCodePart (LabelsN g) = CodeDiagrams mempty mempty (placeGrid . fmap label . clues $ g)
  where
    label c = drawChar c # scale 0.5 # fc gray # translate (r2 (1/3, -1/3))
drawCodePart (LRows'  rs) = CodeDiagrams (placeGrid g # centerX') mempty mempty
  where
    g = Map.fromList [ (C 0 r, arrowRightL l) | (l, r) <- Map.toList rs ]
drawCodePart (LCols   cs) = CodeDiagrams mempty (placeGrid g # centerY') mempty
  where
    g = Map.fromList [ (C c 0, arrowDownL l)  | (l, c) <- Map.toList cs ]
drawCodePart (LRowsN' rs) = CodeDiagrams (placeGrid g # centerX') mempty mempty
  where
    g = Map.fromList [ (N 0 r, arrowRightL l) | (l, r) <- Map.toList rs ]
drawCodePart (LColsN  cs) = CodeDiagrams mempty (placeGrid g # centerY') mempty
  where
    g = Map.fromList [ (N c 0, arrowDownL l)  | (l, c) <- Map.toList cs ]

arrowDown :: Backend' b => Drawing b
arrowDown = draw $ triangle 0.5 # lwG 0 # fc black # rotateBy (1/2)

arrowDownL :: Backend' b => Char -> Drawing b
arrowDownL c = drawChar c # fc white # scale 0.5 <> arrowDown # scale 1.2

arrowRight :: Backend' b => Drawing b
arrowRight = arrowDown # rotateBy (1/4)

arrowRightL :: Backend' b => Char -> Drawing b
arrowRightL c = drawChar c # fc white # scale 0.5 # translate (r2 (-0.05,0)) <> arrowRight # scale 1.2
