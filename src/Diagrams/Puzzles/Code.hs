{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams.Puzzles.Code
    (
      CodeDiagrams (..)
    , drawCode
    ) where

import Data.Puzzles.Code
import Data.Puzzles.GridShape
import Data.Puzzles.Grid
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.Elements

import Diagrams.Prelude

import qualified Data.Map as Map

data CodeDiagrams a = CodeDiagrams { _cdLeft :: a, _cdTop :: a, _cdOver :: a }

instance Monoid a => Monoid (CodeDiagrams a) where
    mempty = CodeDiagrams mempty mempty mempty
    (CodeDiagrams x y z) `mappend` (CodeDiagrams x' y' z') =
        CodeDiagrams (x `mappend` x') (y `mappend` y') (z `mappend` z')

drawCode :: Backend' b => Code -> CodeDiagrams (Diagram b)
drawCode cs = mconcat (map drawCodePart cs)

drawCodePart :: Backend' b => CodePart -> CodeDiagrams (Diagram b)
drawCodePart (Rows'  rs) = CodeDiagrams (placeGrid g # centerX) mempty mempty
  where
    g = Map.fromList [ (C 0 r, arrowRight) | r <- rs ]
drawCodePart (Cols   cs) = CodeDiagrams mempty (placeGrid g # centerY) mempty
  where
    g = Map.fromList [ (C c 0, arrowDown)  | c <- cs ]
drawCodePart (RowsN' rs) = CodeDiagrams (placeGrid g # centerX) mempty mempty
  where
    g = Map.fromList [ (N 0 r, arrowRight) | r <- rs ]
drawCodePart (ColsN  cs) = CodeDiagrams mempty (placeGrid g # centerY) mempty
  where
    g = Map.fromList [ (N c 0, arrowDown)  | c <- cs ]
drawCodePart (LabelsN g) = CodeDiagrams mempty mempty (placeGrid . fmap label . clues $ g)
  where
    label c = drawChar c # scale 0.5 # fc gray # translate (r2 (1/3, -1/3))
drawCodePart (LRows'  rs) = CodeDiagrams (placeGrid g # centerX) mempty mempty
  where
    g = Map.fromList [ (C 0 r, arrowRightL l) | (l, r) <- Map.toList rs ]
drawCodePart (LCols   cs) = CodeDiagrams mempty (placeGrid g # centerY) mempty
  where
    g = Map.fromList [ (C c 0, arrowDownL l)  | (l, c) <- Map.toList cs ]
drawCodePart (LRowsN' rs) = CodeDiagrams (placeGrid g # centerX) mempty mempty
  where
    g = Map.fromList [ (N 0 r, arrowRightL l) | (l, r) <- Map.toList rs ]
drawCodePart (LColsN  cs) = CodeDiagrams mempty (placeGrid g # centerY) mempty
  where
    g = Map.fromList [ (N c 0, arrowDownL l)  | (l, c) <- Map.toList cs ]

arrowDown :: Backend' b => Diagram b
arrowDown = triangle 0.7 # lwG 0 # fc (blend 0.7 white black) # rotateBy (1/2)

arrowDownL :: Backend' b => Char -> Diagram b
arrowDownL c = drawChar c # scale 0.7 <> arrowDown # scale 1.2

arrowRight :: Backend' b => Diagram b
arrowRight = arrowDown # rotateBy (1/4)

arrowRightL :: Backend' b => Char -> Diagram b
arrowRightL c = drawChar c # scale 0.7 <> arrowRight # scale 1.2
