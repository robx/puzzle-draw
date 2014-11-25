{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Diagrams.Puzzles.Code
    (
      CodeDiagrams (..)
    , drawCode
    ) where

import Data.Puzzles.Code
import Data.Puzzles.GridShape
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Grid

import Diagrams.Prelude

import qualified Data.Map as Map

data CodeDiagrams a = CodeDiagrams { _cdLeft :: a, _cdTop :: a }

instance Monoid a => Monoid (CodeDiagrams a) where
    mempty = CodeDiagrams mempty mempty
    (CodeDiagrams x y) `mappend` (CodeDiagrams x' y') =
        CodeDiagrams (x `mappend` x') (y `mappend` y')

drawCode :: Backend' b => Code -> CodeDiagrams (Diagram b R2)
drawCode cs = mconcat (map drawCodePart cs)

drawCodePart :: Backend' b => CodePart -> CodeDiagrams (Diagram b R2)
drawCodePart (Rows'  rs) = CodeDiagrams (placeGrid g # centerX) mempty
  where
    g = Map.fromList [ (C 0 r, arrowRight) | r <- rs ]
drawCodePart (Cols   cs) = CodeDiagrams mempty (placeGrid g # centerY)
  where
    g = Map.fromList [ (C c 0, arrowDown)  | c <- cs ]
drawCodePart (RowsN' rs) = CodeDiagrams (placeGrid g # centerX) mempty
  where
    g = Map.fromList [ (N 0 r, arrowRight) | r <- rs ]
drawCodePart (ColsN  cs) = CodeDiagrams mempty (placeGrid g # centerY)
  where
    g = Map.fromList [ (N c 0, arrowDown)  | c <- cs ]

arrowDown :: Backend' b => Diagram b R2
arrowDown = triangle 0.7 # lwG 0 # fc (blend 0.7 white black) # rotateBy (1/2)

arrowRight :: Backend' b => Diagram b R2
arrowRight = arrowDown # rotateBy (1/4)
