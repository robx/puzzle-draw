{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Diagrams.Puzzles.PuzzleGrids where

import Diagrams.Prelude hiding (size)

import Data.Puzzles.Grid
import Data.Puzzles.Elements
import Data.Puzzles.Sudoku

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Elements

drawClueGrid :: Backend' b =>
                SGrid (Clue Char) -> Diagram b
drawClueGrid = atCentres drawChar . clues <> grid . size

drawIntClues :: Backend' b =>
                SGrid (Clue Int) -> Diagram b
drawIntClues = atCentres drawInt . clues

drawInts :: Backend' b =>
            SGrid Int -> Diagram b
drawInts = atCentres drawInt . values

drawIntGrid :: Backend' b =>
               SGrid (Clue Int) -> Diagram b
drawIntGrid = drawIntClues <> grid . size

drawSlitherGrid :: Backend' b =>
                   SGrid (Clue Int) -> Diagram b
drawSlitherGrid = atCentres drawInt . clues <> slithergrid . size

drawMasyuGrid :: Backend' b =>
                 SGrid MasyuClue -> Diagram b
drawMasyuGrid = atCentres pearl . clues <> grid . size

drawCompassClues :: Backend' b =>
                    SGrid CompassClue -> Diagram b
drawCompassClues = atCentres drawCompassClue . clues

drawCompassGrid :: Backend' b =>
                   SGrid CompassClue -> Diagram b
drawCompassGrid = drawCompassClues <> grid . size

sudokugrid :: Backend' b =>
              SGrid a -> Diagram b
sudokugrid = drawEdges . sudokubordersg  <> grid . size

drawWordsClues :: Backend' b =>
                  SGrid (Clue [String]) -> Diagram b
drawWordsClues = atCentres drawWords . clues

drawTightGrid :: Backend' b =>
                 (t -> Diagram b) -> SGrid (Tightfit t) -> Diagram b
drawTightGrid d g = atCentres (drawTight d) (values g)
                    <> grid (size g)
                    <> phantom' (stroke $ p2i (-1,-1) ~~ p2i (sx + 1, sy + 1))
    where (sx, sy) = size g

drawSlalomGrid :: Backend' b =>
                  SGrid (Clue Int) -> Diagram b
drawSlalomGrid g = atVertices drawSlalomClue (clues g)
                   <> grid (w-1, h-1)
    where (w, h) = size g

drawSlalomDiags :: Backend' b =>
                   SGrid SlalomDiag -> Diagram b
drawSlalomDiags = atCentres diag . clues . fmap Just
    where diag SlalomForward  = stroke ur # lwG edgewidth
          diag SlalomBackward = stroke dr # lwG edgewidth

drawCrosses ::  Backend' b =>
                 SGrid Bool -> Diagram b
drawCrosses = atCentres (if' drawCross mempty) . values
  where
    if' a b x = if x then a else b


outsideGrid :: Backend' b =>
               OutsideClues [String] -> Diagram b
outsideGrid = atCentres (scale 0.8 . drawText) . multiOutsideClues
              <> grid . outsideSize

outsideIntGrid :: Backend' b =>
                  OutsideClues [Int] -> Diagram b
outsideIntGrid = atCentres (scale 0.8 . drawInt) . multiOutsideClues
                 <> grid . outsideSize
