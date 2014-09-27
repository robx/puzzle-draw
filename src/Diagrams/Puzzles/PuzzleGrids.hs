{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.Puzzles.PuzzleGrids where

import Diagrams.Prelude

import Data.Puzzles.Grid
import Data.Puzzles.Elements
import Data.Puzzles.Sudoku

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Elements

drawFillo :: (Backend b R2, Renderable (Path R2) b) =>
             SGrid (Clue Int) -> Diagram b R2
drawFillo = drawIntClues <> dashedgrid . size

drawClueGrid :: (Backend b R2, Renderable (Path R2) b) =>
                SGrid (Clue Char) -> Diagram b R2
drawClueGrid = atCentres drawChar . clues <> grid . size

drawIntClues :: (Backend b R2, Renderable (Path R2) b) =>
                SGrid (Clue Int) -> Diagram b R2
drawIntClues = atCentres drawInt . clues

drawInts :: (Backend b R2, Renderable (Path R2) b) =>
            SGrid Int -> Diagram b R2
drawInts = atCentres drawInt . values

drawIntGrid :: (Backend b R2, Renderable (Path R2) b) =>
               SGrid (Clue Int) -> Diagram b R2
drawIntGrid = drawIntClues <> grid . size

drawSlitherGrid :: (Backend b R2, Renderable (Path R2) b) =>
                   SGrid (Clue Int) -> Diagram b R2
drawSlitherGrid = atCentres drawInt . clues <> slithergrid . size

drawMasyuGrid :: (Backend b R2, Renderable (Path R2) b) =>
                 SGrid MasyuClue -> Diagram b R2
drawMasyuGrid = atCentres pearl . clues <> grid . size

drawCompassClues :: (Backend b R2, Renderable (Path R2) b) =>
                    SGrid CompassClue -> Diagram b R2
drawCompassClues = atCentres drawCompassClue . clues

drawCompassGrid :: (Backend b R2, Renderable (Path R2) b) =>
                   SGrid CompassClue -> Diagram b R2
drawCompassGrid = drawCompassClues <> grid . size

sudokugrid :: (Backend b R2, Renderable (Path R2) b) =>
              SGrid a -> Diagram b R2
sudokugrid = drawEdges . sudokubordersg  <> grid . size

drawWordsClues :: (Backend b R2, Renderable (Path R2) b) =>
                  SGrid (Clue [String]) -> Diagram b R2
drawWordsClues = atCentres drawWords . clues

drawTightGrid :: (Backend b R2, Renderable (Path R2) b) =>
                 (t -> Diagram b R2) -> SGrid (Tightfit t) -> Diagram b R2
drawTightGrid d g = atCentres (drawTight d) (values g)
                    <> grid (size g)
                    <> phantom' (stroke $ p2i (-1,-1) ~~ p2i (sx + 1, sy + 1))
    where (sx, sy) = size g

drawSlalomGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  SGrid (Clue Int) -> Diagram b R2
drawSlalomGrid g = atVertices drawSlalomClue (clues g)
                   <> grid (w-1, h-1)
    where (w, h) = size g

drawSlalomDiags :: (Backend b R2, Renderable (Path R2) b) =>
                   SGrid SlalomDiag -> Diagram b R2
drawSlalomDiags = atCentres diag . clues . fmap Just
    where diag SlalomForward  = stroke ur # lwG edgewidth
          diag SlalomBackward = stroke dr # lwG edgewidth

drawCrosses ::  (Backend b R2, Renderable (Path R2) b) =>
                 SGrid Bool -> Diagram b R2
drawCrosses = atCentres (if' drawCross mempty) . values
  where
    if' a b x = if x then a else b


outsideGrid :: (Backend b R2, Renderable (Path R2) b) =>
               OutsideClues [String] -> Diagram b R2
outsideGrid = atCentres (scale 0.8 . drawText) . multiOutsideClues
              <> grid . outsideSize

outsideIntGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  OutsideClues [Int] -> Diagram b R2
outsideIntGrid = atCentres (scale 0.8 . drawInt) . multiOutsideClues
                 <> grid . outsideSize
