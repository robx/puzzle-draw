{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude

import Data.Puzzles.Grid
import Data.Puzzles.Things
import Data.Puzzles.Sudoku

import Diagrams.TwoD.Puzzles.Grid
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Things

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
              SGrid (Clue Int) -> Diagram b R2
sudokugrid = drawEdges . sudokubordersg  <> grid . size

drawWordsClues :: (Backend b R2, Renderable (Path R2) b) =>
                  SGrid (Clue [String]) -> Diagram b R2
drawWordsClues = atCentres drawWords . clues

drawTightGrid :: (Backend b R2, Renderable (Path R2) b) =>
                 (t -> Diagram b R2) -> SGrid (Tightfit t) -> Diagram b R2
drawTightGrid d g = atCentres (drawTight d) (clues . fmap Just $ g)
                    <> grid (size g)
                    <> phantom (frame (sx + 2, sy + 2) # translate (r2 (-1,-1)))
    where (sx, sy) = size g

drawSlalomGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  SGrid (Clue Int) -> Diagram b R2
drawSlalomGrid g = atVertices drawSlalomClue (clues g)
                   <> grid (w-1, h-1)
                   <> phantom (frame (size g)) # translate (r2 (-1/2,-1/2))
    where (w, h) = size g

drawSlalomDiags :: (Backend b R2, Renderable (Path R2) b) =>
                   SGrid SlalomDiag -> Diagram b R2
drawSlalomDiags = atCentres diag . clues . fmap Just
    where diag SlalomForward  = stroke ur # lw edgewidth
          diag SlalomBackward = stroke dr # lw edgewidth

drawCrosses ::  (Backend b R2, Renderable (Path R2) b) =>
                 SGrid (Maybe a) -> Diagram b R2
drawCrosses = atCentres (const drawCross) . clues
