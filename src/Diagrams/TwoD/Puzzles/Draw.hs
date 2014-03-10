{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Grid
import Data.Puzzles.Things
import Data.Puzzles.Sudoku

import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Grid
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Things

drawFillo = drawIntClues <> dashedgrid . size
drawClueGrid = atCentres drawChar . clues <> grid . size
drawIntClues = atCentres drawInt . clues
drawIntGrid = drawIntClues <> grid . size
drawSlitherGrid = atCentres drawInt . clues <> slithergrid . size
drawMasyuGrid = atCentres pearl . clues <> grid . size
drawCompassClues = atCentres drawCompassClue . clues
drawCompassGrid = drawCompassClues <> grid . size
sudokugrid = drawedges . sudokubordersg  <> grid . size
drawWordsClues = atCentres drawWords . clues

drawTightGrid d g = atCentres (drawTight d) (clues . fmap Just $ g)
                    <> grid (size g)
                    <> phantom (frame (sx + 2, sy + 2) # translate (r2 (-1,-1)))
    where (sx, sy) = size g

drawSlalomGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  Grid (Clue Int) -> Diagram b R2
drawSlalomGrid g = atCentres drawSlalomClue (clues g) # translate (r2 (-1/2,-1/2))
                   <> grid (w-1, h-1)
                   <> phantom (frame (size g)) # translate (r2 (-1/2,-1/2))
    where (w, h) = size g

drawSlalomDiags :: (Backend b R2, Renderable (Path R2) b) =>
                   Grid Char -> Diagram b R2
drawSlalomDiags = atCentres diag . clues . fmap Just
    where diag '/'  = stroke ur # lw edgewidth
          diag '\\' = stroke dr # lw edgewidth

drawCrosses = atCentres (const drawCross) . clues
