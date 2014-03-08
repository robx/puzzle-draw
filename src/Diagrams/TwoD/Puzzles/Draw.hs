{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude hiding (Point)
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Grid
import Data.Puzzles.Things
import Data.Puzzles.Sudoku

import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Grid
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Things

drawFillo g = drawIntClues g <> dashedgrid x y
    where (x, y) = size g

drawClueGrid g = atCentres drawChar (clues g) <> grid sx sy id
    where (sx, sy) = size g

drawIntClues = atCentres drawInt . clues

drawIntGrid g = drawIntClues g `atop` drawGrid g

drawSlitherGrid g = atCentres drawInt (clues g) `atop` slithergrid sx sy
    where (sx, sy) = size g

drawMasyuGrid g = atCentres pearl (clues g) `atop` grid sx sy id
    where (sx, sy) = size g

drawCompassClues g = atCentres drawCompassClue (clues g)

drawCompassGrid g = drawCompassClues g <> drawGrid g

sudokugrid g = drawedges (sudokubordersg g) `atop` grid sx sy id
    where (sx, sy) = size g

drawWordsClues = atCentres drawWords . clues

drawTightGrid d g = atCentres (drawTight d) (clues . fmap Just $ g)
                    <> grid sx sy id
                    <> phantom (frame (sx + 2) (sy + 2) # translate (r2 (-1,-1)))
    where (sx, sy) = size g

drawSlalomGrid g = atCentres drawSlalomClue (clues g) # translate (r2 (-1/2,-1/2))
                   <> grid (w-1) (h-1) id
                   <> phantom (frame w h) # translate (r2 (-1/2,-1/2))
    where (w, h) = size g

drawSlalomDiags g = atCentres diag (clues (fmap Just g))
    where diag '/' = stroke ur # lw edgewidth
          diag '\\' = stroke dr # lw edgewidth

drawCrosses g = atCentres (const drawCross) (clues g)
