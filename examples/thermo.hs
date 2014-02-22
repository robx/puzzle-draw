{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

thermos = map (thermo . map p2)
          [ [(0, 4), (0, 8)]
          , [(0, 3), (0, 0), (2, 0)]
          , [(3, 0), (8, 0), (8, 2)]
          , [(8, 5), (8, 3)]
          , [(8, 8), (8, 6)]
          , [(5, 8), (7, 8)]
          , [(1, 8), (4, 8)]
          , [(2, 2), (6, 2)]
          , [(2, 4), (6, 4), (6, 3)]
          , [(2, 6), (6, 6), (6, 5)]
          ]

clues = [ ((1, 1), 1), ((1, 7), 3), ((7, 1), 2), ((7, 7), 7) ]

d = (drawClues drawInt clues `atop` sudokugrid `atop` mconcat thermos) # frame

main = defaultMain d
