{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

thermos = map (thermo . map p2)
          [ [(1, 0), (1, 1), (2, 1), (2, 2)]
          , [(3, 3), (4, 4), (5, 3)]
          , [(6, 0), (6, 1), (7, 2), (7, 3), (8, 4), (7, 5), (6, 5)]
          ]

d = (sudokugrid `atop` mconcat thermos) # frame

main = defaultMain d
