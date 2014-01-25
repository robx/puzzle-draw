{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Pyramid
import Diagrams.TwoD.Puzzles.Draw (frame)
import Data.Puzzles.Pyramid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

getPyramid = liftM (readKropkiPyramid . lines) getContents

main = liftM (frame . kpyramid) getPyramid >>= defaultMain
