{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Pyramid
import Diagrams.TwoD.Puzzles.Draw (padc)
import Data.Puzzles.Pyramid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

getPyramid = liftM (readPyramid . lines) getContents

main = liftM (padc . pyramid) getPyramid >>= defaultMain
