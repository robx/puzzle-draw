{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

readGrid = liftM (CG . filter (not . null) . lines) getContents

main = liftM (frame . drawCharGridG) readGrid >>= defaultMain
