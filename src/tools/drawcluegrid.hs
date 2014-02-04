{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

readGrid = liftM (fmap charToCharClue . fromListList . filter (not . null) . lines) getContents

main = liftM (frame . drawClueGrid) readGrid >>= defaultMain
