{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fromStr = fmap charToCharClue . fromListList . filter (not . null) . lines
readGrid = liftM fromStr getContents

d g = (drawClues drawChar (clues g) `atop` fillogrid sx sy) # frame # bg white
    where (sx, sy) = size g

main = liftM d readGrid >>= defaultMain
