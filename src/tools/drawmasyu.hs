{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

readGrid = liftM (fmap charToMasyuClue . fromListList . filter (not . null) . lines) getContents

main = liftM (bg white . frame . drawMasyuGrid) readGrid >>= defaultMain
