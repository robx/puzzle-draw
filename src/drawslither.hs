{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Draw
import Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

readGrid = liftM (NG . filter (not . null) . lines) getContents

main = liftM (padc . drawSlitherGrid) readGrid >>= defaultMain
