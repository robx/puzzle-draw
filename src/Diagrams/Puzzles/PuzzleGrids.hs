{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Diagrams.Puzzles.PuzzleGrids
    (
      drawIntGrid
    , drawCharGrid
    , outsideIntGrid
    , drawSlitherGrid
    , drawTightGrid
    , sudokugrid
    , drawWordsClues
    , drawOutsideGrid
    , drawMultiOutsideGrid
    , drawOutsideGridN
    , drawMultiOutsideGridN
    ) where

import Diagrams.Prelude

import qualified Data.Map as Map
import Data.Maybe (maybeToList)

import Data.Puzzles.Grid
import Data.Puzzles.GridShape
import Data.Puzzles.Elements
import Data.Puzzles.Sudoku

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Style
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.Elements

drawCharGrid :: Backend' b =>
                Grid C (Maybe Char) -> Diagram b R2
drawCharGrid = placeGrid . fmap drawChar . clues <> grid gDefault

drawIntGrid :: Backend' b =>
               Grid C (Maybe Int) -> Diagram b R2
drawIntGrid = placeGrid . fmap drawInt . clues  <> grid gDefault

drawSlitherGrid :: Backend' b =>
                   Grid C (Maybe Int) -> Diagram b R2
drawSlitherGrid = placeGrid . fmap  drawInt . clues <> grid gSlither

sudokugrid :: Backend' b =>
              Grid C a -> Diagram b R2
sudokugrid = drawEdges . sudokubordersg <> grid gDefault

drawWordsClues :: Backend' b =>
                  Grid C (Maybe [String]) -> Diagram b R2
drawWordsClues = placeGrid . fmap drawWords . clues

drawTightGrid :: Backend' b =>
                 (t -> Diagram b R2) -> Grid C (Tightfit t) -> Diagram b R2
drawTightGrid d g = (placeGrid . fmap (drawTight d) $ g)
                    <> grid gDefault g
                    <> phantom' (stroke $ p2i (-1,-1) ~~ p2i (sx + 1, sy + 1))
    where (sx, sy) = size (Map.mapKeys toCoord g)

placeMultiOutside :: (Ord k, FromCoord k, ToPoint k,
                      HasOrigin a, Transformable a, V a ~ R2, Monoid a)
                  => OutsideClues k [a] -> a
placeMultiOutside = placeGrid . multiOutsideClues

placeOutside :: (Ord k, FromCoord k, ToPoint k,
                HasOrigin a, Transformable a, V a ~ R2, Monoid a)
             => OutsideClues k (Maybe a) -> a
placeOutside = placeMultiOutside . fmap maybeToList

drawOutsideGrid :: Backend' b => OutsideClues C (Maybe String) -> Diagram b R2
drawOutsideGrid = placeOutside . fmap (fmap (scale 0.8 . drawText))
                  <> grid gDefault . outsideGrid

drawOutsideGridN :: Backend' b => OutsideClues N (Maybe String) -> Diagram b R2
drawOutsideGridN = placeOutside . fmap (fmap (scale 0.8 . drawText))
                  <> grid gDefault . cellGrid . outsideGrid

drawMultiOutsideGrid :: Backend' b => OutsideClues C [String] -> Diagram b R2
drawMultiOutsideGrid = placeMultiOutside . fmap (fmap (scale 0.8 . drawText))
                     <> grid gDefault . outsideGrid

drawMultiOutsideGridN :: Backend' b => OutsideClues N [String] -> Diagram b R2
drawMultiOutsideGridN = placeMultiOutside . fmap (fmap (scale 0.8 . drawText))
                      <> grid gDefault . cellGrid . outsideGrid

outsideIntGrid :: Backend' b => OutsideClues C [Int] -> Diagram b R2
outsideIntGrid = drawMultiOutsideGrid . fmap (fmap show)
