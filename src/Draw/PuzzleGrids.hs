{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Draw.PuzzleGrids
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
    , placeMultiOutside
    ) where

import Diagrams.Prelude hiding (size, N)

import qualified Data.Map as Map
import Data.Maybe (maybeToList)

import Data.Grid
import Data.GridShape
import Data.Elements
import Data.Sudoku

import Draw.Lib
import Draw.Style
import Draw.Grid
import Draw.Elements

drawCharGrid :: Backend' b =>
                Grid C (Maybe Char) -> Diagram b
drawCharGrid = placeGrid . fmap drawChar . clues <> grid gDefault

drawIntGrid :: Backend' b =>
               Grid C (Maybe Int) -> Diagram b
drawIntGrid = placeGrid . fmap drawInt . clues  <> grid gDefault

drawSlitherGrid :: Backend' b =>
                   Grid C (Maybe Int) -> Diagram b
drawSlitherGrid = placeGrid . fmap  drawInt . clues <> grid gSlither

sudokugrid :: Backend' b =>
              Grid C a -> Diagram b
sudokugrid = drawEdges . sudokubordersg <> grid gDefault

drawWordsClues :: Backend' b =>
                  Grid C (Maybe [String]) -> Diagram b
drawWordsClues = placeGrid . fmap drawWords . clues

drawTightGrid :: Backend' b =>
                 (t -> Diagram b) -> Grid C (Tightfit t) -> Diagram b
drawTightGrid d g = (placeGrid . fmap (drawTight d) $ g)
                    <> grid gDefault g
                    <> phantom' (strokePath $ p2i (-1,-1) ~~ p2i (sx + 1, sy + 1))
    where (sx, sy) = size (Map.mapKeys toCoord g)

placeMultiOutside :: (Ord k, FromCoord k, ToPoint k,
                      HasOrigin a, Monoid a,
                      InSpace V2 Double a)
                  => OutsideClues k [a] -> a
placeMultiOutside = placeGrid . multiOutsideClues

placeOutside :: (Ord k, FromCoord k, ToPoint k,
                 HasOrigin a, Monoid a,
                 InSpace V2 Double a)
             => OutsideClues k (Maybe a) -> a
placeOutside = placeMultiOutside . fmap maybeToList

drawOutsideGrid :: Backend' b => OutsideClues C (Maybe String) -> Diagram b
drawOutsideGrid = placeOutside . fmap (fmap (scale 0.8 . drawText))
                  <> grid gDefault . outsideGrid

drawOutsideGridN :: Backend' b => OutsideClues N (Maybe String) -> Diagram b
drawOutsideGridN = placeOutside . fmap (fmap (scale 0.8 . drawText))
                  <> grid gDefault . cellGrid . outsideGrid

drawMultiOutsideGrid :: Backend' b => OutsideClues C [String] -> Diagram b
drawMultiOutsideGrid = placeMultiOutside . fmap (fmap (scale 0.8 . drawText))
                     <> grid gDefault . outsideGrid

drawMultiOutsideGridN :: Backend' b => OutsideClues N [String] -> Diagram b
drawMultiOutsideGridN = placeMultiOutside . fmap (fmap (scale 0.8 . drawText))
                      <> grid gDefault . cellGrid . outsideGrid

outsideIntGrid :: Backend' b => OutsideClues C [Int] -> Diagram b
outsideIntGrid = drawMultiOutsideGrid . fmap (fmap show)
