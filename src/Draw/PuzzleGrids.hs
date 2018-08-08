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
    , placeOutside
    , placeMultiOutside
    , placeMultiOutsideGW
    ) where

import Diagrams.Prelude hiding (size, N)

import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList, fromMaybe)
import Data.Foldable (fold)

import Data.Grid
import Data.GridShape
import Data.Elements
import Data.Sudoku

import Draw.Draw
import Draw.Lib
import Draw.Widths
import Draw.Style
import Draw.Grid
import Draw.Elements

drawCharGrid :: Backend' b => Grid C (Maybe Char) -> Drawing b
drawCharGrid = placeGrid . fmap drawChar . clues <> grid gDefault

drawIntGrid :: Backend' b => Grid C (Maybe Int) -> Drawing b
drawIntGrid = placeGrid . fmap drawInt . clues <> grid gDefault

drawSlitherGrid :: Backend' b => Grid C (Maybe Int) -> Drawing b
drawSlitherGrid = placeGrid . fmap drawInt . clues <> grid gSlither

sudokugrid :: Backend' b => Grid C a -> Drawing b
sudokugrid = drawEdges . sudokubordersg <> grid gDefault

drawWordsClues :: Backend' b => Grid C (Maybe [String]) -> Drawing b
drawWordsClues = placeGrid . fmap drawWords . clues

drawTightGrid
  :: Backend' b => (t -> Drawing b) -> Grid C (Tightfit t) -> Drawing b
drawTightGrid d g =
  (placeGrid . fmap (drawTight d) $ g) <> grid gDefault g <> draw
    (phantom' (strokePath $ p2i (-1, -1) ~~ p2i (sx + 1, sy + 1)))
  where (sx, sy) = size (Map.mapKeys toCoord g)

placeMultiOutside
  :: (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k)
  => OutsideClues k [Drawing b]
  -> Drawing b
placeMultiOutside ocs = Drawing pmo
 where
  pmo cfg = foldMap (place_ cfg) (multiOutsideClues ocs)
  place_ cfg (clueSets, dir) =
    let
      clueSetsD = fmap (map (diagram cfg)) $ clueSets
      minDiam   = diameter (r2i dir) (diagram cfg (drawChar 'M') :: D V2 Double)
      elt =
        max minDiam
          . fromMaybe 0
          . fmap getMax
          . foldMap (maxSize dir)
          $ clueSetsD
      mrg = 1/3
      pt :: (ToPoint k) => k -> Int -> P2 Double
      pt base i =
        toPoint base
          .+^ (  (-(1 / 2) + mrg + (1 / 2) * elt + fromIntegral i * (elt + mrg))
              *^ r2i dir
              )
      placeRow base ds = zipWith (\d i -> d # moveTo (pt base i)) ds [0 ..]
    in
      fold $ Map.foldMapWithKey placeRow clueSetsD
  maxSize dir = foldMap (Just . Max . diameter (r2i dir))

placeMultiOutsideGW
  :: (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k)
  => OutsideClues k [Drawing b]
  -> Drawing b
placeMultiOutsideGW ocs = Drawing pmo
 where
  pmo cfg = foldMap (place_ cfg) (multiOutsideClues ocs)
  place_ cfg (clueSets, dir) =
    let
      clueSetsD = fmap (map (diagram cfg)) $ clueSets
      elt       = fromMaybe 0 . fmap getMax . foldMap (maxSize dir) $ clueSetsD
      pt :: (ToPoint k) => k -> Int -> P2 Double
      pt base i = toPoint base .+^ ((1 / 4 + fromIntegral i * elt) *^ r2i dir)
      placeRow base ds = zipWith (\d i -> d # moveTo (pt base i)) ds [0 ..]
    in
      fold $ Map.foldMapWithKey placeRow clueSetsD
  maxSize dir = foldMap (Just . Max . diameter (r2i dir))

placeOutside
  :: (Backend' b, ToPoint k, FromCoord k, ToCoord k, Ord k)
  => OutsideClues k (Maybe (Drawing b))
  -> Drawing b
placeOutside = placeMultiOutside . fmap maybeToList

drawOutsideGrid :: Backend' b => OutsideClues C (Maybe String) -> Drawing b
drawOutsideGrid =
  placeOutside . fmap (fmap (scale outsideScale . text')) <> grid gDefault . outsideGrid

drawOutsideGridN :: Backend' b => OutsideClues N (Maybe String) -> Drawing b
drawOutsideGridN =
  placeOutside
    .  fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    .  cellGrid
    .  outsideGrid

drawMultiOutsideGrid :: Backend' b => OutsideClues C [String] -> Drawing b
drawMultiOutsideGrid =
  placeMultiOutside
    .  fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    .  outsideGrid

drawMultiOutsideGridN :: Backend' b => OutsideClues N [String] -> Drawing b
drawMultiOutsideGridN =
  placeMultiOutside
    .  fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    .  cellGrid
    .  outsideGrid

outsideIntGrid :: Backend' b => OutsideClues C [Int] -> Drawing b
outsideIntGrid = drawMultiOutsideGrid . fmap (fmap show)
