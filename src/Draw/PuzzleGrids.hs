{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ConstraintKinds           #-}

module Draw.PuzzleGrids
  ( drawIntGrid
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
  , layoutRow
  , layoutGrid
  )
where

import           Diagrams.Prelude        hiding ( size
                                                , N
                                                , el
                                                , offset
                                                )

import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( maybeToList
                                                , fromMaybe
                                                )
import           Data.Foldable                  ( fold )

import           Data.Grid
import           Data.GridShape
import           Data.Elements
import           Data.Sudoku

import           Draw.Draw
import           Draw.Lib
import           Draw.Widths
import           Draw.Style
import           Draw.Grid
import           Draw.Elements

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

maxDiam :: Backend' b => V2 Double -> Config -> [Drawing b] -> Double
maxDiam dir cfg ds =
  fromMaybe 0 . fmap getMax . foldMap maxSize . map (diagram cfg) $ ds
  where maxSize = Just . Max . diameter dir

layoutRow :: Backend' b => V2 Double -> [Drawing b] -> Drawing b
layoutRow dir = fold
  . zipWith (\i -> moveTo (origin .+^ fromIntegral i *^ dir)) [(0 :: Int) ..]

layoutGrid :: Backend' b => V2 Double -> V2 Double -> [[Drawing b]] -> Drawing b
layoutGrid dirA dirB = layoutRow dirA . map (layoutRow dirB)

placeSideGrid
  :: Backend' b
  => Double
  -> (Double -> Double)
  -> V2 Double
  -> V2 Double
  -> P2 Double
  -> [[Drawing b]]
  -> Drawing b
placeSideGrid mrg off dir1 dir2 base cs = withConfig place_
 where
  place_ cfg =
    let minDiam    = diameter dir1 (diagram cfg (drawChar 'M') :: D V2 Double)
        elDiam     = max minDiam (maxDiam dir1 cfg (fold cs))
        step       = elDiam + mrg
        offset     = off elDiam
        baseOffset = base .+^ offset *^ dir1
    in  layoutGrid dir2 (step *^ dir1) cs # moveTo baseOffset

placeMultiOutside
  :: (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k)
  => OutsideClues k [Drawing b]
  -> Drawing b
placeMultiOutside ocs = foldMap
  (\(cs, dir1, base, dir2) ->
    placeSideGrid mrg off (r2i dir1) (r2i dir2) (toPoint base) cs
  )
  (outsideClues ocs)
 where
  mrg = 1 / 3
  off elDiam = 1 / 2 * elDiam - 1 / 2 * mrg

placeMultiOutsideGW
  :: (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k)
  => OutsideClues k [Drawing b]
  -> Drawing b
placeMultiOutsideGW ocs = foldMap
  (\(cs, dir1, base, dir2) ->
    placeSideGrid 0 (const (1 / 4)) (r2i dir1) (r2i dir2) (toPoint base) cs
  )
  (outsideClues ocs)

placeOutside
  :: (Backend' b, ToPoint k, FromCoord k, ToCoord k, Ord k)
  => OutsideClues k (Maybe (Drawing b))
  -> Drawing b
placeOutside = placeMultiOutside . fmap maybeToList

drawOutsideGrid :: Backend' b => OutsideClues C (Maybe String) -> Drawing b
drawOutsideGrid =
  placeOutside
    .  fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    .  outsideGrid

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
