{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.PuzzleGrids
  ( intGrid,
    charGrid,
    outsideIntGrid,
    slitherGrid,
    tightGrid,
    sudokugrid,
    wordsClues,
    outsideGrid,
    multiOutsideGrid,
    placeOutside,
    placeMultiOutside,
    placeMultiOutsideGW,
    layoutRow,
    layoutGrid,
    placeSideGrid,
  )
where

import Data.Elements
import Data.Foldable (fold)
import qualified Data.Grid as Data
import Data.Grid
  ( Grid,
    OutsideClues,
    clues,
    size,
  )
import Data.GridShape
  ( C,
    FromCoord (..),
    ToCoord (..),
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
    maybeToList,
  )
import Data.Sudoku
import Diagrams.Prelude hiding
  ( N,
    el,
    offset,
    size,
  )
import Draw.Draw
import Draw.Elements
import Draw.Grid
import Draw.GridShape
import Draw.Lib
import Draw.Style
import Draw.Widths

charGrid :: Backend' b => Grid C (Maybe Char) -> Drawing b
charGrid = placeGrid . fmap char . clues <> grid gDefault

intGrid :: Backend' b => Grid C (Maybe Int) -> Drawing b
intGrid = placeGrid . fmap int . clues <> grid gDefault

slitherGrid :: Backend' b => Grid C (Maybe Int) -> Drawing b
slitherGrid = placeGrid . fmap int . clues <> grid gSlither

sudokugrid :: Backend' b => Grid C a -> Drawing b
sudokugrid = edges . sudokubordersg <> grid gDefault

wordsClues :: Backend' b => Grid C (Maybe [String]) -> Drawing b
wordsClues = placeGrid . fmap Draw.Elements.words . clues

tightGrid :: Backend' b => (t -> Drawing b) -> Grid C (Tightfit t) -> Drawing b
tightGrid d g =
  (placeGrid . fmap (tight d) $ g) <> grid gDefault g
    <> draw
      (phantom' (strokePath $ p2i (-1, -1) ~~ p2i (sx + 1, sy + 1)))
  where
    (sx, sy) = size (Map.mapKeys toCoord g)

maxDiam :: Backend' b => V2 Double -> Config -> [Drawing b] -> Double
maxDiam dir cfg ds =
  fromMaybe 0 . fmap getMax . foldMap maxSize . map (diagram cfg) $ ds
  where
    maxSize = Just . Max . diameter dir

layoutRow :: Backend' b => V2 Double -> [Drawing b] -> Drawing b
layoutRow dir =
  fold
    . zipWith (\i -> moveTo (origin .+^ fromIntegral i *^ dir)) [(0 :: Int) ..]

layoutGrid :: Backend' b => V2 Double -> V2 Double -> [[Drawing b]] -> Drawing b
layoutGrid dirA dirB = layoutRow dirA . map (layoutRow dirB)

placeSideGrid ::
  Backend' b => V2 Double -> V2 Double -> [[Drawing b]] -> Drawing b
placeSideGrid dir1 dir2 cs = withConfig place_
  where
    place_ cfg =
      let minDiam = diameter dir1 (diagram cfg (char 'M') :: D V2 Double)
          elDiam = max minDiam (maxDiam dir1 cfg (fold cs))
          -- we want the distance of the first outside clue to not depend
          -- on the maximal width of the outside clues, for consistency
          -- across puzzles; thus, strut out the first element
          cs' = map (mapHead (\d -> d <> strutR2' (0.5 * step *^ dir1) # align' dir1)) $ cs
          mapHead f xs = case xs of
            (y : ys) -> (f y) : ys
            [] -> []
          mrg = 1 / 3
          step = elDiam + mrg
       in layoutGrid dir2 (step *^ dir1) cs' # align' (- dir1)

placeMultiOutside ::
  (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k) =>
  OutsideClues k [Drawing b] ->
  Drawing b
placeMultiOutside ocs =
  foldMap
    ( \(cs, dir1, base, dir2) ->
        placeSideGrid (r2i dir1) (r2i dir2) cs
          -- FIXME: I doubt this works for k ~ N
          # moveTo (toPoint base .-^ 1 / 3 *^ r2i dir1)
    )
    (Data.outsideClues ocs)

-- | clue placement for greater wall
placeMultiOutsideGW ::
  (Backend' b, FromCoord k, ToCoord k, ToPoint k, Ord k) =>
  OutsideClues k [Drawing b] ->
  Drawing b
placeMultiOutsideGW ocs =
  foldMap
    ( \(cs, dir1, base, dir2) ->
        layoutGrid (r2i dir2) (2 / 3 *^ r2i dir1) cs # moveTo (toPoint base .+^ 1 / 4 *^ r2i dir1)
    )
    (Data.outsideClues ocs)

placeOutside ::
  (Backend' b, ToPoint k, FromCoord k, ToCoord k, Ord k) =>
  OutsideClues k (Maybe (Drawing b)) ->
  Drawing b
placeOutside = placeMultiOutside . fmap maybeToList

outsideGrid :: Backend' b => OutsideClues C (Maybe String) -> Drawing b
outsideGrid =
  placeOutside
    . fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    . Data.outsideGrid

multiOutsideGrid :: Backend' b => OutsideClues C [String] -> Drawing b
multiOutsideGrid =
  placeMultiOutside
    . fmap (fmap (scale outsideScale . text'))
    <> grid gDefault
    . Data.outsideGrid

outsideIntGrid :: Backend' b => OutsideClues C [Int] -> Drawing b
outsideIntGrid = multiOutsideGrid . fmap (fmap show)
