{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Code
  ( code,
    arrowRight,
    arrowRightL,
    arrowDown,
    arrowDownL,
  )
where

import Data.Code
import Data.Component
import Data.Grid
import Data.GridShape
import qualified Data.Map.Strict as Map
import Diagrams.Prelude hiding
  ( matching,
    parts,
    place,
  )
import Draw.Draw
import Draw.Elements
import Draw.Grid
import Draw.Lib

code :: Backend' b => Code -> [TaggedComponent (Drawing b)]
code cs = concat [collect Atop, collect West, collect North]
  where
    parts = map codePart cs
    collect p =
      let matching = map snd . filter ((==) p . fst) $ parts
       in if null matching then [] else [comp p $ mconcat matching]
    fakeSize = (0, 0) -- should be the dimensions of the code part
    comp p d =
      TaggedComponent (Just Code) $ PlacedComponent p $ RawComponent fakeSize $ d

codePart :: Backend' b => CodePart -> (Placement, Drawing b)
codePart cp = case cp of
  Rows' rs -> (West, placeGrid g # centerX')
    where
      g = Map.fromList [(C 1 r, arrowRight) | r <- rs]
  Cols cs -> (North, placeGrid g # centerY')
    where
      g = Map.fromList [(C c 0, arrowDown) | c <- cs]
  RowsN' rs -> (West, placeGrid g # centerX')
    where
      g = Map.fromList [(N 0 r, arrowRight) | r <- rs]
  ColsN cs -> (North, placeGrid g # centerY')
    where
      g = Map.fromList [(N c 0, arrowDown) | c <- cs]
  LabelsN g -> (Atop, placeGrid . fmap label . clues $ g)
    where
      label c = char c # scale 0.5 # fc gray # translate (r2 (1 / 3, -1 / 3))
  LRows' rs -> (West, placeGrid g # centerX')
    where
      g = Map.fromList [(C 0 r, arrowRightL [l]) | (l, r) <- Map.toList rs]
  LCols cs -> (North, placeGrid g # centerY')
    where
      g = Map.fromList [(C c 0, arrowDownL [l]) | (l, c) <- Map.toList cs]
  LRowsN' rs -> (West, placeGrid g # centerX')
    where
      g = Map.fromList [(N 0 r, arrowRightL [l]) | (l, r) <- Map.toList rs]
  LColsN cs -> (North, placeGrid g # centerY')
    where
      g = Map.fromList [(N c 0, arrowDownL [l]) | (l, c) <- Map.toList cs]

arrowDown :: Backend' b => Drawing b
arrowDown = draw $ triangle 0.5 # lwG 0 # fc black # rotateBy (1 / 2)

arrowDownL :: Backend' b => String -> Drawing b
arrowDownL c = text' c # fc white # scale 0.5 <> arrowDown # scale 1.2

arrowRight :: Backend' b => Drawing b
arrowRight = arrowDown # rotateBy (1 / 4)

arrowRightL :: Backend' b => String -> Drawing b
arrowRightL c =
  text' c
    # fc white
    # scale 0.5
    # translate (r2 (-0.05, 0))
    <> arrowRight
    # scale 1.2
