{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Draw.Component where

import qualified Data.Map.Strict               as Map

import Diagrams.Prelude (scale, gray)

import           Data.Component
import           Data.Elements
import           Draw.Lib
import           Draw.Draw
import           Draw.Grid
import           Draw.Style
import           Draw.Elements

drawComponent :: Backend' b => Component -> Drawing b
drawComponent c = case c of
  Grid s g ->
    let st = case s of
          GridDefault          -> gDefault
          GridDefaultIrregular -> gDefaultIrreg
          GridDashed           -> gDashed
    in  grid st g
  Regions  g -> drawAreas g
  CellGrid g -> placeGrid . fmap drawDecoration $ g
  NodeGrid g -> placeGrid . fmap drawDecoration $ g
  EdgeGrid g -> placeGrid' . Map.mapKeys midPoint . fmap drawDecoration $ g

drawDecoration :: Backend' b => Decoration -> Drawing b
drawDecoration d = case d of
  Blank          -> mempty
  Letter       c -> drawChar c
  Letters      s -> text' s
  DecKropkiDot k -> kropkiDot k
  AfternoonSouth -> afternoonSouth
  AfternoonWest  -> afternoonWest
  Diagonal diag  -> drawPrimeDiag diag
  Dot            -> scale 0.5 $ smallPearl MBlack
  Shade          -> fillBG gray
