{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Draw.Component where

import qualified Data.Map.Strict               as Map

import           Diagrams.Prelude               ( scale
                                                , gray
                                                )

import           Data.Component
import           Data.Elements
import           Draw.Lib
import           Draw.Draw
import           Draw.Grid
import           Draw.Style
import           Draw.Elements

drawComponents :: Backend' b => [Component] -> Drawing b
drawComponents cs = mconcat $ reverse $ map drawComponent $ cs

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
  FullGrid ns cs es ->
    mconcat . map drawComponent $ [NodeGrid ns, CellGrid cs, EdgeGrid es]

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
  SmallPearl p   -> smallPearl p
  Pearl      p   -> pearl p
  Edge       dir -> edgeDecoration dir
  ThinEdge   dir -> edgeDecorationThin dir
  SolEdge    dir -> edgeDecorationSol dir
