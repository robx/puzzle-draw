{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Draw.Component where

import qualified Data.Map.Strict               as Map

import           Diagrams.Prelude        hiding ( dot
                                                , place
                                                )

import           Data.Component
import           Data.Elements
import           Draw.Lib
import           Draw.Draw
import           Draw.Grid
import           Draw.Style
import           Draw.Elements
import           Draw.Code

drawComponents :: Backend' b => [PlacedComponent] -> Drawing b
drawComponents cs = go $ reverse cs
 where
  go [] = mempty
  go ((PlacedComponent p c) : pcs) =
    let dc  = drawComponent c
        dcs = go pcs
    in  case p of
          Atop  -> dc <> dcs
          West  -> dcs |!| strutX' 0.5 |!| dc
          North -> dcs =!= strutY' 0.5 =!= dc
  (=!=) = beside unitY
  (|!|) = beside (negated unitX)

drawComponent :: Backend' b => Component -> Drawing b
drawComponent c = case c of
  Grid s g ->
    let st = case s of
          GridDefault          -> gDefault
          GridDefaultIrregular -> gDefaultIrreg
          GridDashed           -> gDashed
          GridDots             -> gSlither
    in  grid st g
  Regions  g -> drawAreas g
  CellGrid g -> placeGrid . fmap drawDecoration $ g
  NodeGrid g -> placeGrid . fmap drawDecoration $ g
  EdgeGrid g -> placeGrid' . Map.mapKeys midPoint . fmap drawDecoration $ g
  FullGrid ns cs es ->
    mconcat . map drawComponent $ [NodeGrid ns, CellGrid cs, EdgeGrid es]

drawDecoration :: Backend' b => Decoration -> Drawing b
drawDecoration d = case d of
  Blank                  -> mempty
  Letter       c         -> drawChar c
  Letters      s         -> text' s
  DecKropkiDot k         -> kropkiDot k
  AfternoonSouth         -> afternoonSouth
  AfternoonWest          -> afternoonWest
  LightDiagonal diag     -> lc (blend 0.5 gray white) $ drawPrimeDiag diag
  DarkDiagonal  diag     -> lc gray $ drawPrimeDiag diag
  SmallDot               -> dot
  Dot                    -> scale 0.5 $ smallPearl MBlack
  Shade                  -> fillBG gray
  SmallPearl p           -> smallPearl p
  Pearl      p           -> pearl p
  Edge       dir         -> edgeDecoration dir
  ThinEdge   dir         -> edgeDecorationThin dir
  SolEdge    dir         -> edgeDecorationSol dir
  TriangleRight          -> arrowRight
  TriangleDown           -> arrowDown
  LabeledTriangleRight w -> arrowRightL w
  LabeledTriangleDown  w -> arrowDownL w

