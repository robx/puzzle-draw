{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Component where

import Data.Component
import Data.Elements hiding
  ( Black,
    Star,
    Tent,
    Tree,
  )
import qualified Data.Elements
import Data.Grid
import Data.GridShape
import qualified Data.Map.Strict as Map
import Diagrams.Prelude hiding
  ( dot,
    place,
    star,
  )
import Draw.Code
import Draw.Draw
import Draw.Elements
import Draw.Grid
import Draw.Lib
import Draw.PuzzleGrids
import Draw.Style

type GridDrawing b = (Size, Drawing b)

pointWise :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
pointWise f (a, b) (c, d) = (f a c, f b d)

components :: Backend' b => [PlacedComponent (Drawing b)] -> Drawing b
components cs = snd $ go $ reverse cs
  where
    go [] = ((0 :: Int, 0 :: Int), mempty)
    go ((PlacedComponent p c) : pcs) =
      let (sz, dc) = component c
          (szrest, dcs) = go pcs
          maxsz = pointWise max sz szrest
       in case _direction p of
            Atop -> (maxsz, dc <> dcs)
            West -> (szrest, dcs |<| strutX' (margin p) |<| dc)
            North -> (szrest, dcs =^= strutY' (margin p) =^= dc)
            East -> (szrest, dcs |>| strutX' (margin p) |>| dc)
            South -> (szrest, dcs =:= strutY' (margin p) =:= dc)
            TopRight ->
              ( szrest,
                ( dc # alignBL' # translate (0.5 *^ r2i szrest)
                    # translate
                      (r2 (0.6, 0.6))
                )
                  <> dcs
              )
    (=^=) = beside unitY
    (|<|) = beside (negated unitX)
    (=:=) = beside (negated unitY)
    (|>|) = beside unitX
    margin placement = case _margin placement of
      MarginClose -> 1 / 6
      MarginFar -> 1 / 2
      MarginCustom m -> m

component :: Backend' b => Component (Drawing b) -> GridDrawing b
component c = case c of
  RawComponent sz x -> (sz, x)
  Grid s g -> centerGrid (cellSize g, grid (gridStyle s) g)
  Regions g -> centerGrid (cellSize g, areas g)
  CellGrid g -> centerGrid (cellSize g, placeGrid . fmap decoration $ g)
  NodeGrid g -> centerGrid (nodeSize g, placeGrid . fmap decoration $ g)
  EdgeGrid g ->
    centerGrid
      (edgeSize g, placeGrid' . Map.mapKeys midPoint . fmap decoration $ g)
  FullGrid ns cs es ->
    ( nodeSize ns,
      mconcat . map (snd . component) $ [NodeGrid ns, CellGrid cs, EdgeGrid es]
    )
  Note ds -> ((0, 0), note $ hcatSep 0.2 $ map decoration $ ds)
  Pyramid g -> (shiftSize g, shiftGrid g)
  CellPyramid g -> (shiftSize g, placeGrid . fmap decoration $ g)
  Rows rs ->
    centerGrid'
      ( (0, length rs - 1),
        placeSideGrid unitX (- unitY) $ map (map decoration) rs
      )
  Columns cs ->
    centerGrid
      ( (length cs - 1, 0),
        placeSideGrid (- unitY) unitX $ map (map decoration) cs
      )
  where
    gridStyle s = case s of
      GridDefault -> gDefault
      GridDefaultIrregular -> gDefaultIrreg
      GridDashed -> gDashed
      GridDots -> gSlither
      GridPlain -> gPlain
      GridPlainDashed -> gPlainDashed
    centerGrid (sz, d) = (sz, d # translate (-0.5 *^ r2i sz))
    centerGrid' (sz, d) = (sz, d # translate (0.5 *^ r2i sz))

decoration :: Backend' b => Decoration -> Drawing b
decoration d = case d of
  Blank -> mempty
  Letter c -> char c
  Letters s -> text' s
  InvertedLetters s -> invert $ text' s
  DecKropkiDot k -> kropkiDot k
  AfternoonSouth -> afternoonSouth
  AfternoonWest -> afternoonWest
  LightDiagonal diag -> lc (blend 0.5 gray white) $ primeDiag diag
  DarkDiagonal diag -> lc gray $ primeDiag diag
  SmallDot -> dot
  Dot -> scale 0.5 $ smallPearl MBlack
  Star -> star Data.Elements.Star
  Shade -> fillBG gray
  DarkShade -> fillBG (blend 0.5 gray black)
  Black -> fillBG black
  LightShade -> fillBG (blend 0.5 gray white)
  SmallPearl p -> smallPearl p
  Pearl p -> pearl p
  Edge dir -> edgeDecoration dir
  ThinEdge dir -> edgeDecorationThin dir
  SolEdge dir -> edgeDecorationSol dir
  TriangleRight -> arrowRight
  TriangleDown -> arrowDown
  LabeledTriangleRight w -> arrowRightL w
  LabeledTriangleDown w -> arrowDownL w
  MiniLoop -> miniloop
  ShipSquare -> shipSquare
  Ship dir -> shipEnd dir
  LabeledArrow dir w -> labeledArrow dir $ text' w
  InvertedLabeledArrow dir w -> invert $ labeledArrow dir $ text' w
  Tent -> draw tentDia
  Tree -> tree Data.Elements.Tree
  Myopia dirs -> myopia dirs
  Triangle dir -> cornerTriangle dir
