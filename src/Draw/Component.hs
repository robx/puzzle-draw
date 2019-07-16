{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Draw.Component where

import qualified Data.Map.Strict               as Map

import           Diagrams.Prelude        hiding ( dot
                                                , place
                                                )

import           Data.Component
import           Data.Elements           hiding ( Tent
                                                , Tree
                                                , Black
                                                , Star
                                                )
import qualified Data.Elements
import           Data.Grid
import           Data.GridShape
import           Draw.Lib
import           Draw.Draw
import           Draw.Grid
import           Draw.Style
import           Draw.Elements
import           Draw.Code

drawComponents :: Backend' b => [PlacedComponent (Drawing b)] -> Drawing b
drawComponents cs = snd $ go $ reverse cs
 where
  go [] = ((0 :: Int, 0 :: Int), mempty)
  go ((PlacedComponent p c) : pcs)
    = let
        (tl , dc ) = drawComponent c
        (tls, dcs) = go pcs
        ntl        = (max (fst tl) (fst tls), max (snd tl) (snd tls))
      in
        case p of
          Atop  -> (ntl, dc <> dcs)
          West  -> (ntl, dcs |!| strutX' 0.5 |!| dc)
          North -> (ntl, dcs =!= strutY' 0.5 =!= dc)
          TopRight ->
            ( ntl
            , (dc # alignBL' # translatep tls # translate (r2 (0.6, 0.6)))
              <> dcs
            )
  (=!=) = beside unitY
  (|!|) = beside (negated unitX)

drawComponent :: Backend' b => Component (Drawing b) -> (Size, Drawing b)
drawComponent c = case c of
  RawComponent sz x -> (sz, x)
  Grid         s  g -> (cellSize g, grid (gridStyle s) g)
  Regions  g        -> (cellSize g, drawAreas g)
  CellGrid g        -> (cellSize g, placeGrid . fmap drawDecoration $ g)
  NodeGrid g        -> (nodeSize g, placeGrid . fmap drawDecoration $ g)
  EdgeGrid g ->
    (edgeSize g, placeGrid' . Map.mapKeys midPoint . fmap drawDecoration $ g)
  FullGrid ns cs es ->
    ( nodeSize ns
    , mconcat
      . map (snd . drawComponent)
      $ [NodeGrid ns, CellGrid cs, EdgeGrid es]
    )
  Note        ds -> ((0, 0), note $ hcatSep 0.2 $ map drawDecoration $ ds)
  Pyramid     g  -> (shiftSize g, shiftGrid g)
  CellPyramid g  -> (shiftSize g, placeGrid . fmap drawDecoration $ g)
 where
  gridStyle s = case s of
    GridDefault          -> gDefault
    GridDefaultIrregular -> gDefaultIrreg
    GridDashed           -> gDashed
    GridDots             -> gSlither
    GridPlain            -> gPlain
    GridPlainDashed      -> gPlainDashed

drawDecoration :: Backend' b => Decoration -> Drawing b
drawDecoration d = case d of
  Blank                  -> mempty
  Letter       c         -> drawChar c
  Letters      s         -> text' s
  InvertedLetters s      -> invert $ text' s
  DecKropkiDot k         -> kropkiDot k
  AfternoonSouth         -> afternoonSouth
  AfternoonWest          -> afternoonWest
  LightDiagonal diag     -> lc (blend 0.5 gray white) $ drawPrimeDiag diag
  DarkDiagonal  diag     -> lc gray $ drawPrimeDiag diag
  SmallDot               -> dot
  Dot                    -> scale 0.5 $ smallPearl MBlack
  Star                   -> drawStar Data.Elements.Star
  Shade                  -> fillBG gray
  Black                  -> fillBG black
  LightShade             -> fillBG (blend 0.5 gray white)
  SmallPearl p           -> smallPearl p
  Pearl      p           -> pearl p
  Edge       dir         -> edgeDecoration dir
  ThinEdge   dir         -> edgeDecorationThin dir
  SolEdge    dir         -> edgeDecorationSol dir
  TriangleRight          -> arrowRight
  TriangleDown           -> arrowDown
  LabeledTriangleRight w -> arrowRightL w
  LabeledTriangleDown  w -> arrowDownL w
  MiniLoop               -> miniloop
  ShipSquare             -> shipSquare
  Ship dir               -> shipEnd dir
  LabeledArrow dir w     -> labeledArrow dir $ text' w
  InvertedLabeledArrow dir w -> invert $ labeledArrow dir $ text' w
  Tent                   -> draw tent
  Tree                   -> drawTree Data.Elements.Tree
