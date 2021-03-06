{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Grid where

import qualified Data.AffineSpace as AS
import Data.Char (isUpper)
import Data.Grid
import qualified Data.GridShape as Data
import Data.GridShape
  ( C,
    Coord,
    Dir (..),
    Edge (..),
    Edge',
    N,
    ShiftC,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Util
import Diagrams.Prelude hiding
  ( E,
    N,
    dot,
    offset,
    outer,
    size,
    unit,
  )
import Diagrams.TwoD.Offset (offsetPath)
import Draw.Draw hiding (border)
import Draw.GridShape
import Draw.Lib
import Draw.Style
import Draw.Widths

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Drawing b
dot = draw $ circle 0.05 # fc black # smash

gridCell :: Backend' b => Drawing b
gridCell = draw $ square 1 # lwG onepix

grid :: Backend' b => GridStyle -> Grid C a -> Drawing b
grid s g =
  (placeGrid . fmap (const vertex) . nodeGrid $ g)
    <> Drawing (\c -> stroke inner # linestyle c (_line s))
    <> Drawing (\c -> stroke outer # linestyle c (_border s))
    <> Drawing (\c -> frm c)
  where
    vertex = case _vertex s of
      VertexDot -> dot
      VertexNone -> mempty
    edgeWidth cfg = case _cfgDevice cfg of
      Screen -> edgewidth
      Print -> 2 * edgewidth / 3
    linestyle cfg ls =
      let gw = case _cfgDevice cfg of
            Screen -> linewidth
            Print -> linewidth / 2
          ew = edgeWidth cfg
       in case ls of
            LineNone -> const mempty
            LineThin -> lwG gw
            LineDashed -> gridDashing . lwG gw
            LineThick -> lwG ew
    frm cfg = case _frame s of
      Just (FrameStyle f c) -> outLine cfg f outer # fc c
      Nothing -> mempty
    outLine cfg f p = lwG 0 . stroke $ pin <> pout
      where
        pout = reversePath $ offsetPath (f * w - e) p
        pin = offsetPath (- e) p
        e = w / 2
        w = edgeWidth cfg
    (outer, inner) = irregularGridPaths g

shiftGrid :: Backend' b => Grid ShiftC a -> Drawing b
shiftGrid = placeGrid . fmap (const gridCell)

bgdashingG ::
  (Semigroup a, HasStyle a, InSpace V2 Double a) =>
  [Double] ->
  Double ->
  AlphaColour Double ->
  a ->
  a
bgdashingG ds offs c x = x # dashingG ds offs <> x # lcA c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

gridDashing :: (Semigroup a, HasStyle a, InSpace V2 Double a) => a -> a
gridDashing = bgdashingG dashes dashoffset white'
  where
    white' = black `withOpacity` (0.05 :: Double)

data CageParams
  = CageParams
      { cageDashOn :: Double,
        cageDashOff :: Double,
        cageWidth :: Double,
        cageOffset :: Double
      }

cageParams :: Config -> CageParams
cageParams cfg = case _cfgDevice cfg of
  Screen -> CageParams (4 / 40) (4 / 40) onepix (4 * onepix)
  Print ->
    let -- input parameters
        lwidth = 1.25 * onepix
        steps = 10
        gapSteps = 2
        dashFactor = 3
        step = 1 / steps
        cap = lwidth
        on = dashFactor * step / (dashFactor + 1) - cap
        off = step / (dashFactor + 1) + cap
        offset = step * gapSteps / 2
     in CageParams on off lwidth offset

cageDashing :: (HasStyle a, InSpace V2 Double a) => CageParams -> a -> a
cageDashing (CageParams on off w _) =
  lineCap LineCapSquare . lwG w . dashingG [on, off] (on / 2)

-- | `irregularGridPaths g` is a pair `(outer, inner)` of paths.
--
-- `outer` consists of the loops that make up the border of the
-- grid (assuming the grid is connected orthogonally). They are
-- reoriented to be compatible with `outLine`; for some reason,
-- reversePath on the immediate result does not work.
--
-- `inner` consists of the individual inner segments.
irregularGridPaths :: Grid C a -> (Path V2 Double, Path V2 Double)
irregularGridPaths m = (path' (map Data.revEdge outer), path inner)
  where
    (outer, inner) = Data.edges (Map.keysSet m) (`Map.member` m)
    path es = mconcat . map (conn . Data.ends) $ es
    path' es = case loops (map Data.ends' es) of
      Just ls -> mconcat . map (pathFromLoopVertices . map toPoint) $ ls
      Nothing -> mempty
    pathFromLoopVertices =
      pathFromLocTrail . mapLoc (wrapLoop . closeLine) . fromVertices
    conn (v, w) = toPoint v ~~ toPoint w

offsetBorder :: Double -> [C] -> Path V2 Double
offsetBorder off cs =
  pathFromLoopVertices . map offsetCorner . corners . map toPoint $ loop
  where
    pathFromLoopVertices =
      pathFromLocTrail . mapLoc (wrapLoop . closeLine) . fromVertices
    outer :: [Edge' N]
    (outer, _) = Data.edges cs (`elem` cs)
    loop :: [N]
    loop = case loops (map Data.ends' outer) of
      Just [l] -> tail l
      _ -> error "broken cage"
    corners :: [P2 Double] -> [(P2 Double, P2 Double, P2 Double)]
    corners vs =
      catMaybes $
        zipWith3
          (\a b c -> if b .-. a == c .-. b then Nothing else Just (a, b, c))
          vs
          (tail vs ++ vs)
          (tail (tail vs) ++ vs)
    offsetCorner :: (P2 Double, P2 Double, P2 Double) -> P2 Double
    offsetCorner (a, b, c) =
      let dir = perp (normalize (b .-. a)) ^+^ perp (normalize (c .-. b))
       in b .+^ (off *^ dir)

onGrid ::
  (Transformable a, Monoid a, InSpace V2 Double a) =>
  Double ->
  Double ->
  (t -> a) ->
  [(Coord, t)] ->
  a
onGrid dx dy f = mconcat . map g
  where
    g (p, c) = f c # translate (r2coord p)
    r2coord (x, y) = r2 (dx * fromIntegral x, dy * fromIntegral y)

placeGrid ::
  (ToPoint k, HasOrigin a, Monoid a, InSpace V2 Double a) => Grid k a -> a
placeGrid = Map.foldMapWithKey (moveTo . toPoint)

placeGrid' ::
  (HasOrigin a, Monoid a, InSpace V2 Double a) => Grid (P2 Double) a -> a
placeGrid' = Map.foldMapWithKey moveTo

edge :: (ToPoint k) => Edge k -> Path V2 Double
edge (E c d) = rule d # translate (toPoint c .-. origin)
  where
    rule Vert = vrule 1.0 # alignB
    rule Horiz = hrule 1.0 # alignL

midPoint ::
  (AS.AffineSpace k, AS.Diff k ~ (Int, Int), ToPoint k) =>
  Edge k ->
  P2 Double
midPoint e = c .+^ 0.5 *^ (d .-. c)
  where
    (a, b) = Data.ends e
    c = toPoint a
    d = toPoint b

edgeStyle :: (HasStyle a, InSpace V2 Double a) => Config -> a -> a
edgeStyle cfg = lineCap LineCapSquare . lwG ew
  where
    ew = case _cfgDevice cfg of
      Screen -> edgewidth
      Print -> 2 * edgewidth / 3

thinEdgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
thinEdgeStyle = lineCap LineCapSquare . lwG onepix

solEdgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
solEdgeStyle =
  lineCap LineCapSquare . lc (blend 0.8 black white) . lwG (3 * onepix)

solstyle :: (HasStyle a, InSpace V2 Double a) => a -> a
solstyle = lc (blend 0.8 black white) . lwG (3 * onepix)

edges :: (ToPoint k, Backend' b) => [Edge k] -> Drawing b
edges es = Drawing (\cfg -> edgeStyle cfg . stroke . mconcat . map edge $ es)

dirPath :: Dir -> Path V2 Double
dirPath dir = case dir of
  Horiz -> hrule 1.0
  Vert -> vrule 1.0

edgeDecoration :: Backend' b => Dir -> Drawing b
edgeDecoration dir = Drawing (\cfg -> edgeStyle cfg . stroke . dirPath $ dir)

edgeDecorationThin :: Backend' b => Dir -> Drawing b
edgeDecorationThin dir = Drawing (\_ -> thinEdgeStyle . stroke . dirPath $ dir)

edgeDecorationSol :: Backend' b => Dir -> Drawing b
edgeDecorationSol dir = Drawing (\_ -> solEdgeStyle . stroke . dirPath $ dir)

thinEdges :: (ToPoint k, Backend' b) => [Edge k] -> Drawing b
thinEdges = draw . thinEdgeStyle . stroke . mconcat . map edge

areas :: (Backend' b, Eq a) => Grid C a -> Drawing b
areas = edges . borders

cage :: Backend' b => [C] -> Drawing b
cage cs = Drawing dcage
  where
    dcage cfg = border # stroke # cageDashing params
      where
        params = cageParams cfg
        border = offsetBorder (- (cageOffset params)) cs

fillBG :: Backend' b => Colour Double -> Drawing b
fillBG c = draw $ square 1 # lwG onepix # fc c # lc c

shadeGrid :: Backend' b => Grid C (Maybe (Colour Double)) -> Drawing b
shadeGrid = placeGrid . fmap fillBG . clues

shade :: Backend' b => Grid C Bool -> Drawing b
shade = shadeGrid . fmap f
  where
    f True = Just gray
    f False = Nothing

areasGray :: Backend' b => Grid C Char -> Drawing b
areasGray = areas <> shadeGrid . fmap cols
  where
    cols c
      | isUpper c = Just (blend 0.1 black white)
      | otherwise = Nothing
