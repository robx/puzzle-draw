{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw.Draw (
    PuzzleSol,
    Drawers(..),
    QDrawing(..),
    Drawing,
    draw,
    OutputChoice(..),
    render,
    Unit(..),
    diagramSize,
    toOutputWidth,
    CodeDiagrams(..),
    centerX',
    centerY',
    centerXY',
    smash',
    alignBL',
    alignBR',
    alignTL',
    alignTR',
    fit',
    spread',
    phantom'',
    aboveT'
  ) where

import Diagrams.Prelude hiding (render)

import Draw.Lib
import Draw.Widths

type Config = ()

newtype QDrawing b v n m = Drawing { fromDrawing :: Config -> QDiagram b v n m }
    deriving (Monoid, Semigroup, HasStyle, Juxtaposable)

type instance V (QDrawing b v n m) = v
type instance N (QDrawing b v n m) = n

instance (Metric v, OrderedField n, Semigroup m) => HasOrigin (QDrawing b v n m) where
    moveOriginTo p (Drawing f) = Drawing (moveOriginTo p . f)

instance (Metric v, OrderedField n, Semigroup m) => Transformable (QDrawing b v n m) where
    transform t (Drawing f) = Drawing (transform t . f)

draw :: QDiagram b v n m -> QDrawing b v n m
draw = Drawing . const

type Drawing b = QDrawing b (V b) (N b) Any

data Drawers b p s =
    Drawers
        { puzzle :: p -> Drawing b
        , solution :: (p, s) -> Drawing b
        }

type PuzzleSol b = (Diagram b, Maybe (Diagram b))

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
    deriving Show

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
render :: Backend' b
     => Maybe (CodeDiagrams (Diagram b))
     -> PuzzleSol b -> OutputChoice -> Maybe (Diagram b)
render mc (p, ms) = fmap (bg white) . d
  where
    fixup = alignPixel . border borderwidth
    addCode x = case mc of
        Nothing                              -> x
        Just (CodeDiagrams cleft ctop cover) ->
            ((cover <> x) =!= top ctop) |!| lft cleft
    (=!=) = beside unitY
    (|!|) = beside (negated unitX)
    top c = if isEmpty c then mempty else strutY 0.5 =!= c
    lft c = if isEmpty c then mempty else strutX 0.5 |!| c
    isEmpty c = diameter unitX c == 0
    d DrawPuzzle   = fixup . addCode <$> Just p
    d DrawSolution = fixup . addCode <$> ms
    d DrawExample  = sideBySide <$> d DrawPuzzle <*> d DrawSolution
    sideBySide x y = x ||| strutX 2.0 ||| y

data Unit = Pixels | Points

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

diagramSize :: Backend' b => Diagram b -> (Double, Double)
diagramSize = unr2 . boxExtents . boundingBox

toOutputWidth :: Unit -> Double -> Double
toOutputWidth u w = case u of Pixels -> fromIntegral wpix
                              Points -> wpt
  where
    wpix = round (gridresd * w) :: Int  -- grid square size 40px
    wpt = cmtopoint w     -- grid square size 1.0cm

alignPixel :: Backend' b => Diagram b -> Diagram b
alignPixel = scale (1/gridresd) . align' . scale gridresd
  where
    align' d = maybe id grow (getCorners $ boundingBox d) d
    grow (bl, tr) = mappend $ phantoml (nudge bl False) (nudge tr True)
    nudge p dir = let (px, py) = unp2 p in p2 (nudge' px dir, nudge' py dir)
    nudge' x True  = fromIntegral (ceiling (x - 0.5) :: Int) + 0.5
    nudge' x False = fromIntegral (floor   (x + 0.5) :: Int) - 0.5
    phantoml p q = phantom' $ p ~~ q

-- | Add a phantom border of the given width around a diagram.
border :: Backend' b => Double -> Diagram b -> Diagram b
border w = extrudeEnvelope (w *^ unitX) . extrudeEnvelope (-w *^ unitX)
         . extrudeEnvelope (w *^ unitY) . extrudeEnvelope (-w *^ unitY)

data CodeDiagrams a = CodeDiagrams { _cdLeft :: a, _cdTop :: a, _cdOver :: a }

instance Semigroup a => Semigroup (CodeDiagrams a) where
    (CodeDiagrams x y z) <> (CodeDiagrams x' y' z') =
        CodeDiagrams (x <> x') (y <> y') (z <> z')

instance Monoid a => Monoid (CodeDiagrams a) where
    mempty = CodeDiagrams mempty mempty mempty
    (CodeDiagrams x y z) `mappend` (CodeDiagrams x' y' z') =
        CodeDiagrams (x `mappend` x') (y `mappend` y') (z `mappend` z')


centerX' :: Backend' b => Drawing b -> Drawing b
centerX' (Drawing d) = Drawing (\c -> centerX (d c))

centerY' :: Backend' b => Drawing b -> Drawing b
centerY' (Drawing d) = Drawing (\c -> centerY (d c))

centerXY' :: Backend' b => Drawing b -> Drawing b
centerXY' (Drawing d) = Drawing (\c -> centerXY (d c))

smash' :: Backend' b => Drawing b -> Drawing b
smash' (Drawing d) = Drawing (\c -> smash (d c))

alignBL' :: Backend' b => Drawing b -> Drawing b
alignBL' (Drawing d) = Drawing (\c -> alignBL (d c))

alignBR' :: Backend' b => Drawing b -> Drawing b
alignBR' (Drawing d) = Drawing (\c -> alignBR (d c))

alignTL' :: Backend' b => Drawing b -> Drawing b
alignTL' (Drawing d) = Drawing (\c -> alignTL (d c))

alignTR' :: Backend' b => Drawing b -> Drawing b
alignTR' (Drawing d) = Drawing (\c -> alignTR (d c))

fit' :: Backend' b => Double -> Drawing b -> Drawing b
fit' f (Drawing d) = Drawing (\c -> fit f (d c))

spread' :: Backend' b => V2 Double -> [Drawing b] -> Drawing b
spread' v ds = Drawing (\c -> spread v $ map (\d -> fromDrawing d c) ds)

phantom'' :: Backend' b => Drawing b -> Drawing b
phantom'' d = Drawing (\c -> phantom (fromDrawing d c))

aboveT' :: Backend' b =>
          Drawing b -> Drawing b -> Drawing b
aboveT' a b = Drawing (\c -> fromDrawing a c `aboveT`  fromDrawing b c)

