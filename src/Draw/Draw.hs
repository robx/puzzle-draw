{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw.Draw
  ( Device(..)
  , Config(..)
  , PuzzleSol
  , Drawers(..)
  , QDrawing(..)
  , Drawing
  , draw
  , diagram
  , OutputChoice(..)
  , render
  , Unit(..)
  , DiagramSize
  , diagramSize
  , toOutputWidth
  , CodeDiagrams(..)
  , centerX'
  , centerY'
  , centerXY'
  , smash'
  , alignBL'
  , alignBR'
  , alignTL'
  , alignTR'
  , alignL'
  , alignR'
  , fit'
  , spread'
  , phantom''
  , aboveT'
  , besidesR'
  , strutX'
  , text'
  , textFixed
  )
where

import           Diagrams.Prelude        hiding ( render )

import           Draw.Font
import           Draw.Lib
import           Draw.Widths

data Device = Screen | Print

data Config = Config
  { _cfgDevice :: Device
  , _cfgFontVar :: Font
  , _cfgFontFixed :: Font
  }

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

diagram :: Config -> QDrawing b v n m -> QDiagram b v n m
diagram c d = fromDrawing d c

type Drawing b = QDrawing b (V b) (N b) Any

data Drawers b p s =
    Drawers
        { puzzle :: p -> Drawing b
        , solution :: (p, s) -> Drawing b
        }

type PuzzleSol b = (Drawing b, Maybe (Drawing b))

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
    deriving Show

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
render
  :: Backend' b
  => Config
  -> Maybe (CodeDiagrams (Drawing b))
  -> PuzzleSol b
  -> OutputChoice
  -> Maybe (Diagram b)
render config mc (p, ms) = fmap (bg white) . d
 where
  fixup = alignPixel . border borderwidth
  addCode x = case mc of
    Nothing -> x
    Just (CodeDiagrams cleft ctop cover) ->
      ((diagram config cover <> x) =!= top (diagram config ctop))
        |!| lft (diagram config cleft)
  (=!=) = beside unitY
  (|!|) = beside (negated unitX)
  top c = if isEmpty c then mempty else strutY 0.5 =!= c
  lft c = if isEmpty c then mempty else strutX 0.5 |!| c
  isEmpty c = diameter unitX c == 0
  d DrawPuzzle   = fixup . addCode <$> Just (diagram config p)
  d DrawSolution = fixup . addCode <$> fmap (diagram config) ms
  d DrawExample  = sideBySide <$> d DrawPuzzle <*> d DrawSolution
  sideBySide x y = x ||| strutX 2.0 ||| y

data Unit = Pixels | Points

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

type DiagramSize = (Double, Double)

diagramSize :: Backend' b => Diagram b -> DiagramSize
diagramSize = unr2 . boxExtents . boundingBox

toOutputWidth :: Unit -> Double -> Double
toOutputWidth u w = case u of
  Pixels -> fromIntegral wpix
  Points -> wpt
 where
  wpix = round (gridresd * w) :: Int  -- grid square size 40px
  wpt  = cmtopoint w     -- grid square size 1.0cm

alignPixel :: Backend' b => Diagram b -> Diagram b
alignPixel = scale (1 / gridresd) . align' . scale gridresd
 where
  align' d = maybe id grow (getCorners $ boundingBox d) d
  grow (bl, tr) = mappend $ phantoml (nudge bl False) (nudge tr True)
  nudge p dir = let (px, py) = unp2 p in p2 (nudge' px dir, nudge' py dir)
  nudge' x True  = fromIntegral (ceiling (x - 0.5) :: Int) + 0.5
  nudge' x False = fromIntegral (floor (x + 0.5) :: Int) - 0.5
  phantoml p q = phantom' $ p ~~ q

-- | Add a phantom border of the given width around a diagram.
border :: Backend' b => Double -> Diagram b -> Diagram b
border w =
  extrudeEnvelope (w *^ unitX)
    . extrudeEnvelope (-w *^ unitX)
    . extrudeEnvelope (w *^ unitY)
    . extrudeEnvelope (-w *^ unitY)

data CodeDiagrams a = CodeDiagrams { _cdLeft :: a, _cdTop :: a, _cdOver :: a }

instance Semigroup a => Semigroup (CodeDiagrams a) where
    (CodeDiagrams x y z) <> (CodeDiagrams x' y' z') =
        CodeDiagrams (x <> x') (y <> y') (z <> z')

instance Monoid a => Monoid (CodeDiagrams a) where
    mempty = CodeDiagrams mempty mempty mempty
    (CodeDiagrams x y z) `mappend` (CodeDiagrams x' y' z') =
        CodeDiagrams (x `mappend` x') (y `mappend` y') (z `mappend` z')


centerX' :: Backend' b => Drawing b -> Drawing b
centerX' = lift centerX

centerY' :: Backend' b => Drawing b -> Drawing b
centerY' = lift centerY

centerXY' :: Backend' b => Drawing b -> Drawing b
centerXY' = lift centerXY

smash' :: Backend' b => Drawing b -> Drawing b
smash' = lift smash

alignBL' :: Backend' b => Drawing b -> Drawing b
alignBL' = lift alignBL

alignBR' :: Backend' b => Drawing b -> Drawing b
alignBR' = lift alignBR

alignTL' :: Backend' b => Drawing b -> Drawing b
alignTL' = lift alignTL

alignTR' :: Backend' b => Drawing b -> Drawing b
alignTR' = lift alignTR

alignL' :: Backend' b => Drawing b -> Drawing b
alignL' = lift alignL

alignR' :: Backend' b => Drawing b -> Drawing b
alignR' = lift alignR

fit' :: Backend' b => Double -> Drawing b -> Drawing b
fit' f = lift (fit f)

spread' :: Backend' b => V2 Double -> [Drawing b] -> Drawing b
spread' v ds = Drawing (\c -> spread v $ map (\d -> fromDrawing d c) ds)

phantom'' :: Backend' b => Drawing b -> Drawing b
phantom'' = lift phantom

aboveT' :: Backend' b => Drawing b -> Drawing b -> Drawing b
aboveT' = lift2 aboveT

besidesR' :: Backend' b => Drawing b -> Drawing b -> Drawing b
besidesR' = lift2 besidesR

strutX' :: Backend' b => Double -> Drawing b
strutX' = draw . strutX

lift :: (Diagram b -> Diagram b) -> Drawing b -> Drawing b
lift f d = Drawing (\c -> f (fromDrawing d c))

lift2
  :: (Diagram b -> Diagram b -> Diagram b)
  -> Drawing b
  -> Drawing b
  -> Drawing b
lift2 f d1 d2 = Drawing (\c -> f (fromDrawing d1 c) (fromDrawing d2 c))

text' :: Renderable (Path V2 Double) b => String -> QDrawing b V2 Double Any
text' t = Drawing (\cfg -> text'' (_cfgFontVar cfg) t)

textFixed :: Backend' b => String -> Drawing b
textFixed t = Drawing (\cfg -> text'' (_cfgFontFixed cfg) t)
