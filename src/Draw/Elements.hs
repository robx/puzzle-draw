{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Module: Diagrams.TwoD.Puzzles.Elements
--
-- Tools to draw individual puzzle components. In particular
-- contents and decorations for individual cells.

module Draw.Elements where

import Diagrams.Prelude hiding (N)
import Diagrams.TwoD.Offset

import qualified Data.Map as Map

import Data.Grid
import Data.Elements hiding (Loop)
import Data.GridShape hiding (edge)

import Draw.Lib
import Draw.Style
import Draw.Widths
import Draw.Grid

pearl :: Backend' b =>
         MasyuPearl -> Diagram b
pearl m = circle 0.35 # lwG 0.05 # fc (c m)
  where
    c MWhite = white
    c MBlack = black

smallPearl :: Backend' b =>
              MasyuPearl -> Diagram b
smallPearl = scale 0.4 . pearl

drawEnd :: Backend' b =>
       MEnd -> Diagram b
drawEnd MEnd = smallPearl MBlack

drawBigEnd :: Backend' b =>
       MEnd -> Diagram b
drawBigEnd MEnd = pearl MBlack

-- | The up-right diagonal of a centered unit square.
ur :: Path V2 Double
ur = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]

-- | The down-right diagonal of a centered unit square.
dr :: Path V2 Double
dr = fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

-- | Both diagonals of a centered unit square.
cross :: Path V2 Double
cross = ur <> dr

-- | Draw a cross.
drawCross :: Backend' b => Bool -> Diagram b
drawCross True = stroke cross # scale 0.8 # lwG edgewidth
drawCross False = mempty

-- | Draw a Compass clue.
drawCompassClue :: Backend' b =>
                   CompassC -> Diagram b
drawCompassClue (CC n e s w) = texts <> stroke cross # lwG onepix
    where tx Nothing _ = mempty
          tx (Just x) v = text' (show x) # scale 0.5 # translate (r2 v)
          texts = mconcat . zipWith tx [n, e, s, w] $
                  [(0,f), (f,0), (0,-f), (-f,0)]
          f = 3/10

drawSlovakClue :: Backend' b =>
                  SlovakClue -> Diagram b
drawSlovakClue (SlovakClue s c) =
    centerY (drawInt s === strutY 0.1 === dots c) <> fillBG gray
  where
    dots n = centerX $ hcat' with {_sep = 0.04} (replicate n $ d)
    d = circle 0.1 # lwG 0.02 # fc white

-- | Draw a thermometer.
thermo :: Backend' b => [P2 Double] -> Diagram b
thermo vs@(v:_) = (bulb `atop` line) # col
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs)
                 # lwG 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black
thermo [] = error "invalid empty thermometer"

-- | Draw a list of thermometers, given as lists of @(Int, Int)@ cell
-- coordinates.
drawThermos :: Backend' b => [Thermometer] -> Diagram b
drawThermos = mconcat . map (thermo . map toPoint)

-- | @drawTight d t@ draws the tight-fit value @t@, using @d@ to
-- draw the components.
drawTight :: Backend' b =>
             (a -> Diagram b) -> Tightfit a -> Diagram b
drawTight d (Single x) = d x
drawTight d (UR x y) = stroke ur # lwG onepix
                       <> d x # scale s # translate (r2 (-t,t))
                       <> d y # scale s # translate (r2 (t,-t))
    where t = 1/5
          s = 2/3
drawTight d (DR x y) = stroke dr # lwG onepix
                       <> d x # scale s # translate (r2 (-t,-t))
                       <> d y # scale s # translate (r2 (t,t))
    where t = 1/5
          s = 2/3

-- | Stack the given words, left-justified.
stackWords :: Backend' b => [String] -> Diagram b
stackWords = vcat' with {_sep = 0.1} . scale 0.8 . map (alignL . textFixed)

-- | Stack the given words, left-justified, a bit more generous, nice font
stackWordsLeft :: Backend' b => [String] -> Diagram b
stackWordsLeft = vcat' (with & catMethod .~ Distrib & sep .~ 1) . map (alignL . text')

-- | Stack the given words, left-justified, a bit more generous, nice font
stackWordsRight :: Backend' b => [String] -> Diagram b
stackWordsRight = vcat' (with & catMethod .~ Distrib & sep .~ 1) . map (alignR . text')

-- | Mark a word in a grid of letters.
drawMarkedWord :: Backend' b => MarkedWord -> Diagram b
drawMarkedWord (MW s e) = lwG onepix . stroke $ expandTrail' with {_expandCap = LineCapRound} 0.4 t
    where t = fromVertices [p2i s, p2i e] # translate (r2 (1/2,1/2))

-- | Apply 'drawMarkedWord' to a list of words.
drawMarkedWords :: Backend' b => [MarkedWord] -> Diagram b
drawMarkedWords = mconcat . map drawMarkedWord

drawMarkedLine :: (ToPoint a, Backend' b) => MarkedLine a -> Diagram b
drawMarkedLine (MarkedLine s e) = strokePath (toPoint s ~~ toPoint e) # lwG edgewidth # lc gray

drawMarkedLines :: (ToPoint a, Backend' b) => [MarkedLine a] -> Diagram b
drawMarkedLines = mconcat . map drawMarkedLine

-- | Draw a slalom clue.
drawSlalomClue :: (Show a, Backend' b) =>
                  a -> Diagram b
drawSlalomClue x = text' (show x) # scale 0.75
                   <> circle 0.4 # fc white # lwG onepix

drawSlalomDiag :: Backend' b
               => SlalomDiag -> Diagram b
drawSlalomDiag d = stroke (v d) # lwG edgewidth
  where
    v SlalomForward = ur
    v SlalomBackward = dr

-- | Draw text. Shouldn't be more than two characters or so to fit a cell.
drawText :: Backend' b => String -> Diagram b
drawText = text'

drawTextFixed :: Backend' b => String -> Diagram b
drawTextFixed = textFixed

-- | Draw an @Int@.
drawInt :: Backend' b =>
           Int -> Diagram b
drawInt s = drawText (show s)

-- | Draw a character.
drawChar :: Backend' b =>
            Char -> Diagram b
drawChar c = drawText [c]

drawCharFixed :: Backend' b =>
                 Char -> Diagram b
drawCharFixed c = drawTextFixed [c]

drawCharOpaque :: Backend' b =>
                  Char -> Diagram b
drawCharOpaque c = drawChar c <> circle 0.5 # lwG 0 # fc white

hintTL :: Backend' b => String -> Diagram b
hintTL = moveTo (p2 (-0.4,0.4)) . scale 0.5 . alignTL . drawText

-- | Stack a list of words into a unit square. Scaled such that at least
-- three words will fit.
drawWords :: Backend' b =>
             [String] -> Diagram b
drawWords ws = spread (-1.0 *^ unitY)
                      (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

-- | Fit a line drawing into a unit square.
--   For example, a Curve Data clue.
drawCurve :: Backend' b => [Edge N] -> Diagram b
drawCurve = lwG onepix . fit 0.6 . centerXY . mconcat . map (stroke . edge)

-- | Draw a shadow in the style of Afternoon Skyscrapers.
drawShadow :: Backend' b => Shade -> Diagram b
drawShadow (Shade s w) = (if s then south else mempty) <>
                         (if w then west else mempty)
  where
    shape = translate (r2 (-1/2, -1/2)) . fromVertices . map p2 $
        [ (0, 0), (1/4, 1/4), (1, 1/4), (1, 0), (0, 0) ]
    south = strokeLocLoop shape # lwG 0 # fc gray
    west = reflectAbout (p2 (0, 0)) (direction $ r2 (1, 1)) south

-- | Draws the digits of a tapa clue, ordered
--   left to right, top to bottom.
drawTapaClue :: Backend' b =>
                TapaClue -> Diagram b
drawTapaClue (TapaClue [x]) = drawInt x
drawTapaClue (TapaClue xs)  = fit 0.8
                            . atPoints (p (length xs))
                            . map drawInt
                            $ xs
  where
    p n = mconcat . pathVertices $ centerXY (p' n)
    p' 2 = p2 (-1/4, 1/4) ~~ p2 (1/4, -1/4)
    p' 3 = reflectX . rotateBy (1/6) $ triangle 0.8
    p' 4 = reflectX . rotateBy (3/8) $ square 0.7
    p' 1 = error "singleton clues handled separately"
    p' _ = error "invalid tapa clue"

drawPrimeDiag :: Backend' b =>
                 PrimeDiag -> Diagram b
drawPrimeDiag (PrimeDiag d) = stroke p # lwG (3 * onepix) # lc (blend 0.5 gray white)
  where
    p = case d of (False, False) -> mempty
                  (True,  False) -> ur
                  (False, True)  -> dr
                  (True,  True)  -> ur <> dr

drawAnglePoly :: Backend' b =>
                 Int -> Diagram b
drawAnglePoly 3 = strokePath (triangle 0.3) # fc black
drawAnglePoly 4 = strokePath (square 0.25) # fc gray
drawAnglePoly 5 = strokePath (pentagon 0.2) # fc white
drawAnglePoly _ = error "expected 3..5"

fish :: Double -> Angle Double -> Trail' Loop V2 Double
fish off startAngle = closeLine $ half <> half # reverseLine # reflectY
  where
    half = arc (angleDir startAngle) endAngle # translateY (-off)
    endAngle = acosA off ^+^ (90 @@ deg)

drawFish :: Backend' b =>
            Fish -> Diagram b
drawFish Fish = fit 0.6 . centerXY . fc black . strokeLoop $
                fish 0.7 (30 @@ deg)

drawStar :: Backend' b =>
            Star -> Diagram b
drawStar Star = fc black . stroke . star (StarSkip 2) $ pentagon 0.3

drawTree :: Backend' b => Tree -> Diagram b
drawTree Tree =
    fit 0.5 $ centerXY $ scaleY 0.5 $ fc black $ mconcat
        [ rect 0.1 0.6 # moveTo (p2 (0.5, 0.7))
        , circle 0.1   # moveTo (p2 (0.4, 0.9))
        , circle 0.2   # moveTo (p2 (0.6, 1.0))
        , circle 0.2   # moveTo (p2 (0.4, 1.2))
        , circle 0.16  # moveTo (p2 (0.6, 1.3))
        , circle 0.15  # moveTo (p2 (0.45, 1.45))
        , circle 0.1   # moveTo (p2 (0.7, 1.4))
        ]

drawTent :: Backend' b => PlacedTent -> Diagram b
drawTent (Tent d) = tent <> lwG gridwidth (stroke conn)
  where
    conn :: Path V2 Double
    conn = p2 (0, 0) ~~ p2 (case d of
        U -> (0, 1)
        R -> (1, 0)
        D -> (0, -1)
        L -> (-1, 0))

    tent = fit 0.7 $ centerXY $ lwG 0 $ mconcat
        [ rect 10 (1/4) # fc black
        , shape [(-2, 0), (0, 4), (2, 0), (-2, 0)] # fc white
        , shape [(-4, 0), (0, 8), (4, 0), (-4, 0)] # fc black
        , shape [(0, 8), (-1/2, 8 + 1), (-1, 8 + 1 - 1/4), (0, 8 + 1 - 1/4 - 2), (0, 8) ] # fc black
        , shape [(0, 8), (1/2, 8 + 1), (1, 8 + 1 - 1/4), (0, 8 + 1 - 1/4 - 2), (0, 8) ] # fc black
        ]
    shape = strokeLocLoop . fromVertices . map p2

vertexLoop :: VertexLoop -> Located (Trail' Loop V2 Double)
vertexLoop = mapLoc closeLine . fromVertices . map toPoint

note :: Backend' b =>
        Diagram b -> Diagram b
note d = d # frame 0.2 # bg (blend 0.2 black white)

placeNoteTR :: Backend' b =>
             Size -> Diagram b -> Diagram b
placeNoteTR sz d = note d # alignBL # translatep sz # translate (r2 (0.6,0.6))

placeNoteTL :: Backend' b =>
             Size -> Diagram b -> Diagram b
placeNoteTL sz d = note d # alignBR # translatep sz # translate (r2 (-0.6,0.6))

placeNoteBR :: Backend' b =>
             Size -> Diagram b -> Diagram b
placeNoteBR (x,_) d = note d # alignTL # translatep (x,0) # translate (r2 (0.6,-0.6))

miniloop :: Backend' b => Diagram b
miniloop = (drawThinEdges (map unorient out) <> grid gSlither g)
           # centerXY # scale 0.4
  where
    g = sizeGrid (1, 1)
    (out, _) = edgesM g

dominoBG :: Colour Double
dominoBG = blend 0.3 black white

drawDomino :: Backend' b => (Int, Int) -> Diagram b
drawDomino (x, y) =
    (drawInt x # smash ||| strutX 0.65 ||| drawInt y # smash) # centerXY # scale 0.6
    <> strokePath (rect 0.8 0.5) # lwG 0 # fc dominoBG

newtype DominoC = DominoC C
  deriving (Ord, Eq)

instance ToPoint DominoC where
    toPoint (DominoC (C x y)) = p2 ((1.0 * fromIntegral x),
                                    (0.7 * fromIntegral y))

drawDominos :: Backend' b => DigitRange -> Diagram b
drawDominos = centerXY . placeGrid
            . Map.mapKeys DominoC . fmap drawDomino . dominoGrid

drawPill :: Backend' b => Int -> Diagram b
drawPill x = drawInt x # scale 0.6
             <> strokePath (roundedRect 0.8 0.5 0.2) # lwG 0 # fc dominoBG

drawPills :: Backend' b => DigitRange -> Diagram b
drawPills (DigitRange a b) = centerXY . onGrid 1.0 0.7 drawPill $ placed
  where
    n = b - a + 1
    root = head [ x | x <- [n,n-1..], x*x <= n ]
    placed = zip [(x, y) | x <- [0..root], y <- [root,root-1..0]] [a..b]

polyominoGrid :: Backend' b => Grid C (Maybe Char) -> Diagram b
polyominoGrid = placeGrid . fmap (scale 0.8) . fmap
    (\x -> case x of
        Nothing -> fillBG black
        Just c -> (drawText [c] # fc white # lc white) <> fillBG black)

drawPentominos :: Backend' b => Diagram b
drawPentominos = centerXY . scale 0.5 . polyominoGrid $ pentominoGrid

drawLITS :: Backend' b => Diagram b
drawLITS = centerXY . scale 0.5 . polyominoGrid $ litsGrid

drawLITSO :: Backend' b => Diagram b
drawLITSO = centerXY . scale 0.5 . polyominoGrid $ litsoGrid

drawCrossing :: Backend' b => Crossing -> Diagram b
drawCrossing = const $ drawChar '+'

drawBahnhofClue :: Backend' b => BahnhofClue -> Diagram b
drawBahnhofClue = either drawInt drawCrossing

kropkiDot :: Backend' b => KropkiDot -> Diagram b
kropkiDot KNone = mempty
kropkiDot c = circle 0.1 # lwG 0.03 # fc (col c) # smash
    where col KWhite = white
          col KBlack = blend 0.98 black white
          col KNone  = error "can't reach"

drawFraction :: Backend' b => Fraction -> Diagram b
drawFraction f = centerX $ case f of
    FInt a      -> drawText a # scale 0.8
    FFrac a b   -> frac a b
    FComp a b c -> (drawText a # scale 0.8) ||| strutX (1/10) ||| frac b c
  where
    frac b c = stroke slash # scale (1/4) # lwG onepix
               <> drawText b # scale s # translate (r2 (-t,t))
               <> drawText c # scale s # translate (r2 (t,-t))
      where t = 1/6
            s = 1/2
            slash :: Path V2 Double
            slash = fromVertices [p2 (-1/3,-1/2), p2 (1/3,1/2)]

drawMyopia :: Backend' b => Myopia -> Diagram b
drawMyopia = foldMap d'
  where
    d' = lwG onepix . scale (1/3) . d
    d U = a (0, 0) (0, 1)
    d R = a (0, 0) (1, 0)
    d D = a (0, 0) (0, -1)
    d L = a (0, 0) (-1, 0)
    a p q = arrowBetween' (with & arrowHead .~ tri & lengths .~ verySmall) (p2 p) (p2 q)

greaterClue :: Backend' b => GreaterClue -> [Diagram b]
greaterClue [] = mempty
greaterClue (_:rs) = g rs
  where
    g [] = [placeholder]
    g (r:rs') = placeholder : drawRel r : g rs'
    drawRel RUndetermined = mempty
    drawRel RLess = drawText "<"
    drawRel RGreater = drawText ">"
    drawRel REqual = drawText "="
    placeholder = circle 0.35 # lwG onepix # dashingG [0.05, 0.05] 0
