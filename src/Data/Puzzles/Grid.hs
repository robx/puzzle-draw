module Data.Puzzles.Grid where

import Data.Maybe

newtype Grid a = GG {unGG :: [[a]]}
    deriving Show

type CharGrid = Grid Char

type IntClue = Maybe Int
type CharClue = Maybe Char
type IntGrid = Grid IntClue

data MasyuPearl = MWhite | MBlack
type MasyuClue = Maybe MasyuPearl

type MasyuGrid = Grid MasyuClue

size :: Grid a -> (Int, Int)
size (GG g) = (length (head g), length g)

type Point = (Int, Int)

inBounds :: Grid a -> Point -> Bool
inBounds g (x, y) = x >= 0 && y >= 0 && x < sx && y < sy
    where (sx, sy) = size g

add :: Point -> Point -> Point
add (x, y) (x', y') = (x + x', y + y')

neighbours :: Grid a -> Point -> [Point]
neighbours g p = filter (inBounds g) . map (add p) $ deltas
    where deltas = [ (dx, dy)
                   | dx <- [-1..1], dy <- [-1..1]
                   , dx /= 0 || dy /= 0
                   ]

(!) :: Grid a -> Point -> a
g@(GG g') ! (x, y) = g' !! (sy - y - 1) !! x
    where (_, sy) = size g

instance Functor Grid where
    fmap f = GG . map (map f) . unGG

fromListList :: [[a]] -> Grid a
fromListList = GG

charToIntClue c
    | '0' <= c && c <= '9'  = Just $ fromEnum c - fromEnum '0'
    | otherwise             = Nothing

charToCharClue c
    | c == ' ' || c == '.' || c == '-'  = Nothing
    | otherwise                         = Just c

charToMasyuClue :: Char -> MasyuClue
charToMasyuClue '*' = Just MBlack
charToMasyuClue 'o' = Just MWhite
charToMasyuClue c
    | c == ' ' || c == '.'  = Nothing

points g = [ (x, y) | x <- [0..sx-1], y <- [0..sy-1] ]
    where (sx, sy) = size g

clues g = [ (p, fromJust $ g ! p) | p <- points g
                                  , isJust $ g ! p ]

data Dir = V | H
    deriving (Eq, Ord)

instance Show Dir where
    show V = "|"
    show H = "-"

data Edge = E Point Dir
    deriving (Show, Eq, Ord)

swap (x, y) = (y, x)

borders :: Eq a => Grid a -> [Edge]
borders g = [ E p V | p <- vborders ] ++ [ E p H | p <- hborders ]
    where
        borders' f (sx, sy) = [ (x + 1, y) | x <- [0 .. sx - 2]
                                           , y <- [0 .. sy - 1]
                                           , f (x, y) /= f (x + 1, y) ]
        vborders = borders' (g !) (size g)
        hborders = map swap $ borders' ((g !) . swap) (swap . size $ g)

msqrt :: Integral a => a -> Maybe a
msqrt x = if r ^ 2 == x then Just r else Nothing
    where r = round . sqrt . fromIntegral $ x

mhalf :: Integral a => a -> Maybe a
mhalf x = if even x then Just (x `div` 2) else Nothing

sudokuborders :: Int -> [Edge]
sudokuborders s =
    case msqrt s of
        Just r  -> squareborders r
        Nothing -> case mhalf s of 
                       Just h  -> rectborders h
                       Nothing -> error "no sudoku layout of this size"
    where squareborders r = [ E (r*x, y) V | x <- [1..r-1], y <- [0..r*r-1] ]
                            ++ [ E (x, r*y) H | x <- [0..r*r-1], y <- [1..r-1] ]
          rectborders h = [ E (h, y) V | y <- [0..2*h-1] ]
                          ++ [ E (x, 2*y) H | x <- [0..2*h-1], y <- [1..h-1] ]

sudokubordersg :: Grid a -> [Edge]
sudokubordersg g = sudokuborders s
   where (w, h) = size g
         s | w == h    = w
           | otherwise = error "non-square sudoku grid?"

data Tightfit a = Single a | UR a a | DR a a

instance Show a => Show (Tightfit a) where
    show c = "(" ++ show' c ++ ")"
        where show' (Single x) = show x
              show' (UR x y)   = show x ++ "/" ++ show y
              show' (DR x y)   = show x ++ "\\" ++ show y

data OutsideClues a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }

clueso (OC l r b t) = catMaybes . map liftMaybe . concat $
                             [ zipWith (\ y c -> ((-1, y), c)) [0..h-1] l
                             , zipWith (\ y c -> (( w, y), c)) [0..h-1] r
                             , zipWith (\ x c -> (( x,-1), c)) [0..w-1] b
                             , zipWith (\ x c -> (( x, h), c)) [0..w-1] t
                             ]
    where w = length b
          h = length l
          liftMaybe (p, Just x) = Just (p, x)
          liftMaybe (p, Nothing) = Nothing
