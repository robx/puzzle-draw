module Data.Puzzles.Read where

import Data.Puzzles.Grid
import Data.Puzzles.Things

import Text.Read (readMaybe)
import Data.Char (digitToInt, isAlpha, isDigit)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)

charToIntClue c
    | isDigit c  = Just $ digitToInt c
    | otherwise  = Nothing

strToIntClue :: String -> IntClue
strToIntClue = readMaybe

charToCharClue c
    | c `elem` [' ', '.', '-']  = Nothing
    | otherwise                 = Just c

charToMasyuClue :: Char -> MasyuClue
charToMasyuClue '*' = Just MBlack
charToMasyuClue 'o' = Just MWhite
charToMasyuClue c
    | c == ' ' || c == '.'  = Nothing


type CharGrid = SGrid Char
type MasyuGrid = SGrid MasyuClue
type AreaGrid = CharGrid
type ShadedGrid = SGrid Bool
type CharClueGrid = SGrid (Maybe Char)
type IntGrid = SGrid (Clue Int)

readCharGridUnfilled = fromListList . lines

-- | Read a grid of characters, filling short lines
-- to form a rectangle.
readCharGrid g = fromListList filled
  where
    ls = lines g
    w = maximum . map length $ ls
    filled = map (take w . (++ (repeat ' '))) ls

readAreaGrid = readCharGrid

readCharClueGrid = fmap charToCharClue . readCharGrid
readBoolGrid = fmap (`elem` ['x', 'X']) . readCharGrid
readIntGrid = fmap charToIntClue . readCharGrid
readStrGrid = fromListList . map words . lines
readWideIntGrid = fmap strToIntClue . readStrGrid
readMasyuGrid = fmap charToMasyuClue . readCharGrid
readXGrid = fmap f . readCharGrid
    where f 'X' = Just ()
          f _   = Nothing

-- parses a string like
--  o-o-o
--  |   |
--  o-o o
--    | |
--    o-o
readEdges :: String -> [Edge]
readEdges s = horiz ++ vert
    where g = readCharGrid s
          (w, h) = size g
          w' = w `div` 2
          h' = h `div` 2
          isHoriz (x, y) = g ! (2 * x + 1, 2 * y) == '-'
          isVert  (x, y) = g ! (2 * x, 2 * y + 1) == '|'
          horiz = [ E (x, y) H | x <- [0 .. w' - 1]
                               , y <- [0 .. h']
                               , isHoriz (x, y)
                               ]
          vert =  [ E (x, y) V | x <- [0 .. w']
                               , y <- [0 .. h' - 1]
                               , isVert (x, y)
                               ]

-- parses a string like
--  ┌┐┌─┐
--  ││└┐│
--  │└─┘│
--  └──┐│
--     └┘
readEdges' :: String -> [Edge]
readEdges' s = nub . sort . concatMap edges . cells $ g
    where g = readCharGrid s
          (w, h) = size g
          isV c = c `elem` "│└┘"
          isH c = c `elem` "─└┌"
          edges p = [ E p V | isV (g ! p) ] ++ [ E p H | isH (g ! p) ]

readThermos :: CharGrid -> (IntGrid, [Thermometer])
readThermos cg = (ig, thermos)
    where ig = fmap charToIntClue cg
          thermos = catMaybes [ thermo p | p <- cells cg ]
          at p = cg ! p
          isStart p = let c = at p in
                      isAlpha c
                      && (not
                         . any (\q -> at q == pred c)
                         . neighbours cg
                         $ p)
          thermo p | isStart p = Just (p : thermo' p)
                   | otherwise = Nothing
          thermo' p = p : ps ss
              where ss = filter (\q -> at q == succ (at p)) (neighbours cg p)
                    ps [s]  = thermo' s
                    ps []   = []
                    ps _    = error "invalid thermo"

readTightOutside :: String -> (OutsideClues (Maybe Int), SGrid (Tightfit ()))
readTightOutside s = (OC l r b t, gt)
    where g = readCharGrid s
          (w', h') = size g
          w = w' - 2
          h = h' - 2
          l = map charToIntClue [ g ! (0, y+1) | y <- [0..h-1] ]
          r = map charToIntClue [ g ! (w'-1, y+1) | y <- [0..h-1] ]
          b = map charToIntClue [ g ! (x+1, 0) | x <- [0..w-1] ]
          t = map charToIntClue [ g ! (x+1, h'-1) | x <- [0..w-1] ]
          readTight '.' = Single ()
          readTight '/' = UR () ()
          readTight '\\' = DR () ()
          gt = fromListList [ [ readTight (g ! (x, y)) | x <- [1..w'-2] ]
                            | y <- [h'-2,h'-3..1]
                            ]

readTightInt [c] = Single (digitToInt c)
readTightInt (c:'/':d:[]) = UR (digitToInt c) (digitToInt d)
readTightInt (c:'\\':d:[]) = DR (digitToInt c) (digitToInt d)

readTightIntGrid :: String -> SGrid (Tightfit Int)
readTightIntGrid = fromListList . map (map readTightInt . words) . lines
