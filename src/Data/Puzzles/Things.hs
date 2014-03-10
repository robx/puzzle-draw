module Data.Puzzles.Things where

import Data.Puzzles.Grid

type Clue a = Maybe a

data MasyuPearl = MWhite | MBlack
type MasyuClue = Clue MasyuPearl

data CompassC = CC (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
type CompassClue = Clue CompassC

type IntClue = Clue Int

-- | A cell that is optionally bisected by a diagonal
--   (up-right or down-right).
data Tightfit a = Single a | UR a a | DR a a

instance Show a => Show (Tightfit a) where
    show c = "(" ++ show' c ++ ")"
        where show' (Single x) = show x
              show' (UR x y)   = show x ++ "/" ++ show y
              show' (DR x y)   = show x ++ "\\" ++ show y

data MarkedWord = MW { mwstart :: Coord, mwend :: Coord }

type Loop = [Edge]

-- | A thermometer, as a list of coordinates from bulb to end.
--   There should be at least two entries, entries should be distinct,
--   and successive entries should be neighbours (diagonal neighbours
--   are fine).
type Thermometer = [Coord]
