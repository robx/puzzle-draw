-- | Types for a variety of puzzle elements.
module Data.Puzzles.Elements where

import Data.Puzzles.GridShape (Coord, Edge)

type Clue a = Maybe a

data MasyuPearl = MWhite | MBlack
    deriving (Eq, Show)

type MasyuClue = Clue MasyuPearl

type IntClue = Clue Int

-- | A Compass clue, specifiying optional numbers in the
--   four cardinal directions.
data CompassC = CC (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
    deriving Show

type CompassClue = Clue CompassC

-- | A cell that is optionally bisected by a diagonal
--   (up-right or down-right).
data Tightfit a = Single a | UR a a | DR a a

instance Show a => Show (Tightfit a) where
    show c = "(" ++ show' c ++ ")"
        where show' (Single x) = show x
              show' (UR x y)   = show x ++ "/" ++ show y
              show' (DR x y)   = show x ++ "\\" ++ show y

-- | A marked word in a letter grid, by its start and end
--   coordinates.
data MarkedWord = MW { mwstart :: Coord, mwend :: Coord }

-- | A loop of edges.
type Loop = [Edge]

-- | A loop consisting of straight segments of arbitrary
-- angles between vertices.
type VertexLoop = [Coord]

-- | A thermometer, as a list of coordinates from bulb to end.
--   There should be at least two entries, entries should be distinct,
--   and successive entries should be neighbours (diagonal neighbours
--   are fine).
type Thermometer = [Coord]

-- | A forward or backward diagonal as occurring in the solution
--   of a slalom puzzle.
data SlalomDiag = SlalomForward | SlalomBackward
    deriving Show

-- | Shadow along from the western and southern side, as used for
--   afternoon skyscrapers.
data Shade = Shade Bool Bool
    deriving Show

data KropkiDot = KNone | KBlack | KWhite
    deriving Show

newtype TapaClue = TapaClue [Int]
    deriving Show

-- | Diagonal marking for Prime Place: forward diag?, backward diag?
newtype PrimeDiag = PrimeDiag (Bool, Bool)

data Black = Black
data Fish = Fish
