-- | Types for a variety of puzzle elements.
module Data.Elements where

import Data.GridShape

type Clue a = Maybe a

data MasyuPearl = MWhite | MBlack
  deriving (Eq, Show)

type MasyuClue = Clue MasyuPearl

type IntClue = Clue Int

-- | A Compass clue, specifiying optional numbers in the
--   four cardinal directions.
data CompassC = CC (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)
  deriving (Show)

type CompassClue = Clue CompassC

data SlovakClue = SlovakClue !Int !Int

-- | A cell that is optionally bisected by a diagonal
--   (up-right or down-right).
data Tightfit a = TightSingle a | TightUR a a | TightDR a a

instance Show a => Show (Tightfit a) where
  show c = "(" ++ show' c ++ ")"
    where
      show' (TightSingle x) = show x
      show' (TightUR x y) = show x ++ "/" ++ show y
      show' (TightDR x y) = show x ++ "\\" ++ show y

-- | A marked word in a letter grid, by its start and end
--   coordinates.
data MarkedWord = MW {mwstart :: Coord, mwend :: Coord}

-- | A loop of edges.
type Loop a = [Edge a]

-- | A loop consisting of straight segments of arbitrary
-- angles between vertices.
type VertexLoop = [N]

-- | A thermometer, as a list of coordinates from bulb to end.
--   There should be at least two entries, entries should be distinct,
--   and successive entries should be neighbours (diagonal neighbours
--   are fine).
type Thermometer = [C]

-- | A forward or backward diagonal as occurring in the solution
--   of a slalom puzzle.
data SlalomDiag = SlalomForward | SlalomBackward
  deriving (Show)

data KropkiDot = KNone | KBlack | KWhite
  deriving (Show, Eq, Ord)

newtype TapaClue = TapaClue [Int]
  deriving (Show)

-- | Diagonal marking for Prime Place: forward diag?, backward diag?
newtype PrimeDiag = PrimeDiag (Bool, Bool)

data Black = Black
  deriving (Eq)

data Fish = Fish
  deriving (Eq)

data Star = Star
  deriving (Eq)

data Crossing = Crossing
  deriving (Eq)

type BahnhofClue = Either Int Crossing

data DigitRange = DigitRange !Int !Int
  deriving (Show, Eq)

digitList :: DigitRange -> [Int]
digitList (DigitRange a b) = [a .. b]

data MEnd = MEnd

data Fraction
  = FComp String String String -- a b/c
  | FFrac String String -- a/b
  | FInt String -- a

data PlainNode = PlainNode

type Myopia = [Dir']

data Relation = RGreater | RLess | REqual | RUndetermined
  deriving (Show, Eq)

type GreaterClue = [Relation]

data GalaxyCentre = GalaxyCentre

data PlacedTent = Tent Dir'

data Tree = Tree

data Pentomino = Pentomino Char
  deriving (Show, Eq)
