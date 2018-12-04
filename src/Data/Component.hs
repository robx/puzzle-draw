module Data.Component where

import qualified Data.Map.Strict               as Map

import           Data.GridShape
import           Data.Grid
import           Data.Elements

data Component =
    Grid !GridStyle !(Grid C ())
  | Regions !(Grid C Char)
  | NodeGrid !(Grid N Decoration)
  | CellGrid !(Grid C Decoration)
  | EdgeGrid !(Map.Map (Edge N) Decoration)

data Tag =
    Puzzle
  | Solution
 deriving (Eq, Show)

data TaggedComponent = TaggedComponent (Maybe Tag) Component

data GridStyle =
    GridDefault
  | GridDefaultIrregular
  | GridDashed

data Decoration =
    Blank
  | Letter !Char
  | Letters String
  | DecKropkiDot KropkiDot
  | AfternoonWest
  | AfternoonSouth
  | Diagonal PrimeDiag
  | Dot
  | Shade

