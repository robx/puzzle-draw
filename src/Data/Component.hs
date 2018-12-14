module Data.Component where

import qualified Data.Map.Strict               as Map

import           Data.GridShape
import           Data.Grid
import           Data.Elements

data Component a =
    Grid !GridStyle !(Grid C ())
  | Regions !(Grid C Char)
  | NodeGrid !(Grid N Decoration)
  | CellGrid !(Grid C Decoration)
  | EdgeGrid !(Map.Map (Edge N) Decoration)
  | FullGrid !(Grid N Decoration) !(Grid C Decoration) !(Map.Map (Edge N) Decoration)
  | RawComponent !a

data Tag =
    Puzzle
  | Solution
  | Code
 deriving (Eq, Show)

data TaggedComponent a = TaggedComponent (Maybe Tag) (PlacedComponent a)

data Placement =
    Atop
  | West
  | North

data PlacedComponent a = PlacedComponent Placement (Component a)

tagged :: Tag -> TaggedComponent a -> Bool
tagged tag component = case component of
  TaggedComponent (Just t) _ -> tag == t
  _                          -> False

untag :: TaggedComponent a -> PlacedComponent a
untag (TaggedComponent _ c) = c

extractPuzzle :: Bool -> [TaggedComponent a] -> [PlacedComponent a]
extractPuzzle code tcs = map untag . filter want $ tcs
  where want c = not (tagged Solution c) && (code || not (tagged Code c))

extractSolution :: Bool -> [TaggedComponent a] -> Maybe [PlacedComponent a]
extractSolution code tcs = if haveSol
  then Just . map untag . filter want $ tcs
  else Nothing
 where
  haveSol = not . null . filter (tagged Solution) $ tcs
  want c = not (tagged Puzzle c) && (code || not (tagged Code c))

data GridStyle =
    GridDefault
  | GridDefaultIrregular
  | GridDashed
  | GridDots

data Decoration =
    Blank
  | Letter !Char
  | Letters String
  | DecKropkiDot KropkiDot
  | SmallPearl MasyuPearl
  | Pearl MasyuPearl
  | AfternoonWest
  | AfternoonSouth
  | LightDiagonal PrimeDiag
  | DarkDiagonal PrimeDiag
  | Dot
  | SmallDot
  | Shade
  | Edge Dir
  | ThinEdge Dir
  | SolEdge Dir
  | TriangleRight
  | TriangleDown
  | LabeledTriangleRight String
  | LabeledTriangleDown String
