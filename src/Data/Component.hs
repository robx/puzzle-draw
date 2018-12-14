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
  | FullGrid !(Grid N Decoration) !(Grid C Decoration) !(Map.Map (Edge N) Decoration)

data Tag =
    Puzzle
  | Solution
  | Code
 deriving (Eq, Show)

data TaggedComponent = TaggedComponent (Maybe Tag) PlacedComponent

data Placement =
    Atop
  | West
  | North

data PlacedComponent = PlacedComponent Placement Component

tagged :: Tag -> TaggedComponent -> Bool
tagged tag component = case component of
  TaggedComponent (Just t) _ -> tag == t
  _                          -> False

untag :: TaggedComponent -> PlacedComponent
untag (TaggedComponent _ c) = c

extractPuzzle :: [TaggedComponent] -> [PlacedComponent]
extractPuzzle tcs = map untag . filter (not . tagged Solution) $ tcs

extractSolution :: [TaggedComponent] -> Maybe [PlacedComponent]
extractSolution tcs = if haveSol
  then Just . map untag . filter (not . tagged Puzzle) $ tcs
  else Nothing
  where haveSol = not . null . filter (tagged Solution) $ tcs

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

