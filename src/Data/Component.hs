module Data.Component where

import Data.Default.Class
import Data.Elements
import Data.Grid
import Data.GridShape
import qualified Data.Map.Strict as Map

data Component a
  = Grid !GridStyle !(Grid C ())
  | Regions !(Grid C Char)
  | NodeGrid !(Grid N Decoration)
  | CellGrid !(Grid C Decoration)
  | EdgeGrid !(Map.Map (Edge N) Decoration)
  | FullGrid !(Grid N Decoration) !(Grid C Decoration) !(Map.Map (Edge N) Decoration)
  | Pyramid !(Grid ShiftC ())
  | CellPyramid !(Grid ShiftC Decoration)
  | Note [Decoration]
  | Rows [[Decoration]]
  | Columns [[Decoration]]
  | RawComponent !Size !a

data Tag
  = Puzzle
  | Solution
  | Code
  deriving (Eq, Show)

data TaggedComponent a = TaggedComponent (Maybe Tag) (PlacedComponent a)

data PlacementDir
  = Atop
  | West
  | North
  | South
  | East
  | TopRight
  deriving (Eq, Show)

data PlacementAlignment
  = AlignCenter
  | AlignBottom
  | AlignTop
  | AlignLeft
  | AlignRight

data PlacementMargin
  = MarginFar
  | MarginClose
  | MarginCustom Double

data Placement
  = Placement
      { _direction :: PlacementDir,
        _margin :: PlacementMargin,
        _alignment :: PlacementAlignment
      }

instance Default Placement where
  def = Placement {_direction = Atop, _margin = MarginFar, _alignment = AlignCenter}

atop :: Placement
atop = def {_direction = Atop}

data PlacedComponent a = PlacedComponent Placement (Component a)

tagged :: Tag -> TaggedComponent a -> Bool
tagged tag component = case component of
  TaggedComponent (Just t) _ -> tag == t
  _ -> False

untag :: TaggedComponent a -> PlacedComponent a
untag (TaggedComponent _ c) = c

extractPuzzle :: Bool -> [TaggedComponent a] -> [PlacedComponent a]
extractPuzzle code tcs = map untag . filter want $ tcs
  where
    want c = not (tagged Solution c) && (code || not (tagged Code c))

extractSolution :: Bool -> [TaggedComponent a] -> Maybe [PlacedComponent a]
extractSolution code tcs =
  if haveSol
    then Just . map untag . filter want $ tcs
    else Nothing
  where
    haveSol = not . null . filter (tagged Solution) $ tcs
    want c = not (tagged Puzzle c) && (code || not (tagged Code c))

data GridStyle
  = GridDefault
  | GridDefaultIrregular
  | GridDashed
  | GridDots
  | GridPlain
  | GridPlainDashed

data Decoration
  = Blank
  | Letter !Char
  | Letters String
  | InvertedLetters String
  | DecKropkiDot KropkiDot
  | SmallPearl MasyuPearl
  | Pearl MasyuPearl
  | AfternoonWest
  | AfternoonSouth
  | LightDiagonal PrimeDiag
  | DarkDiagonal PrimeDiag
  | Dot
  | SmallDot
  | Star
  | Shade
  | Black
  | LightShade
  | DarkShade
  | Edge Dir
  | ThinEdge Dir
  | SolEdge Dir
  | TriangleRight
  | TriangleDown
  | LabeledTriangleRight String
  | LabeledTriangleDown String
  | MiniLoop
  | Ship Dir'
  | ShipSquare
  | LabeledArrow Dir' String
  | InvertedLabeledArrow Dir' String
  | Tent
  | Tree
  | Myopia [Dir']
  | Triangle CornerDir
