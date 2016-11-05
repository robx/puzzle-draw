import qualified Data.Text as T
import Data.Yaml
import Control.Monad.Identity
import Data.Char
import Control.Applicative
import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Style
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Draw
import Diagrams.Puzzles.CmdLine
import Data.Puzzles.Grid
import Data.Puzzles.GridShape
import Data.Puzzles.Elements
import Diagrams.Puzzles.Elements
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.PuzzleGrids
import Text.Puzzles.Util

outc = OC (reverse (map reverse l)) [] [] (map reverse t)
  where
    l = [[w,b,w]
        ,[b,w]
        ,[w,b]
        ,[b,w]
        ,[w,b]
        ,[w,w,w]
        ,[w,w,w]
        ,[w,w,w,w]
        ]
    t = [[b]
        ,[w,b]
        ,[w,w]
        ,[w,b]
        ,[w,w,w]
        ,[w,b]
        ,[b,w]
        ,[b]
        ]
    w = MWhite
    b = MBlack

puzzle :: OutsideClues C [MasyuPearl] -> Diagram B
puzzle = placeMultiOutside . fmap (fmap (scale 0.8 . pearl))
                     <> grid gDefault . outsideGrid

dia = fromJust $ draw Nothing (puzzle outc, Nothing) DrawPuzzle

main = renderToFile (RenderOpts "paintbythrees.png" (toOutputWidth Pixels . diagramWidth $ dia)) dia
