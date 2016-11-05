import qualified Data.Text as T
import Data.Yaml
import Control.Monad.Identity
import Data.Char
import Control.Applicative
import Data.Maybe

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Draw
import Data.Puzzles.Grid
import Data.Puzzles.Elements
import Diagrams.Puzzles.Elements
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.PuzzleGrids
import Text.Puzzles.Util

outc = OC (reverse (map reverse l)) [] [] (map reverse t)
  where
    l = [[w,w,w,w]
        ,[b,b]
        ,[w,b]
        ,[w]
        ,[b,w,b]
        ,[w,b,w,w,b]
        ,[b,w]
        ,[b,b]
        ,[w,b]
        ]
    t = [[w,b,w]
        ,[w,b,b,b,w]
        ,[w]
        ,[w,w]
        ,[b]
        ,[w,b]
        ,[w,w,b]
        ,[w,b,w]
        ,[b,b,b]
        ]
    w = MWhite
    b = MBlack

puzzle :: OutsideClues [MasyuPearl] -> Diagram B R2
puzzle = atCentres (scale 0.8 . pearl) . multiOutsideClues <> grid . outsideSize

dia = fromJust $ draw (puzzle outc, Nothing) DrawPuzzle

renderd f d = renderCairo f (Width (toOutputWidth Pixels (diagramWidth d))) d

main = renderd "masyureconstruction.png" dia
