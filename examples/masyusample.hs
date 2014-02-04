import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Diagrams.Prelude
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine

cluegrid = fmap charToMasyuClue . fromListList $
         [ ".o.*.."
         , "....o."
         , "*..o.."
         , "......"
         , "...o.."
         , "..o..."
         ]

sol :: [(Double, Double)]
sol = [ (1, 0), (5, 0), (5, 5), (4, 5), (4, 1)
      , (2, 1), (2, 2), (3, 2), (3, 5), (0, 5)
      , (0, 4), (2, 4), (2, 3), (0, 3), (0, 1)
      , (1, 1), (1, 0)
      ]

drawSol s = strokeLocLoop (mapLoc glueLine $ fromVertices (map p2 s))
          # lw 0.12 # translate (r2 (0.5, 0.5))

dsol :: Diagram B R2
dsol = drawSol sol

dmasyu :: Diagram B R2
dmasyu = drawMasyuGrid cluegrid

d = (dmasyu ||| strutX 2 ||| (dsol `atop` dmasyu)) # frame # bg white

main = defaultMain d
