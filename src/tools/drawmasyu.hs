{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

readGrid = liftM (fmap charToMasyuClue . fromListList . filter (not . null) . lines) getContents

drawMasyu g = drawClues pearl (clues g) `atop` gridpx sx sy (lc (blend 0.25 white black))
    where (sx, sy) = size g

main = liftM (bg white . drawMasyu) readGrid >>= defaultMain
