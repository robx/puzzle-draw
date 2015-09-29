module Diagrams.Puzzles.GridSpec where

import Diagrams.Path (pathPoints)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Puzzles.Grid
import Data.Puzzles.GridShape
import Diagrams.Puzzles.Grid
import Diagrams.Puzzles.Lib (p2i)

spec :: Spec
spec = do
    describe "irregularGridPaths" $ do
        it "gives the border of a rectangular grid" $ do
            let g = sizeGrid (2, 3) :: Grid C ()
                (outer, _) = irregularGridPaths g
                pts = pathPoints outer
            length pts `shouldBe` 1
            let [opts] = pts
            opts `shouldSatisfy` elem (p2i (0, 0))
            opts `shouldSatisfy` elem (p2i (2, 0))
            opts `shouldSatisfy` elem (p2i (2, 3))
            opts `shouldSatisfy` elem (p2i (0, 3))
