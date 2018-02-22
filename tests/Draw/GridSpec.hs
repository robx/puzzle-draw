module Draw.GridSpec where

import Diagrams.Prelude (p2)
import Diagrams.Path (pathPoints)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Grid
import Data.GridShape
import Draw.Grid
import Draw.Lib (p2i)

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

    describe "midPoint" $ do
        it "gives the center of a node edge" $ do
            let e = E (N 0 1) Horiz
            midPoint e `shouldBe` p2 (0.5, 1.0)
        it "gives the center of a cell edge" $ do
            let e = E (C 0 0) Vert
            midPoint e `shouldBe` p2 (0.5, 1.0)
