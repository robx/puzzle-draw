module Draw.GridSpec where

import Data.Grid
import Data.GridShape
import Diagrams.Path (pathPoints)
import Diagrams.Prelude (p2)
import Draw.Grid
import Draw.Lib (p2i)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )

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
  describe "offsetBorder" $ do
    it "gives the corners of a square cell" $ do
      let b = offsetBorder 0 [C 0 0]
          [vs] = pathPoints b
      length vs `shouldBe` 4
      vs `shouldSatisfy` elem (p2i (0, 0))
      vs `shouldSatisfy` elem (p2i (1, 0))
      vs `shouldSatisfy` elem (p2i (0, 1))
      vs `shouldSatisfy` elem (p2i (1, 1))
    it "omits in-between nodes of a rectangle" $ do
      let b = offsetBorder 0 [C 0 0, C 1 0]
          [vs] = pathPoints b
      length vs `shouldBe` 4
      vs `shouldSatisfy` elem (p2i (0, 0))
      vs `shouldSatisfy` elem (p2i (2, 0))
      vs `shouldSatisfy` elem (p2i (0, 1))
      vs `shouldSatisfy` elem (p2i (2, 1))
    it "offsets inside for negative offset" $ do
      let b = offsetBorder (-0.5) [C 0 0, C 1 0, C 1 1, C 0 1]
          [vs] = pathPoints b
      length vs `shouldBe` 4
      vs `shouldSatisfy` elem (p2 (0.5, 0.5))
      vs `shouldSatisfy` elem (p2 (0.5, 1.5))
      vs `shouldSatisfy` elem (p2 (1.5, 1.5))
      vs `shouldSatisfy` elem (p2 (1.5, 0.5))
