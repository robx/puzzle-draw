module Data.Puzzles.GridSpec where

import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Puzzles.Grid
import Data.Puzzles.GridShape (N(..))

spec :: Spec
spec = do
    describe "sizeGrid" $ do
        it "creates a rectangular grid of nodes" $ do
            nodes (sizeGrid (2, 3)) `shouldBe` Set.fromList [
                                          N 0 0, N 0 1, N 0 2,
                                          N 1 0, N 1 1, N 1 2]
            nodes (sizeGrid (0, 0)) `shouldBe` Set.empty
            nodes (sizeGrid (2, 0)) `shouldBe` Set.empty
            nodes (sizeGrid (2, 1)) `shouldBe ` Set.fromList [N 0 0, N 1 0]

    describe "nodeGrid" $ do
        it "creates the grid of nodes from a rectangular grid of cells" $ do
            nodes (nodeGrid (sizeGrid (2, 1))) `shouldBe` nodes (sizeGrid (3, 2))
