module Data.GridSpec where

import           Data.List                      ( nub
                                                , sort
                                                )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Data.Grid
import           Data.GridShape                 ( N(..)
                                                , C(..)
                                                )

spec :: Spec
spec = do
  describe "sizeGrid" $ do
    it "creates a rectangular grid of nodes" $ do
      nodes (sizeGrid (2, 3))
        `shouldBe` Set.fromList [N 0 0, N 0 1, N 0 2, N 1 0, N 1 1, N 1 2]
      nodes (sizeGrid (0, 0)) `shouldBe` Set.empty
      nodes (sizeGrid (2, 0)) `shouldBe` Set.empty
      nodes (sizeGrid (2, 1)) `shouldBe` Set.fromList [N 0 0, N 1 0]

  describe "nodeGrid" $ do
    it "creates the grid of nodes from a rectangular grid of cells" $ do
      nodes (nodeGrid (sizeGrid (2, 1))) `shouldBe` nodes (sizeGrid (3, 2))

  describe "colour" $ do
    it "colours a line alternatingly" $ do
      let input = [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 1 3), 3)]
      let want  = [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 1 3), 1)]
      colour (Map.fromList input) `shouldBe` (Map.fromList want)
    it "colours a checkerboard" $ do
      let input =
            [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 3), ((C 2 2), 1)]
      let want =
            [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 2), ((C 2 2), 1)]
      colour (Map.fromList input) `shouldBe` (Map.fromList want)
    it "uses three colours to colour a T (a)" $ do
      let input =
            [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 3), ((C 2 2), 3)]
      let count = length . nub . sort . Map.elems
      count (colour (Map.fromList input)) `shouldBe` 3
    it "uses three colours to colour a T (b)" $ do
      let input =
            [((C 1 1), 1 :: Int), ((C 1 2), 1), ((C 2 1), 2), ((C 2 2), 3)]
      let count = length . nub . sort . Map.elems
      count (colour (Map.fromList input)) `shouldBe` 3
    it "uses three colours to colour a T (c)" $ do
      let input =
            [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 3), ((C 2 2), 2)]
      let count = length . nub . sort . Map.elems
      count (colour (Map.fromList input)) `shouldBe` 3
    it "uses three colours to colour a T (d)" $ do
      let input =
            [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 1), ((C 2 2), 3)]
      let count = length . nub . sort . Map.elems
      count (colour (Map.fromList input)) `shouldBe` 3
