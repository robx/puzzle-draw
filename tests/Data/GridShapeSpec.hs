module Data.GridShapeSpec where

import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

import           Data.GridShape
import qualified Data.Map.Strict               as Map
import           Data.List                      ( sort )

spec :: Spec
spec = do
  describe "rows" $ do
    it "computes the rows for a simple grid" $ do
      sort
          (rows
            (Map.fromList
              [((C 1 1), 1 :: Int), ((C 1 2), 2), ((C 2 1), 3), ((C 2 2), 4)]
            )
          )
        `shouldBe` sort [[1, 2], [3, 4], [1, 3], [2, 4]]
