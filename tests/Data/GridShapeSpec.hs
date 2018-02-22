module Data.GridShapeSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.GridShape

spec :: Spec
spec = do
    describe "edgeSize" $ do
        it "computes the size in cells" $ do
            edgeSize [E (N 1 1) Vert, E (N 2 1) Horiz] `shouldBe` (3, 2)
