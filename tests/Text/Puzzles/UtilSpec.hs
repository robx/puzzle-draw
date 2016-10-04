{-# LANGUAGE TypeFamilies #-}
module Text.Puzzles.UtilSpec where

import qualified Data.Map as Map
import Data.Yaml
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Puzzles.GridShape (Edge(..), N(..), Dir(..))
import Text.Puzzles.Util (parseCoordGrid, parseAnnotatedEdges)

packLines :: [String] -> Value
packLines = String . T.pack . unlines

spec :: Spec
spec = do
    describe "parseCoordGrid" $ do
        it "parses a rectangle of letters" $ do
            let y = packLines [ "abc"
                              , "def"
                              ]
                want = Map.fromList [ ((0, 0), 'd')
                                    , ((1, 0), 'e')
                                    , ((2, 0), 'f')
                                    , ((0, 1), 'a')
                                    , ((1, 1), 'b')
                                    , ((2, 1), 'c')
                                    ]
            parseMaybe parseCoordGrid y `shouldBe` Just want

    describe "parseAnnotatedEdges" $ do
        it "parses some kropki clues" $ do
            let y = packLines [ "+ + + +"
                              , " . .*.o"
                              , "+ +o+ +"
                              , " . . . "
                              , "+ + + +"
                              ]
                want = Map.fromList
                           [ (E (N 1 1) Horiz, 'o')
                           , (E (N 2 1) Vert,  '*')
                           , (E (N 3 1) Vert,  'o')
                           ]
                parseNonempty v = Map.filter ((/=) ' ') <$> parseAnnotatedEdges v
            parseMaybe parseNonempty y `shouldBe` Just want