module Text.Puzzles.PuzzleTypesSpec where

import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Yaml
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Data.Puzzles.GridShape (edgeSize)
import Data.Puzzles.Elements (DigitRange(..))
import Text.Puzzles.PuzzleTypes

packLines :: [String] -> B.ByteString
packLines = encodeUtf8 . T.pack . unlines

parse :: (Value -> Parser a) -> B.ByteString -> Maybe a
parse p t = decode t >>= parseMaybe p

spec :: Spec
spec = do
    describe "abctje" $ do
        it "parses a list of clues" $ do
            let (p, _) = abctje
                y = packLines [ "numbers: 1-10"
                              , "clues:"
                              , "- HELLO: 15"
                              , "- WORLD: 20"
                              , "- weird stuff, too!: 100"
                              ]
            parse p y `shouldBe` Just (DigitRange 1 10, [("HELLO", 15), ("WORLD", 20), ("weird stuff, too!", 100)])
        it "parses a solution" $ do
            let (_, p) = abctje
                y = packLines [ "- 1: A"
                              , "- 100: C"
                              ]
            parse p y `shouldBe` Just [(1, 'A'), (100, 'C')]

    describe "kropki" $ do
        it "parses edges of the right size" $ do
            let (p, _) = kropki
                y = packLines [ "|"
                              , "  + + + +"
                              , "   . . . "
                              , "  +*+ + +"
                              , "   . .o. "
                              , "  + + + +"
                              ]
                res = parse p y
            res `shouldSatisfy` isJust
            let Just m = res
            edgeSize (Map.keys m) `shouldBe` (3, 2)
