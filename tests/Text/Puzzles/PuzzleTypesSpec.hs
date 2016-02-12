module Text.Puzzles.PuzzleTypesSpec where

import Data.Yaml
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Text as T

import Test.Hspec (Spec, describe, it, shouldBe)

import Text.Puzzles.PuzzleTypes (abctje)

packLines :: [String] -> B.ByteString
packLines = encodeUtf8 . T.pack . unlines

parse :: (Value -> Parser a) -> B.ByteString -> Maybe a
parse p t = decode t >>= parseMaybe p

spec :: Spec
spec = do
    describe "abctje" $ do
        it "parses a list of mappings to a list of pairs" $ do
            let (p, _) = abctje
                y = packLines [ "- HELLO: 15"
                              , "- WORLD: 20"
                              , "- weird stuff, too!: 100"
                              ]
            parse p y `shouldBe` Just [("HELLO", 15), ("WORLD", 20), ("weird stuff, too!", 100)]
