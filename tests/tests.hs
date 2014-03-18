import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text, pack)
import Data.Yaml
import Data.Maybe (isJust)

import Data.Puzzles.ReadPuzzle (geradeweg')

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

geradeweg_1 :: Text
geradeweg_1 = pack . unlines $
    [ ".....    "
    , ".211."
    , "..2..  "
    , "3...4"
    , "....."
    ]

geradeweg_1_sol = pack . unlines $
    [ "┌┐┌─┐"
    , "││└┐│  "
    , "│└─┘│"
    , "└──┐│"
    , "  .└┘ "
    ]

testParse p t = isJust (parseMaybe p (String t)) @? "parsing failed"

unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $ testParse (fst geradeweg') geradeweg_1
    , testCase "parse geradeweg solution" $ testParse (snd geradeweg') geradeweg_1_sol
    ]
