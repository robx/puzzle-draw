import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text, pack)
import Data.Yaml
import Data.Maybe (isJust)

import Data.Puzzles.ReadPuzzle (geradeweg', tightfitskyscrapers')

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

tightfit_1 :: Text
tightfit_1 = pack . unlines $
    [ "3/\\.-"
    , "-\\.\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_1_sol :: Text
tightfit_1_sol = pack . unlines $
    [ "2/1 4\\5  3"
    , "  4\\5  3  2\\1 "
    , "   3  1\\2 5/4"
    ]

testParse p t = isJust (parseMaybe p (String t)) @? "parsing failed"

unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $ testParse (fst geradeweg') geradeweg_1
    , testCase "parse geradeweg solution" $ testParse (snd geradeweg') geradeweg_1_sol
    , testCase "parse tightfit" $ testParse (fst tightfitskyscrapers') tightfit_1
    , testCase "parse tightfit solution" $ testParse (snd tightfitskyscrapers') tightfit_1_sol
    ]
