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

geradeweg_1_sol =
              [ "┌┐┌─┐"
              , "││└┐│  "
              , "│└─┘│"
              , "└──┐│"
              , "  .└┘ "
              ]

unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $
        isJust (parseMaybe (fst geradeweg') (String geradeweg_1))
        @? "parsing failed"
    ]
