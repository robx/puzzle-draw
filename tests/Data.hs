module Data where

import qualified Data.Text as T
import Data.Yaml
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

decodeLines :: [String] -> Value
decodeLines = fromJust . decode . packStr . unlines

packLine :: String -> Value
packLine = String . T.pack

packLines :: [String] -> Value
packLines = String . T.pack . unlines

geradeweg_1 :: Value
geradeweg_1 = packLines $
    [ ".....    "
    , ".211."
    , "..2..  "
    , "3...4"
    , "....."
    ]

geradeweg_1_sol :: Value
geradeweg_1_sol = packLines $
    [ "┌┐┌─┐"
    , "││└┐│  "
    , "│└─┘│"
    , "└──┐│"
    , "  .└┘ "
    ]

tightfit_1 :: Value
tightfit_1 = packLines $
    [ " --- "
    , "3/\\.-"
    , "-\\.\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_broken_1 :: Value
tightfit_broken_1 = packLines $
    [ " --- "
    , "3/\\.-"
    , "-\\x\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_broken_2 :: Value
tightfit_broken_2 = packLines $
    [ "3/\\.-"
    , "-\\x\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_1_sol :: Value
tightfit_1_sol = packLines $
    [ "2/1 4\\5  3"
    , "  4\\5  3  2\\1 "
    , "   3  1\\2 5/4"
    ]

tightfit_sol_broken :: Value
tightfit_sol_broken = packLine "2/1 4 /5"

tightfit_sol_broken_2 :: Value
tightfit_sol_broken_2 = packLine "2/x 4 3/5"

slalom_sol_broken :: Value
slalom_sol_broken = packLine "//\\ /\\x5 "

kpyramid_1 :: Value
kpyramid_1 = packLines $
    [ "G     3"
    , "G    . ."
    , "G   . . ."
    , "W  .o. . ."
    , "G 1*.*.o.*6"
    ]

kpyramid_1_sol :: Value
kpyramid_1_sol = packLines $
    [ "    3"
    , "   8 5"
    , "  1 9 4"
    , " 3 2 7 3"
    , "1 2 4 3 6"
    ]

kpyramid_broken_1 :: Value
kpyramid_broken_1 = packLines $
    [ "  G     3"
    , "  G    . 22"
    , "  H   . aa ."
    , "  W  .o. .|. "
    , "  G 1*.*.o.*6"
    ]

kpyramid_broken_2 :: Value
kpyramid_broken_2 = packLines $
    [ "G     3"
    , "G    . 22"
    , "H   . aa ."
    , "W  .o. .|. "
    , "G 1*.*.o.*6"
    ]

kpyramid_broken_3 :: Value
kpyramid_broken_3 = packLines $
    [ "G     3"
    , "G    . ."
    , "G   . . ."
    , "W  .o. . ."
    , "G a*.*.o.*6"
    ]

compass_1 :: Value
compass_1 = decodeLines $
    [ "grid: |"
    , "  ..."
    , "  a.b"
    , "clues:"
    , "  a: 2 1 . 2"
    , "  b: 21 . . 0"
    ]

compass_broken_1 :: Value
compass_broken_1 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  b: 21 . . 0"
    ]

compass_broken_2 :: Value
compass_broken_2 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: x . . 0"
    , "  b: 21 . . 0"
    ]

compass_broken_3 :: Value
compass_broken_3 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: 1 . ."
    , "  b: 21 . . 0"
    ]

compass_broken_4 :: Value
compass_broken_4 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: 1 . . 2 3"
    , "  b: 21 . . 0"
    ]

compass_broken_5 :: Value
compass_broken_5 = decodeLines $
    [ "grid: |"
    , "  a3b"
    , "clues:"
    , "  a: 1 . . 2 3"
    , "  b: 21 . . 0"
    ]

thermo_1 :: Value
thermo_1 = packLines $
    [ ".b..2."
    , "a.c5.1"
    , ".d..6."
    , ".4..c."
    , "5.3b.d"
    , ".2..a."
    ]

thermo_2 :: Value
thermo_2 = packLines $
    [ "....2."
    , "..c5.1"
    , ".b..6."
    , "a..c.."
    , "a...b."
    , ".bc.a."
    ]

thermo_broken_1 :: Value
thermo_broken_1 = packLines $
    [ ".."
    , "a."
    ]

thermo_broken_2 :: Value
thermo_broken_2 = packLines $
    [ "bb"
    , "a."
    ]

multioutside :: Value
multioutside = decodeLines $
    [ "left:"
    , "  - [2, 1]"
    , "  - [3]"
    , "right:"
    , "  - []"
    , "  - [1, 0]"
    , "bottom:"
    , "  - [0, 0, 1]"
    , "top:"
    , "  - [-1, 1]"
    ]

boxof2or3_1 :: Value
boxof2or3_1 = packLines $
    [ "*-*-*-*-*"
    , "| | | | |"
    , "*-*-*-*-*"
    , "| | |"
    , "*-o-o-o-*"
    , "    | | |"
    , "*-*-*-*-*"
    , "| | | | |"
    , "o-*-*-*-*"
    ]
