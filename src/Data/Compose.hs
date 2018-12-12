{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Helpers to string together parser and renderer by puzzle type.

module Data.Compose
  ( compose
  )
where

import           Data.Yaml                      ( Parser
                                                , Value
                                                )

import           Parse.Puzzle
import           Draw.Draw
import           Draw.Lib
import           Data.PuzzleTypes

import qualified Parse.PuzzleTypes             as R
import qualified Draw.PuzzleTypes              as D

compose
  :: Backend' b
  => PuzzleType
  -> ((Value, Maybe Value) -> Parser (Drawing b, Maybe (Drawing b)))
compose = handle drawPuzzleMaybeSol

-- | A function to compose an arbitrary matching pair of parser and renderer.
--   In @PuzzleHandler b a@, @b@ is the rendering backend type, while @a@ is
--   the result type of the composition.
type PuzzleHandler b a = forall p q.
                         ParsePuzzle p q -> Drawers b p q -> a

-- | @handle h t@ composes the parser and renderer for the puzzle
--   type @t@ with the handler @h@.
handle :: Backend' b => PuzzleHandler b a -> PuzzleType -> a
handle f LITS                = f R.lits D.lits
handle f Geradeweg           = f R.geradeweg D.geradeweg
handle f Fillomino           = f R.fillomino D.fillomino
handle f Masyu               = f R.masyu D.masyu
handle f Nurikabe            = f R.nurikabe D.nurikabe
handle f LatinTapa           = f R.latintapa D.latintapa
handle f Sudoku              = f R.sudoku D.sudoku
handle f ThermoSudoku        = f R.thermosudoku D.thermosudoku
handle f Pyramid             = f R.pyramid D.pyramid
handle f RowKropkiPyramid    = f R.kpyramid D.kpyramid
handle f SlitherLink         = f R.slither D.slither
handle f SlitherLinkLiar     = f R.liarslither D.liarslither
handle f WordLoop            = f R.wordloop D.wordloop
handle f WordSearch          = f R.wordsearch D.wordsearch
handle f CurveData           = f R.curvedata D.curvedata
handle f DoubleBack          = f R.doubleback D.doubleback
handle f Slalom              = f R.slalom D.slalom
handle f Compass             = f R.compass D.compass
handle f MeanderingNumbers   = f R.meanderingnumbers D.meanderingnumbers
handle f Tapa                = f R.tapa D.tapa
handle f JapaneseSums        = f R.japanesesums D.japanesesums
handle f Coral               = f R.coral D.coral
handle f MaximalLengths      = f R.maximallengths D.maximallengths
handle f Labyrinth           = f R.labyrinth D.labyrinth
handle f Bahnhof             = f R.bahnhof D.bahnhof
handle f BlackoutDominos     = f R.blackoutDominos D.blackoutDominos
handle f TwilightTapa        = f R.tapa D.tapa
handle f TapaCave            = f R.tapa D.tapa
handle f DominoPillen        = f R.dominoPills D.dominoPills
handle f AngleLoop           = f R.angleLoop D.angleLoop
handle f Shikaku             = f R.shikaku D.shikaku
handle f SlovakSums          = f R.slovaksums D.slovaksums
handle f Anglers             = f R.anglers D.anglers
handle f Dominos             = f R.dominos D.dominos
handle f FillominoCheckered  = f R.fillomino D.fillominoCheckered
handle f FillominoLoop       = f R.fillominoLoop D.fillominoLoop
handle f Cave                = f R.cave D.cave
handle f Numberlink          = f R.numberlink D.numberlink
handle f Skyscrapers         = f R.skyscrapers D.skyscrapers
handle f SkyscrapersStars    = f R.skyscrapersStars D.skyscrapersStars
handle f SkyscrapersFrac     = f R.tightfitskyscrapers D.tightfitskyscrapers
handle f SkyscrapersTightfit = f R.tightfitskyscrapers D.tightfitskyscrapers
handle f TurningFences       = f R.slither D.slither
handle f Summon              = f R.summon D.summon
handle f Baca                = f R.baca D.baca
handle f Buchstabensalat     = f R.buchstabensalat D.buchstabensalat
handle f Doppelblock         = f R.doppelblock D.doppelblock
handle f SudokuDoppelblock   = f R.sudokuDoppelblock D.sudokuDoppelblock
handle f Loopki              = f R.loopki D.loopki
handle f Scrabble            = f R.scrabble D.scrabble
handle f Neighbors           = f R.neighbors D.neighbors
handle f Starbattle          = f R.starbattle D.starbattle
handle f Heyawake            = f R.heyawake D.heyawake
handle f Pentominous         = f R.pentominous D.pentominous
handle f ColorAkari          = f R.colorakari D.colorakari
handle f PersistenceOfMemory = f R.persistenceOfMemory D.persistenceOfMemory
handle f ABCtje              = f R.abctje D.abctje
handle f Kropki              = f R.kropki D.kropki
handle f StatuePark          = f R.statuepark D.statuepark
handle f PentominousBorders  = f R.pentominousBorders D.pentominousBorders
handle f NanroSignpost       = f R.nanroSignpost D.nanroSignpost
handle f TomTom              = f R.tomTom D.tomTom
handle f Illumination        = f R.illumination D.illumination
handle f Pentopia            = f R.pentopia D.pentopia
handle f GreaterWall         = f R.greaterWall D.greaterWall
handle f Galaxies            = f R.galaxies D.galaxies
handle f Mines               = f R.mines D.mines
handle f Tents               = f R.tents D.tents
handle f PentominoSums       = f R.pentominoSums D.pentominoSums
handle f CoralLITS           = f R.coralLits D.coralLits
handle f CoralLITSO          = f R.coralLitso D.coralLitso
handle f Snake               = f R.snake D.snake
handle f CountryRoad         = f R.countryRoad D.countryRoad
handle f KillerSudoku        = f R.killersudoku D.killersudoku
handle f JapaneseSumsMasyu   = f R.japsummasyu D.japsummasyu


-- | Handler that parses puzzle and an optional solution from a pair of
--   corresponding YAML values, and renders both individually, optionally
--   for the solution.
drawPuzzleMaybeSol
  :: PuzzleHandler
       b
       ((Value, Maybe Value) -> Parser (Drawing b, Maybe (Drawing b)))
drawPuzzleMaybeSol (pp, ps) (Drawers dp ds) (p, s) = do
  p' <- pp p
  s' <- traverse ps s
  let mps = case s' of
        Nothing  -> Nothing
        Just s'' -> Just (p', s'')
  return (dp p', ds <$> mps)
