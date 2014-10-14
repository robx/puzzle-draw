module Diagrams.Puzzles.DSL (
      Puzzle
    , mainWith
    , mainWithNamed
    , readData
    , parseAreaGrid
    , parseCharGrid
    , parseOutsideClues
    , drawAreaGrid
    , drawGrid
    , dashed
    , drawOutsideClues
    , drawOutsideClues'
    , (<>)
    ) where

import qualified Diagrams.Prelude as D

import qualified Data.Yaml as Y
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromJust)
import Control.Applicative

import System.Environment (getProgName)

import qualified Text.Puzzles.Util as Puzzle
import qualified Data.Puzzles.Grid as Puzzle
import Data.Puzzles.Grid (size, clues)
import Diagrams.Puzzles.Elements (drawChar, drawText)
import Diagrams.Puzzles.Grid (atCentres, dashedgrid,
                              outsideCells, outsideVertices)
import qualified Diagrams.Puzzles.Grid as Puzzle

import Diagrams.Puzzles.Draw (
    diagramWidth, toOutputWidth, Unit(..), draw, OutputChoice(..))
import Diagrams.Puzzles.CmdLine

type Grid = Puzzle.CharClueGrid
type OutsideClues = Puzzle.OutsideClues [String]

type AreaGrid = Puzzle.CharGrid

data GridStyle = GridDashed

dashed :: GridStyle
dashed = GridDashed

(<>) :: D.Semigroup a => a -> a -> a
(<>) = (D.<>)

type Diagram = D.Diagram B D.R2

data Env = Env
    { baseName :: String }

type P a = StateT Y.Value (ReaderT Env IO) a

type Puzzle = P Diagram

runPuzzle :: Env -> Puzzle -> IO Diagram
runPuzzle e x = runReaderT (fst <$> runStateT x Y.Null) e

mainWithNamed :: String -> Puzzle -> IO ()
mainWithNamed basename p = do
    d <- runPuzzle (Env basename) p
    let d' = fromJust (draw (d, Nothing) DrawPuzzle) -- FIXME
        ropts = RenderOpts (basename ++ ".png")
                           (toOutputWidth Pixels (diagramWidth d'))
    renderToFile ropts d'

mainWith :: Puzzle -> IO ()
mainWith p = do
    basename <- getProgName
    mainWithNamed basename p

readData :: P ()
readData = do
    base <- asks baseName
    v <- liftIO $ Y.decodeFileEither (base ++ ".pzl")
    case v of Left err    -> liftIO $ exitErr ("failed to parse: " ++ show err)
              Right value -> put value

type Path = [String]

field :: Path -> Y.Value -> Y.Parser Y.Value
field = field' . map T.pack
  where
    field' [] v                = pure v
    field' (f:fs) (Y.Object v) = v Y..: f >>= field' fs
    field' _  _                = empty

parseFrom :: Path -> (Y.Value -> Y.Parser b) -> Y.Value -> Y.Parser b
parseFrom fs p v = field fs v >>= p

parse :: Path -> (Y.Value -> Y.Parser a) -> P a
parse fs p = do
    v <- get
    let ex = Y.parseEither (parseFrom fs p) v
    either (\err -> liftIO $ exitErr ("failed to parse: " ++ show err))
           return
           ex

parseCharGrid :: Path -> P Grid
parseCharGrid p = parse p Puzzle.parseClueGrid

parseAreaGrid :: Path -> P AreaGrid
parseAreaGrid p = parse p Puzzle.parseGrid

parseOutsideClues :: Path -> P OutsideClues
parseOutsideClues p = do
    Puzzle.OC l r b t <- parse p Puzzle.parseMultiOutsideClues
    return $ Puzzle.OC (extend l) (extend r) (extend b) (extend t)
  where
    extend xs = [[]] ++ xs ++ [[]]

drawGrid :: GridStyle -> Grid -> Diagram
drawGrid GridDashed g = dashedgrid (size g)
                      <> atCentres drawChar (clues g)

drawAreaGrid :: AreaGrid -> Diagram
drawAreaGrid = Puzzle.drawAreaGrid

drawOutsideClues' :: OutsideClues -> Diagram
drawOutsideClues' = outsideVertices 0.75 . map (fmap (map drawOne))
                                        . Puzzle.outsideClueList
  where
    drawOne = D.scale 0.8 . drawText

drawOutsideClues :: OutsideClues -> Diagram
drawOutsideClues = outsideCells 0.75 . map (fmap (map drawOne))
                                     . Puzzle.outsideClueList
  where
    drawOne = D.scale 0.8 . drawText
