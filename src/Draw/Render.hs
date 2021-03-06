{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Render where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Component
import Data.Compose
import Data.Lib
import Data.PuzzleTypes
import Data.Yaml
import Diagrams.Prelude hiding
  ( atop,
    parts,
    render,
    sc,
  )
import Draw.CmdLine
import qualified Draw.Code as Draw
import qualified Draw.Component as Draw
import Draw.Draw
import Draw.Generic
import Draw.Lib (Backend')
import Draw.Widths
import Parse.Code
import Parse.Component
import Parse.Puzzle (TypedPuzzle (..))
import System.FilePath.Posix

data Params
  = Params
      { paramFormat :: Format,
        paramConfig :: Config,
        paramOutputChoice :: OutputChoice,
        paramScale :: Double,
        paramCode :: Bool,
        paramPuzzleFormat :: PuzzleFormat
      }

newtype ParseComponent a = PC {unPC :: TaggedComponent a}

instance FromJSON (ParseComponent a) where
  parseJSON v = PC <$> parseComponent v

decodeAndDraw :: Params -> B.ByteString -> Either String BL.ByteString
decodeAndDraw params b = case backend fmt of
  BackendSVG -> withSize (renderBytesSVG fmt) toDiagram
  BackendRasterific -> withSize (renderBytesRasterific fmt) toDiagram
  where
    Params fmt cfg oc s code pfmt = params
    u = case fmt of
      PDF -> Points
      _ -> Pixels
    withSize ::
      (Monad m, Backend' b) =>
      (SizeSpec V2 Double -> Diagram b -> BL.ByteString) ->
      m (Diagram b) ->
      m BL.ByteString
    withSize f x = do
      d <- x
      let (w, h) = diagramSize d
          sz =
            mkSizeSpec2D
              (Just $ toOutputWidth u (s * w))
              (Just $ toOutputWidth u (s * h))
      return $ f sz d
    toDiagram :: Backend' b => Either String (Diagram b)
    toDiagram = do
      components <- case pfmt of
        PZL -> toComponentsPzl b
        PZG -> toComponentsPzg b
      render cfg components code oc
    toComponentsPzg ::
      Backend' b => B.ByteString -> Either String [TaggedComponent (Drawing b)]
    toComponentsPzg bytes = do
      fmap (map unPC) . mapLeft (\e -> "parse failure: " ++ show e) $
        decodeThrow
          bytes
    toComponentsPzl ::
      Backend' b => B.ByteString -> Either String [TaggedComponent (Drawing b)]
    toComponentsPzl bytes = do
      TP mt mrt p ms mc <-
        mapLeft (\e -> "parse failure: " ++ show e) $
          decodeThrow bytes
      codeComponents <- case (code, mc) of
        (True, Just c) -> mapLeft ("solution code parse failure: " ++) $ do
          parsedCode <- parseEither parseCode c
          return $ Draw.code parsedCode
        _ -> pure []
      t' <- checkType (mrt `mplus` mt)
      if isGeneric t'
        then parseEither (generic t') (p, ms)
        else do
          (pzl, msol) <- parseEither (compose t') (p, ms)
          let fakeSize = (0, 0)
              pc =
                [ TaggedComponent (Just Puzzle)
                    $ PlacedComponent atop
                    $ RawComponent fakeSize pzl
                ]
              sc = case msol of
                Just sol ->
                  [ TaggedComponent (Just Solution)
                      $ PlacedComponent atop
                      $ RawComponent fakeSize sol
                  ]
                Nothing -> []
          return $ concat [pc, sc, codeComponents]

data PuzzleFormat = PZL | PZG
  deriving (Show, Ord, Eq)

lookupPuzzleFormat :: FilePath -> Maybe PuzzleFormat
lookupPuzzleFormat fp = case takeExtension fp of
  ".pzl" -> Just PZL
  ".pzg" -> Just PZG
  _ -> Nothing

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
  deriving (Show)

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
render ::
  Backend' b =>
  Config ->
  [TaggedComponent (Drawing b)] ->
  Bool ->
  OutputChoice ->
  Either String (Diagram b)
render config components code oc = fmap (bg white) $ d oc
  where
    d choice = case choice of
      DrawPuzzle -> Right . fixup $ diagram config pzl
      DrawSolution -> case msol of
        Just sol -> Right . fixup $ diagram config sol
        Nothing -> Left "missing solution"
      DrawExample -> sideBySide <$> d DrawPuzzle <*> d DrawSolution
    fixup = alignPixel . border borderwidth
    sideBySide x y = x ||| strutX 2.0 ||| y
    pzl = Draw.components $ extractPuzzle code components
    msol = fmap Draw.components $ extractSolution code components
