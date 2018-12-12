{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Render where

import           Control.Monad

import           System.FilePath.Posix
import           Data.Yaml
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

import           Diagrams.Prelude        hiding ( parts
                                                , render
                                                )

import           Data.Component
import           Data.Compose
import           Parse.Code
import           Parse.Component
import           Data.PuzzleTypes
import           Draw.CmdLine
import           Draw.Code
import           Draw.Component
import           Draw.Draw
import           Draw.Lib                       ( Backend' )
import           Parse.Puzzle                   ( TypedPuzzle(..) )

data Params = Params
  { paramFormat :: Format
  , paramConfig :: Config
  , paramOutputChoice :: OutputChoice
  , paramScale :: Double
  , paramCode :: Bool
  , paramPuzzleFormat :: PuzzleFormat
  }

newtype ParseComponent = PC { unPC :: TaggedComponent }

instance FromJSON ParseComponent where
  parseJSON v = PC <$> parseComponent v

decodeAndDraw :: Params -> B.ByteString -> Either String BL.ByteString
decodeAndDraw params b = case backend fmt of
  BackendSVG        -> withSize (renderBytesSVG fmt) toDiagram
  BackendRasterific -> withSize (renderBytesRasterific fmt) toDiagram
 where
  Params fmt cfg oc s code pfmt = params
  u                             = case fmt of
    PDF -> Points
    _   -> Pixels
  withSize
    :: (Monad m, Backend' b)
    => (SizeSpec V2 Double -> Diagram b -> BL.ByteString)
    -> m (Diagram b)
    -> m BL.ByteString
  withSize f x = do
    d <- x
    let (w, h) = diagramSize d
        sz     = mkSizeSpec2D (Just $ toOutputWidth u (s * w))
                              (Just $ toOutputWidth u (s * h))
    return $ f sz d

  toDiagram :: Backend' b => Either String (Diagram b)
  toDiagram = do
    parts <- case pfmt of
      PZL -> toPartsPzl b
      PZG -> toPartsPzg b
    render cfg parts oc

  toPartsPzg :: Backend' b => B.ByteString -> Either String (Parts b)
  toPartsPzg bytes = do
    components <-
      fmap (map unPC) . fmapL (\e -> "parse failure: " ++ show e) $ decodeThrow
        bytes
    let pzl = drawComponents . extractPuzzle $ components
        sol = fmap drawComponents . extractSolution $ components
    return $ Parts pzl sol Nothing

  toPartsPzl :: Backend' b => B.ByteString -> Either String (Parts b)
  toPartsPzl bytes = do
    TP mt mrt p ms mc <- fmapL (\e -> "parse failure: " ++ show e)
      $ decodeThrow bytes
    mcode <- case (code, mc) of
      (True, Just c) -> fmapL ("solution code parse failure: " ++) $ do
        parsedCode <- parseEither parseCode c
        return . Just $ drawCode parsedCode
      _ -> return Nothing
    t'         <- checkType (mrt `mplus` mt)
    (pzl, sol) <- parseEither (compose t') (p, ms)
    return $ Parts pzl sol mcode
  fmapL f e = case e of
    Left  l -> Left (f l)
    Right r -> Right r

data PuzzleFormat = PZL | PZG
    deriving (Show, Ord, Eq)

lookupPuzzleFormat :: FilePath -> Maybe PuzzleFormat
lookupPuzzleFormat fp = case takeExtension fp of
  ".pzl" -> Just PZL
  ".pzg" -> Just PZG
  _      -> Nothing

