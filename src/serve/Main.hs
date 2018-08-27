{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import Data.List (sort)

import System.IO.Unsafe (unsafePerformIO)

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server hiding (Config)

import Diagrams.Prelude hiding (Result, (.=), render)
import Diagrams.Backend.SVG

import qualified Data.Aeson as J
import Data.Yaml

import Draw.Draw
import Data.Compose
import Parse.Puzzle
import Data.PuzzleTypes
import Draw.Lib (fontAnelizaRegular, fontBit)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Graphics.Svg.Core (renderText)

import System.Directory
import System.FilePath.Posix
import Data.List (stripPrefix)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop (redirect "static/puzzle.html") <|>
       route [ ("puzzle", puzzlePostHandler)
             , ("examples", examplesGetHandler) ] <|>
       dir "static" (serveDirectory "static")

fail400 :: String -> Snap ()
fail400 e = do
   modifyResponse $ setResponseStatus 400 "Bad Request"
   writeBS . C.pack $ "Bad request: " ++ e
   r <- getResponse
   finishWith r

serveDiagram :: SizeSpec V2 Double -> Diagram B -> Snap ()
serveDiagram sz d = do
    let svg = renderDia SVG (SVGOptions sz Nothing (Text.pack "") [] True) d
    modifyResponse $ setContentType "image/svg+xml"
    writeLazyText . renderText $ svg

sizeServeDiagram :: Diagram B -> Snap ()
sizeServeDiagram d = serveDiagram w d
  where
    w = mkWidth . toOutputWidth Pixels . fst . diagramSize $ d

loadConfig :: IO Config
loadConfig = do
    var <- fontAnelizaRegular
    fix <- fontBit
    return $ Config Screen var fix

{-# NOINLINE config #-}
config :: Config
config = unsafePerformIO loadConfig

decodeAndDrawPuzzle :: OutputChoice -> B.ByteString ->
                       Either String (Diagram B)
decodeAndDrawPuzzle oc b = dec b >>= drawP
  where
    dec x = case decodeEither' x of
      Left e -> Left $ show e
      Right y -> Right y
    drawP :: TypedPuzzle -> Either String (Diagram B)
    drawP (TP mt mrt p ms _mc) = parseEither goP (mt, mrt, (p, ms))
    goP :: (Maybe String, Maybe String, (Value, Maybe Value)) -> Parser (Diagram B)
    goP (mt, mrt, x) = do
        t' <- either fail pure (checkType (mrt `mplus` mt))
        handle handler t' x
    handler :: PuzzleHandler B ((Value, Maybe Value) -> Parser (Diagram B))
    handler (pp, ps) (Drawers dp ds) (p, ms) = do
        p' <- pp p
        ms' <- maybe (pure Nothing) (fmap Just . ps) ms
        let pzl = dp p'
            sol = do s' <- ms'
                     return (ds (p', s'))
        maybe (fail "no solution provided") return (render config Nothing (pzl, sol) oc)

getOutputChoice :: Snap OutputChoice
getOutputChoice = do
    ocs <- maybe "puzzle" id <$> getParam "output"
    return $ case ocs of "solution" -> DrawSolution
                         "both"     -> DrawExample
                         _          -> DrawPuzzle

puzzlePostHandler :: Snap ()
puzzlePostHandler = do
    o <- getOutputChoice
    body <- readRequestBody 4096
    case decodeAndDrawPuzzle o (BL.toStrict body) of
        Left e   -> fail400 e
        Right d  -> sizeServeDiagram d

data Example = Example { name :: String, path :: FilePath }
    deriving (Show, Ord, Eq)

instance ToJSON Example where
    toJSON (Example n p) = object [ "name" .= n, "path" .= p ]

exampleFromPath :: FilePath -> Maybe Example
exampleFromPath fp = do
    guard $ takeExtension fp == ".pzl"
    let n = stripSuffixMaybe "-example" $ takeBaseName fp
    guard $ length n > 0
    return . Example n $ ".." </> "static" </> "examples" </> fp
  where
    stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse
    stripSuffixMaybe suffix str = case stripSuffix suffix str of
        Just s -> s
        Nothing -> str

listExamples :: IO [Example]
listExamples = do
    files <- getDirectoryContents "static/examples"
    return . sort . catMaybes . map exampleFromPath $ files

examplesGetHandler :: Snap ()
examplesGetHandler = do
    examples <- liftIO listExamples
    writeLBS $ J.encode examples
