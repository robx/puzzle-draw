{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import Data.List (sort)

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server hiding (Config)

import Diagrams.Prelude hiding (Result, (.=), render)

import qualified Data.Aeson as J
import Data.Yaml

import Parse.Code (parseCode)
import Draw.Code (drawCode)
import Draw.CmdLine
import Draw.Draw
import Data.Compose
import Parse.Puzzle
import Data.PuzzleTypes
import Draw.Font (fontAnelizaRegular, fontBit)
import Draw.Lib (Backend')

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import System.Directory
import System.FilePath.Posix
import Data.List (stripPrefix)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop (serveFile "static/index.html") <|>
       route [ ("/api/preview", previewPostHandler)
             , ("/api/download", downloadPostHandler)
             , ("/api/examples", examplesGetHandler) ] <|>
       serveDirectory "static"

fail400 :: String -> Snap a
fail400 e = do
   modifyResponse $ setResponseStatus 400 "Bad Request"
   writeBS . C.pack $ "Bad request: " ++ e
   r <- getResponse
   finishWith r

addContentDisposition :: Format -> B.ByteString -> Snap ()
addContentDisposition fmt filename = do
    modifyResponse $ addHeader "Content-Disposition"
              (B.concat ["attachment; filename=\"", n, "\""])
  where
    n = B.filter (flip B.elem safe) filename <> "." <> (C.pack $ extension fmt)
    safe = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-."

contentType :: Format -> B.ByteString
contentType fmt = case fmt of
    PNG -> "image/png"
    SVG -> "image/svg+xml"
    JPG -> "image/jpeg"
    PDF -> "application/pdf"

serveDiagram :: Format -> Maybe B.ByteString -> BL.ByteString -> Snap ()
serveDiagram format name bs = do
    modifyResponse $ setContentType (contentType format)
    case name of
        Nothing -> return ()
        Just n -> addContentDisposition format n
    writeLBS $ bs

config :: Format -> Config
config fmt = Config device fontAnelizaRegular fontBit
  where
     device = case fmt of
                     PDF -> Print
                     _ -> Screen

decodeAndDrawPuzzle :: Format -> OutputChoice -> Bool -> B.ByteString ->
                       Either String BL.ByteString
decodeAndDrawPuzzle fmt oc code b = 
  case backend fmt of
     BackendSVG -> withSize (renderBytesSVG fmt) (dec b >>= drawP)
     BackendRasterific -> withSize (renderBytesRasterific fmt) (dec b >>= drawP)
  where
    u = case fmt of PDF -> Points
                    _ -> Pixels
    withSize :: (Monad m, Backend' b) => (SizeSpec V2 Double -> Diagram b -> BL.ByteString) -> m (Diagram b) -> m BL.ByteString
    withSize f x = do
      d <- x
      let (w, h) = diagramSize d
          sz = mkSizeSpec2D (Just $ toOutputWidth u w) (Just $ toOutputWidth u h)
      return $ f sz d
    dec :: B.ByteString -> Either String TypedPuzzle
    dec x = case decodeEither' x of
      Left e -> Left $ show e
      Right y -> Right y
    fmapL f e = case e of
        Left l -> Left (f l)
        Right r -> Right r
    drawP :: Backend' b => TypedPuzzle -> Either String (Diagram b)
    drawP (TP mt mrt p ms mc) = do
      mcode <- case (code, mc) of
          (True, Just c) ->
              fmapL ("solution code parse failure: " ++) $ do
                      parsedCode <- parseEither parseCode c
                      return . Just $ drawCode parsedCode
          _ -> return Nothing
      parseEither goP (mt, mrt, (p, ms), mcode)
    goP :: Backend' b => (Maybe String, Maybe String, (Value, Maybe Value), (Maybe (CodeDiagrams (Drawing b)))) -> Parser (Diagram b)
    goP (mt, mrt, x, mcode) = do
        t' <- either fail pure (checkType (mrt `mplus` mt))
        handle (handler mcode) t' x

    handler :: Backend' b => Maybe (CodeDiagrams (Drawing b)) -> PuzzleHandler b ((Value, Maybe Value) -> Parser (Diagram b))
    handler mcode (pp, ps) (Drawers dp ds) (p, ms) = do
          p' <- pp p
          ms' <- maybe (pure Nothing) (fmap Just . ps) ms
          let pzl = dp p'
              sol = do s' <- ms'
                       return (ds (p', s'))
          maybe (fail "no solution provided") return (render (config fmt) mcode (pzl, sol) oc)

getOutputChoice :: Snap OutputChoice
getOutputChoice = do
    ocs <- maybe "puzzle" id <$> getParam "output"
    case ocs of "solution" -> return DrawSolution
                "both"     -> return DrawExample
                "puzzle"   -> return DrawPuzzle
                _          -> fail400 "invalid parameter value: output"

getBoolParam ::  B.ByteString -> Snap Bool
getBoolParam key = do
    param <- getParam key
    case fromMaybe "" param of
      "yes" -> return True
      "true" -> return True
      "1" -> return True
      "no" -> return False
      "false" -> return False
      "0" -> return False
      "" -> return False
      _ -> fail400 "invalid boolean parameter value"

getFormat :: Snap Format
getFormat = do
    fmt <- getParam "format"
    case fmt of
        Nothing -> return SVG
        Just f  -> case lookupFormat (C.unpack f) of
            Nothing     -> fail400 "invalid parameter value: format"
            Just format -> return format

previewPostHandler :: Snap ()
previewPostHandler = do
    outputChoice <- getOutputChoice
    code <- getBoolParam "code"
    body <- readRequestBody 4096
    case decodeAndDrawPuzzle SVG outputChoice code (BL.toStrict body) of
        Left err   -> fail400 err
        Right bytes  -> serveDiagram SVG Nothing bytes

downloadPostHandler :: Snap ()
downloadPostHandler = do
    body <- maybe "" id <$> getParam "pzl"
    outputChoice <- getOutputChoice
    code <- getBoolParam "code"
    format <- getFormat
    fname <- maybe "" id <$> getParam "filename"
    let filename = if fname == "" then "puzzle" else fname
    case decodeAndDrawPuzzle format outputChoice code body of
        Left e      -> fail400 e
        Right bytes -> serveDiagram format (Just filename) bytes

data Example = Example { _name :: String, _path :: FilePath }
    deriving (Show, Ord, Eq)

instance ToJSON Example where
    toJSON (Example n p) = object [ "name" .= n, "path" .= p ]

exampleFromPath :: FilePath -> Maybe Example
exampleFromPath fp = do
    guard $ takeExtension fp == ".pzl"
    let n = stripSuffixMaybe "-example" $ takeBaseName fp
    guard $ length n > 0
    return . Example n $ "./examples" </> fp
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
