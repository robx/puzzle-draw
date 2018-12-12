{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Control.Monad.IO.Class
import           Data.List                      ( sort )
import           Safe                           ( readMay )

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server        hiding ( Config )

import           Diagrams.Prelude        hiding ( Result
                                                , (.=)
                                                , render
                                                )

import qualified Data.Aeson                    as J
import           Data.Yaml

import           Parse.Code                     ( parseCode )
import           Draw.Code                      ( drawCode )
import           Draw.CmdLine
import           Draw.Draw
import           Data.Compose
import           Data.Component
import           Parse.Component
import           Draw.Component
import           Parse.Puzzle
import           Data.PuzzleTypes
import           Draw.Font                      ( fontAnelizaRegular
                                                , fontBit
                                                )
import           Draw.Lib                       ( Backend' )

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

import           System.Directory
import           System.FilePath.Posix
import           Data.List                      ( stripPrefix )

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  ifTop (serveFile "static/index.html")
    <|> route [("/static/puzzle.html", redirect "/")] -- old demo redirect
    <|> route
          [ ("/api/preview" , previewPostHandler)
          , ("/api/download", downloadPostHandler)
          , ("/api/examples", examplesGetHandler)
          ]
    <|> serveDirectory "static"

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
  n    = B.filter (flip B.elem safe) filename <> "." <> (C.pack $ extension fmt)
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
    Just n  -> addContentDisposition format n
  writeLBS $ bs

config :: Device -> Config
config device = Config device fontAnelizaRegular fontBit

newtype ParseComponent = PC { unPC :: TaggedComponent }

instance FromJSON ParseComponent where
  parseJSON v = PC <$> parseComponent v

decodeAndDrawPuzzle
  :: Format
  -> OutputChoice
  -> Device
  -> Double
  -> Bool
  -> PuzzleFormat
  -> B.ByteString
  -> Either String BL.ByteString
decodeAndDrawPuzzle fmt oc device s code pfmt b = case backend fmt of
  BackendSVG        -> withSize (renderBytesSVG fmt) doit
  BackendRasterific -> withSize (renderBytesRasterific fmt) doit
 where
  u = case fmt of
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

  doit :: Backend' b => Either String (Diagram b)
  doit = case pfmt of
    PZL -> dec b >>= drawP
    PZG -> runPzg b

  runPzg :: Backend' b => B.ByteString -> Either String (Diagram b)
  runPzg bytes = do
    components <-
      fmap (map unPC) . fmapL (\e -> "parse failure: " ++ show e) $ decodeThrow
        bytes
    let
      pzl = drawComponents . extractPuzzle $ components
      sol = fmap drawComponents . extractSolution $ components
    maybe (fail "no solution provided")
          return
          (render (config device) Nothing (pzl, sol) oc)

  dec :: B.ByteString -> Either String TypedPuzzle
  dec x = case decodeEither' x of
    Left  e -> Left $ show e
    Right y -> Right y
  drawP :: Backend' b => TypedPuzzle -> Either String (Diagram b)
  drawP (TP mt mrt p ms mc) = do
    mcode <- case (code, mc) of
      (True, Just c) -> fmapL ("solution code parse failure: " ++) $ do
        parsedCode <- parseEither parseCode c
        return . Just $ drawCode parsedCode
      _ -> return Nothing
    parseEither goP (mt, mrt, (p, ms), mcode)
  goP
    :: Backend' b
    => ( Maybe String
       , Maybe String
       , (Value, Maybe Value)
       , (Maybe (CodeDiagrams (Drawing b)))
       )
    -> Parser (Diagram b)
  goP (mt, mrt, x, mcode) = do
    t' <- either fail pure (checkType (mrt `mplus` mt))
    handle (handler mcode) t' x
  fmapL f e = case e of
    Left  l -> Left (f l)
    Right r -> Right r

  handler
    :: Backend' b
    => Maybe (CodeDiagrams (Drawing b))
    -> PuzzleHandler b ((Value, Maybe Value) -> Parser (Diagram b))
  handler mcode (pp, ps) (Drawers dp ds) (p, ms) = do
    p'  <- pp p
    ms' <- maybe (pure Nothing) (fmap Just . ps) ms
    let pzl = dp p'
        sol = do
          s' <- ms'
          return (ds (p', s'))
    maybe (fail "no solution provided")
          return
          (render (config device) mcode (pzl, sol) oc)

getOutputChoice :: Snap OutputChoice
getOutputChoice = do
  ocs <- maybe "puzzle" id <$> getParam "output"
  case ocs of
    "solution" -> return DrawSolution
    "both"     -> return DrawExample
    "puzzle"   -> return DrawPuzzle
    _          -> fail400 "invalid parameter value: output"

getDevice :: Format -> Snap Device
getDevice fmt = do
  devs <- maybe "auto" id <$> getParam "device"
  return $ case (devs, fmt) of
    ("screen", _  ) -> Screen
    ("print" , _  ) -> Print
    (_       , PDF) -> Print
    _               -> Screen

getBoolParam :: B.ByteString -> Snap Bool
getBoolParam key = do
  param <- getParam key
  case fromMaybe "" param of
    "yes"   -> return True
    "true"  -> return True
    "1"     -> return True
    "no"    -> return False
    "false" -> return False
    "0"     -> return False
    ""      -> return False
    _       -> fail400 "invalid boolean parameter value"

getFormat :: Snap Format
getFormat = do
  fmt <- getParam "format"
  case fmt of
    Nothing -> return SVG
    Just f  -> case lookupFormat (C.unpack f) of
      Nothing     -> fail400 "invalid parameter value: format"
      Just format -> return format

getPuzzleFormat :: Snap PuzzleFormat
getPuzzleFormat = do
  fmt <- getParam "pformat"
  case fmt of
    Nothing    -> return PZL
    Just "pzl" -> return PZL
    Just "pzg" -> return PZG
    _          -> fail400 "invalid parameter value: pformat"

getDouble :: B.ByteString -> Double -> Snap Double
getDouble key defaultValue = do
  param <- getParam key
  case param of
    Nothing -> return defaultValue
    Just "" -> return defaultValue
    Just d  -> case readMay (C.unpack d) of
      Nothing -> fail400 "invalid number parameter"
      Just dd -> return dd

previewPostHandler :: Snap ()
previewPostHandler = do
  outputChoice <- getOutputChoice
  code         <- getBoolParam "code"
  s            <- getDouble "scale" 1.0
  device       <- getDevice SVG
  pzlFormat    <- getPuzzleFormat
  body         <- readRequestBody 4096
  case
      decodeAndDrawPuzzle SVG
                          outputChoice
                          device
                          s
                          code
                          pzlFormat
                          (BL.toStrict body)
    of
      Left  err   -> fail400 err
      Right bytes -> serveDiagram SVG Nothing bytes

downloadPostHandler :: Snap ()
downloadPostHandler = do
  body         <- maybe "" id <$> getParam "pzl"
  outputChoice <- getOutputChoice
  code         <- getBoolParam "code"
  s            <- getDouble "scale" 1.0
  format       <- getFormat
  pzlFormat    <- getPuzzleFormat
  device       <- getDevice format
  fname        <- maybe "" id <$> getParam "filename"
  let filename = if fname == "" then "puzzle" else fname
  case decodeAndDrawPuzzle format outputChoice device s code pzlFormat body of
    Left  e     -> fail400 e
    Right bytes -> serveDiagram format (Just filename) bytes

data PuzzleFormat = PZL | PZG
    deriving (Show, Ord, Eq)

lookupPuzzleFormat :: FilePath -> Maybe PuzzleFormat
lookupPuzzleFormat fp = case takeExtension fp of
  ".pzl" -> Just PZL
  ".pzg" -> Just PZG
  _      -> Nothing

data Example = Example
    { _name :: String
    , _path :: FilePath
    , _puzzleFormat :: PuzzleFormat }
    deriving (Show, Ord, Eq)

instance ToJSON Example where
    toJSON (Example n p f) =
       object
          [ "name" .= n
          , "path" .= p
          , "pformat" .= case f of
                          PZL -> "pzl" :: String
                          PZG -> "pzg" ]

exampleFromPath :: FilePath -> Maybe Example
exampleFromPath fp = case lookupPuzzleFormat fp of
  Nothing  -> Nothing
  Just fmt -> do
    guard $ length n > 0
    return $ Example n ("./examples" </> fp) fmt
 where
  n = stripSuffixMaybe "-example" $ takeBaseName fp
  stripSuffix suffix = fmap reverse . stripPrefix (reverse suffix) . reverse
  stripSuffixMaybe suffix str = case stripSuffix suffix str of
    Just s  -> s
    Nothing -> str

listExamples :: IO [Example]
listExamples = do
  files <- getDirectoryContents "static/examples"
  return . sort . catMaybes . map exampleFromPath $ files

examplesGetHandler :: Snap ()
examplesGetHandler = do
  examples <- liftIO listExamples
  writeLBS $ J.encode examples
