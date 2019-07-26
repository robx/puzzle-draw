{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Control.Monad.IO.Class
import           Data.List                      ( sort )
import           Safe                           ( readMay )

import           Snap.Core               hiding ( getParams
                                                , Params
                                                , dir
                                                )
import           Snap.Util.FileServe
import           Snap.Http.Server        hiding ( Config )

import qualified Data.Aeson                    as J
import           Data.Yaml

import           Draw.CmdLine
import           Draw.Draw
import           Draw.Render
import           Draw.Font                      ( fontAnelizaRegular
                                                , fontBit
                                                )

import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

import           System.Directory
import           System.FilePath.Posix
import           System.Environment
import           Data.List                      ( stripPrefix )

main :: IO ()
main = do
  root <- lookupEnv "PUZZLE_DRAW_ROOT"
  case root of
    Just dir -> setCurrentDirectory dir
    Nothing  -> return ()
  quickHttpServe site

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

getParams :: Format -> Snap Params
getParams fmt = do
  device <- getDevice fmt
  Params fmt (config device)
    <$> getOutputChoice
    <*> getDouble "scale" 1.0
    <*> getBoolParam "code"
    <*> getPuzzleFormat

previewPostHandler :: Snap ()
previewPostHandler = do
  params <- getParams SVG
  body   <- readRequestBody 4096
  case decodeAndDraw params (BL.toStrict body) of
    Left  err   -> fail400 err
    Right bytes -> serveDiagram SVG Nothing bytes

downloadPostHandler :: Snap ()
downloadPostHandler = do
  body   <- maybe "" id <$> getParam "pzl"
  format <- getFormat
  params <- getParams format
  fname  <- maybe "" id <$> getParam "filename"
  let filename = if fname == "" then "puzzle" else fname
  case decodeAndDraw params body of
    Left  e     -> fail400 e
    Right bytes -> serveDiagram format (Just filename) bytes

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
