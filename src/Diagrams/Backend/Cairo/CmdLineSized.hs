{-# LANGUAGE TypeFamilies #-}

module Diagrams.Backend.Cairo.CmdLineSized where

import Diagrams.Prelude hiding ((<>), option, value)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.BoundingBox

import Data.Maybe (fromMaybe)

import System.FilePath (splitExtension)

import Options.Applicative

data SizedOpts = SizedOpts
    { _scale   :: Maybe Double
    , _outp    :: String
    }

sizedOpts :: Parser SizedOpts
sizedOpts = SizedOpts
    <$> (optional . option)
            (long "scale" <> short 's'
             <> metavar "FACTOR"
             <> help "Desired scaling factor relative to default size")
    <*> strOption
            (long "output" <> short 'o'
             <> metavar "FILE"
             <> help "Desired output file")

instance Parseable SizedOpts where
    parser = sizedOpts

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

newtype M = M (Diagram Cairo R2)

instance Mainable M where
    type MainOpts M = SizedOpts

    mainRender opts (M x) = do
        let w = fst . unr2 . boxExtents . boundingBox $ x
            w' = (fromMaybe 1 (_scale opts)) * w
            (base, ext) = splitExtension (_outp opts)
            w'' = case ext of
                      ".png" -> round (40 * w')
                      _      -> round . cmtopoint $ w'
            dopts = DiagramOpts (Just w'') Nothing (_outp opts)
            lopts = DiagramLoopOpts False Nothing 0
        mainRender (dopts, lopts) x

instance ToResult M where
    type Args M = ()
    type ResultOf M = M

    toResult d _ = d
