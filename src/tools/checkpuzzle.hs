{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Parse.Puzzle
import           Data.PuzzleTypes               ( typeOptions
                                                , PuzzleType(..)
                                                , checkType
                                                )

import           Data.Lib
import           Data.Elements                  ( KropkiDot(..)
                                                , digitList
                                                )
import           Data.GridShape                 ( Edge
                                                , N
                                                , C
                                                , rows
                                                , edgesM
                                                , unorient
                                                , ends
                                                , dualE
                                                )
import           Data.Grid                      ( Grid )
import qualified Parse.PuzzleTypes             as T

import qualified Data.ByteString               as ByteString
import           Options.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Map.Strict               as Map
import           Data.List                      ( intercalate
                                                , sort
                                                )

import           System.Exit
import           System.Environment             ( getProgName )

import qualified Data.Yaml                     as Y

optListTypes :: Parser (a -> a)
optListTypes = infoOption
  (unlines' typeOptions)
  (long "list-types" <> help "List supported puzzle types")
  where unlines' = intercalate "\n"

data PuzzleOpts = PuzzleOpts
    { _type     :: Maybe String
    , _input    :: FilePath
    }

puzzleOpts :: Parser PuzzleOpts
puzzleOpts =
  PuzzleOpts
    <$> ( optional
        . strOption
        $ (long "type" <> short 't' <> metavar "TYPE" <> help
            "Puzzle type, overriding type in input file"
          )
        )
    <*> argument str (metavar "INPUT" <> help "Puzzle file in .pzl format")

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
  prog <- getProgName
  let p = info
        (helper <*> optListTypes <*> optsParser)
        (fullDesc <> progDesc "Command-line puzzle checking." <> header prog)
  execParser p

main :: IO ()
main = do
  opts  <- defaultOpts puzzleOpts
  bytes <- ByteString.readFile (_input opts)
  es    <- orExit $ do
    TP mt _ pv msv _ <- mapLeft (\e -> "parse failure: " ++ show e)
      $ Y.decodeThrow bytes
    t  <- checkType $ _type opts `mplus` mt
    sv <- note "need solution" msv
    Y.parseEither (check t) (pv, sv)
  case es of
    [] -> exitSuccess
    _  -> mapM_ putStrLn es >> exitFailure

orExit :: Either String a -> IO a
orExit (Left  err) = putStrLn err >> exitFailure
orExit (Right r  ) = return r

note :: String -> Maybe a -> Either String a
note err Nothing  = Left err
note _   (Just x) = Right x

check :: PuzzleType -> (Y.Value, Y.Value) -> Y.Parser [String]
check t (pv, sv) = case t of
  ABCtje -> checkABCtje (pv, sv)
  Kropki -> checkKropki (pv, sv)
  _      -> return []

checkABCtje :: (Y.Value, Y.Value) -> Y.Parser [String]
checkABCtje (pv, sv) = do
  p <- fst T.abctje $ pv
  s <- snd T.abctje $ sv
  return . catMaybes . map (\c -> c p s) $ [solutionKeys, values]
 where
  solutionKeys (ds, _) s =
    let have = sort (digitList ds)
        want = sort (map fst (s))
    in  if have /= want then Just "unequal digit lists" else Nothing
  values (_, ws) vs =
    (\es -> case es of
        [] -> Nothing
        _  -> Just $ intercalate ", " es
      )
      . catMaybes
      . map
          (\(w, v) -> if val w == v
            then Nothing
            else Just (w ++ " should be " ++ show (val w))
          )
      $ ws
   where
    l c = fromMaybe 0 . lookup c . map (\(x, y) -> (y, x)) $ vs
    val = sum . map l

checkKropki :: (Y.Value, Y.Value) -> Y.Parser [String]
checkKropki (pv, sv) = do
  p <- fst T.kropki $ pv
  s <- snd T.kropki $ sv
  return . catMaybes . map (\c -> c p s) $ [match, latin, dots]
 where
  match :: Map.Map (Edge N) KropkiDot -> Grid C Int -> Maybe String
  match p s = if solEdges == puzEdges
    then Nothing
    else Just "puzzle and solution shape don't match"
   where
    solEdges =
      let (outer, inner) = edgesM s in sort (inner ++ map unorient outer)
    puzEdges = sort (Map.keys p)
  latin _ s = either Just (const Nothing) . mapM_ latinRow . rows $ s
  latinRow ds = if sort ds == [1 .. length ds]
    then Right ()
    else Left $ "row not 1..N: " ++ show ds
  dots p s = either Just (const Nothing) . mapM_ (checkEdge s) . Map.toList $ p
  checkEdge :: Grid C Int -> (Edge N, KropkiDot) -> Either String ()
  checkEdge s (e, d) =
    let (a, b) = ends (dualE e)
        okDot x y =
          let white = x - y == 1 || x - y == -1
              black = x == y * 2 || y == x * 2
          in  case d of
                KBlack -> black
                KWhite -> white
                KNone  -> not black && not white
    in  case (Map.lookup a s, Map.lookup b s) of
          (Just x, Just y) -> if okDot x y
            then Right ()
            else Left ("bad dot between: " ++ show a ++ "," ++ show b)
          _ -> if d == KNone then Right () else Left "dot on the edge"
