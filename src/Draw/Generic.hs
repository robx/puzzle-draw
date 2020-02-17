module Draw.Generic
  ( generic,
  )
where

import Data.Component
import Data.GridShape
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.PuzzleTypes
import Data.Yaml
import qualified Parse.PuzzleTypes as Parse

generic :: PuzzleType -> (Value, Maybe Value) -> Parser [TaggedComponent a]
generic t (p, ms) = case t of
  Yajilin -> do
    g <- fst Parse.yajilin p
    msol <- traverse (snd Parse.yajilin) ms
    pure
      . catMaybes
      $ [ ( \(x, _) ->
              TaggedComponent (Just Solution)
                $ PlacedComponent Atop
                $ CellGrid
                $ fmap shade
                $ x
          )
            <$> msol,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap lightShade
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap yajClue
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ Grid GridDefault
            $ fmap (const ())
            $ g,
          ( \(_, l) ->
              TaggedComponent (Just Solution)
                $ PlacedComponent Atop
                $ cellEdges l
          )
            <$> msol
        ]
    where
      yajClue = maybe Blank (maybe Blank arr)
      lightShade x = case x of
        Just _ -> LightShade
        Nothing -> Blank
      shade x = if x then Shade else Blank
      arr (v, d) = LabeledArrow d (show v)
  ShakaShaka -> do
    g <- fst Parse.shakashaka p
    pure
      . catMaybes
      $ [ Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap (maybe Blank (const Black))
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap
              ( \x -> case x of
                  Just (Right v) -> InvertedLetters (show v)
                  _ -> Blank
              )
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ Grid GridDashed
            $ fmap (const ())
            $ g
        ]
  RingRing -> do
    g <- fst Parse.ringring p
    msol <- traverse (snd Parse.ringring) ms
    pure
      . catMaybes
      $ [ Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap (maybe Blank (const Black))
            $ g,
          ( \es ->
              TaggedComponent (Just Solution)
                $ PlacedComponent Atop
                $ cellEdges es
          )
            <$> msol,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ Grid GridDashed
            $ fmap (const ())
            $ g
        ]
  _ -> fail $ "puzzle type not implemented as generic: " ++ show t
  where
    cellEdges es =
      EdgeGrid $ solutionEdges $ es
    solutionEdges :: [Edge C] -> Map.Map (Edge N) Decoration
    solutionEdges es = Map.fromList . map (\e@(E _ dir) -> (dualE e, SolEdge dir)) $ es
