module Draw.Generic
  ( generic,
  )
where

import Data.Component
import Data.Grid
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
            $ fmap (const LightShade)
            $ clues
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap yajClue
            $ clues
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
                $ EdgeGrid
                $ es
                $ l
          )
            <$> msol
        ]
    where
      es :: [Edge C] -> Map.Map (Edge N) Decoration
      es l = Map.fromList . map (\e@(E _ dir) -> (dualE e, SolEdge dir)) $ l
      yajClue x = maybe Blank arr x
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
            $ fmap (const Black)
            $ clues
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ CellGrid
            $ fmap (InvertedLetters . show)
            $ rights
            $ clues
            $ g,
          Just
            $ TaggedComponent Nothing
            $ PlacedComponent Atop
            $ Grid GridDashed
            $ fmap (const ())
            $ g
        ]
  _ -> fail $ "puzzle type not implemented as generic: " ++ show t
