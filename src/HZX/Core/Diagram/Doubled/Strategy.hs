-- | Simplification strategies for doubled ZX diagrams.
module HZX.Core.Diagram.Doubled.Strategy
  ( DoubledStrategy
  , dRunStrategy
  , dBasicSimp
  ) where

import HZX.Core.Diagram.Doubled.Types (DoubledDiagram)
import HZX.Core.Diagram.Doubled.Rewrite
  ( DoubledRule, dSpiderFusion, dHadamardEdgeSimp
  , dCopyIdentity, dXorIdentity, dCopyFusion, dXorFusion, dMeasureCopy
  )
import HZX.Rewrite.Generic (fixpoint)

type DoubledStrategy = DoubledDiagram -> DoubledDiagram

dRunStrategy :: DoubledRule -> DoubledStrategy
dRunStrategy = fixpoint

dBasicSimp :: DoubledStrategy
dBasicSimp = dRunStrategy basicRule
  where
    basicRule d =
      case dHadamardEdgeSimp d of
        Just d' -> Just d'
        Nothing ->
          case dSpiderFusion d of
            Just d' -> Just d'
            Nothing ->
              case dCopyIdentity d of
                Just d' -> Just d'
                Nothing ->
                  case dXorIdentity d of
                    Just d' -> Just d'
                    Nothing ->
                      case dCopyFusion d of
                        Just d' -> Just d'
                        Nothing ->
                          case dXorFusion d of
                            Just d' -> Just d'
                            Nothing -> dMeasureCopy d
