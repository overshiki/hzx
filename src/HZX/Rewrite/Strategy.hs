module HZX.Rewrite.Strategy
  ( Strategy
  , runStrategy
  , spiderSimp
  , phaseFreeSimp
  , basicSimp
  , toGraphLike
  , cliffordSimp
  ) where

import HZX.Core.Diagram
import HZX.Rewrite.Rule

type Strategy = Diagram -> Diagram

-- | Repeatedly apply a rule until it no longer applies.
runStrategy :: Rule -> Strategy
runStrategy r d =
  case r d of
    Nothing  -> d
    Just d'  -> runStrategy r d'

-- | Spider fusion followed by self-loop removal.
spiderSimp :: Strategy
spiderSimp d = runStrategy spiderFusion (runStrategy selfLoopRemoval d)

-- | A simple strategy applying only phase-free rules.
-- Note: colorChange is excluded to avoid infinite loops.
phaseFreeSimp :: Strategy
phaseFreeSimp = runStrategy phaseFreeRule
  where
    phaseFreeRule d =
      case hadamardEdgeSimp d of
        Just d' -> Just d'
        Nothing ->
          case bialgebraSimp d of
            Just d' -> Just d'
            Nothing ->
              case spiderFusion d of
                Just d' -> Just d'
                Nothing ->
                  case identityRemoval d of
                    Just d' -> Just d'
                    Nothing ->
                      case hopfRule d of
                        Just d' -> Just d'
                        Nothing -> selfLoopRemoval d

-- | Basic simplification: repeatedly apply all left-connected rules.
basicSimp :: Strategy
basicSimp d =
  let d' = phaseFreeSimp d
  in if d' == d then d else basicSimp d'

-- | Convert a diagram to graph-like form:
-- all interior spiders are Z-spiders, all interior edges are Hadamard edges.
toGraphLike :: Strategy
toGraphLike d =
  let d1 = runStrategy hadamardEdgeSimp d
      d2 = runStrategy colorChange d1
      d3 = spiderSimp d2
  in d3

-- | Clifford simplification: convert to graph-like form, then apply
-- local complementation and pivoting until no more rules apply.
cliffordSimp :: Strategy
cliffordSimp d0 =
  let d1 = toGraphLike d0
      d2 = runStrategy cliffordRule d1
  in d2
  where
    cliffordRule d =
      case localComplementation d of
        Just d' -> Just d'
        Nothing ->
          case pivot d of
            Just d' -> Just d'
            Nothing ->
              case spiderFusion d of
                Just d' -> Just d'
                Nothing ->
                  case identityRemoval d of
                    Just d' -> Just d'
                    Nothing -> Nothing
