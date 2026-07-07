-- | Detector / observable component extraction for doubled ZX diagrams.
module HZX.Core.Diagram.Doubled.Components
  ( dClassicalComponents
  , dHasClassicalStructure
  , dIsDetectorComponent
  , dIsObservableComponent
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import HZX.Core.Diagram.Types (Vertex)
import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..) )
import HZX.Core.Diagram.Doubled.Instances (dLookupVertex, dAllVertices)

-- | Connected components of the classical subgraph.
dClassicalComponents :: DoubledDiagram -> [[Vertex]]
dClassicalComponents d = go S.empty seeds
  where
    seeds = filter hasClassicalEdge (dAllVertices d)
    hasClassicalEdge v = not . M.null $ fromMaybe M.empty $ IM.lookup v (_cNeighborMap d)

    go _ [] = []
    go seen (v:vs)
      | v `S.member` seen = go seen vs
      | otherwise =
          let comp = dfs [v] S.empty
          in S.toList comp : go (S.union seen comp) vs

    dfs [] acc = acc
    dfs (v:stack) acc
      | v `S.member` acc = dfs stack acc
      | otherwise =
          let nbs = M.keys $ fromMaybe M.empty $ IM.lookup v (_cNeighborMap d)
          in dfs (nbs ++ stack) (S.insert v acc)

-- | True if the component contains a classical COPY or XOR spider.
dHasClassicalStructure :: DoubledDiagram -> [Vertex] -> Bool
dHasClassicalStructure d = any isClassicalStructure
  where
    isClassicalStructure v =
      case dLookupVertex v d of
        Just DCopy -> True
        Just DXor  -> True
        _          -> False

-- | True if the component represents a detector.  Detectors are classical
--   structures that terminate in a smooth output boundary.
dIsDetectorComponent :: DoubledDiagram -> [Vertex] -> Bool
dIsDetectorComponent d comp =
  dHasClassicalStructure d comp && any isDetectorOutput comp
  where
    isDetectorOutput v =
      v `elem` _dClassicalOutputs d &&
      case dLookupVertex v d of
        Just (DBoundary _) -> True
        _                  -> False

-- | True if the component represents a logical observable.  Observables are
--   classical components that terminate in an observable output boundary.
dIsObservableComponent :: DoubledDiagram -> [Vertex] -> Bool
dIsObservableComponent d comp =
  dHasClassicalStructure d comp && any (`elem` _dObservableOutputs d) comp
