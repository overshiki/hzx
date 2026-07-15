{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Concrete implementation of doubled ZX diagrams and their operations.
module HZX.Core.Diagram.Doubled.Instances
  ( -- * Diagram type
    DoubledDiagram(..)
    -- * Construction
  , dEmpty
  , dAllocVertex
  , dAllocVertices
    -- * Edge operations
  , dAddEdge
  , dRemoveEdge
  , dRemoveEdgeBundle
  , dMergeEdges
  , dUpdateEdge
    -- * Vertex operations
  , dAdjustVertex
  , dRemoveVertex
    -- * Queries
  , dLookupVertex
  , dAllVertices
  , dQNeighbors
  , dCNeighbors
  , dAllNeighbors
  , dDegree
  , dNumVertices
  , dNumEdges
  , dIsWellFormed
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

import HZX.Core.Diagram.Types (Vertex, normalizeEdge)
import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..), DoubledEdgeType(..)
  , EdgeKind(..), DoubledEdgeBundle(..) )

import HZX.Core.Diagram.Parametric.Scalar (paramScalarOne)
import HZX.Rewrite.Generic (Rewritable(..))

-- | Empty doubled diagram.
dEmpty :: DoubledDiagram
dEmpty = DoubledDiagram IM.empty M.empty M.empty IM.empty IM.empty [] [] [] [] 0 paramScalarOne

-- | Doubled diagrams are rewritable using the generic combinators.
instance Rewritable DoubledDiagram where
  type RVertex DoubledDiagram = DoubledVertexType
  type REdge DoubledDiagram = DoubledEdgeBundle
  rAllVertices = dAllVertices
  rLookupVertex = dLookupVertex
  rAllEdges d = M.toList (_qEdges d) ++ M.toList (_cEdges d)

-- | Allocate a single vertex.
dAllocVertex :: DoubledVertexType -> DoubledDiagram -> (Vertex, DoubledDiagram)
dAllocVertex ty d =
  let v = _dNextId d
      d' = d { _dVertices = IM.insert v ty (_dVertices d)
             , _qNeighborMap = IM.insert v M.empty (_qNeighborMap d)
             , _cNeighborMap = IM.insert v M.empty (_cNeighborMap d)
             , _dNextId = v + 1
             }
  in (v, d')

-- | Allocate multiple vertices.
dAllocVertices :: [DoubledVertexType] -> DoubledDiagram -> ([Vertex], DoubledDiagram)
dAllocVertices tys d = foldl go ([], d) tys
  where
    go (vs, d') ty = let (v, d'') = dAllocVertex ty d' in (vs ++ [v], d'')

-- | Empty edge bundle.
dEmptyBundle :: DoubledEdgeBundle
dEmptyBundle = DoubledEdgeBundle 0 0 0

-- | Check whether an edge bundle has any edges.
dBundleHasEdges :: DoubledEdgeBundle -> Bool
dBundleHasEdges b = qSimpleCount b > 0 || qHadamardCount b > 0 || cSimpleCount b > 0

-- | Add an edge to a bundle.
dAddToBundle :: DoubledEdgeType -> DoubledEdgeBundle -> DoubledEdgeBundle
dAddToBundle (DSimple Quantum)  b = b { qSimpleCount = qSimpleCount b + 1 }
dAddToBundle (DSimple Classical) b = b { cSimpleCount = cSimpleCount b + 1 }
dAddToBundle DHadamard          b = b { qHadamardCount = qHadamardCount b + 1 }

-- | Remove an edge from a bundle.
dRemoveFromBundle :: DoubledEdgeType -> DoubledEdgeBundle -> Maybe DoubledEdgeBundle
dRemoveFromBundle (DSimple Quantum) b =
  let n = qSimpleCount b - 1 in if n < 0 then Nothing else Just (b { qSimpleCount = n })
dRemoveFromBundle (DSimple Classical) b =
  let n = cSimpleCount b - 1 in if n < 0 then Nothing else Just (b { cSimpleCount = n })
dRemoveFromBundle DHadamard b =
  let n = qHadamardCount b - 1 in if n < 0 then Nothing else Just (b { qHadamardCount = n })

-- | Add an edge between two vertices.
dAddEdge :: Vertex -> Vertex -> DoubledEdgeType -> DoubledDiagram -> DoubledDiagram
dAddEdge v1 v2 et d =
  let isQuantum = case et of DHadamard -> True; DSimple Quantum -> True; DSimple Classical -> False
  in if isQuantum
     then let k = normalizeEdge v1 v2
              b = dAddToBundle et (M.findWithDefault dEmptyBundle k (_qEdges d))
          in d { _qEdges = M.insert k b (_qEdges d)
               , _qNeighborMap = updateNM v1 v2 b (_qNeighborMap d)
               }
     else let k = normalizeEdge v1 v2
              b = dAddToBundle et (M.findWithDefault dEmptyBundle k (_cEdges d))
          in d { _cEdges = M.insert k b (_cEdges d)
               , _cNeighborMap = updateNM v1 v2 b (_cNeighborMap d)
               }
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ IM.adjust (M.insert x bndl) y nm

-- | Remove a single edge between two vertices.
dRemoveEdge :: Vertex -> Vertex -> DoubledEdgeType -> DoubledDiagram -> DoubledDiagram
dRemoveEdge v1 v2 et d =
  let isQuantum = case et of DHadamard -> True; DSimple Quantum -> True; DSimple Classical -> False
      edgeMap = if isQuantum then _qEdges d else _cEdges d
      k = normalizeEdge v1 v2
  in case M.lookup k edgeMap of
       Nothing -> d
       Just b  ->
         case dRemoveFromBundle et b of
           Nothing  -> d
           Just b'  ->
             if dBundleHasEdges b'
             then if isQuantum
                  then d { _qEdges = M.insert k b' (_qEdges d)
                         , _qNeighborMap = updateNM v1 v2 b' (_qNeighborMap d)
                         }
                  else d { _cEdges = M.insert k b' (_cEdges d)
                         , _cNeighborMap = updateNM v1 v2 b' (_cNeighborMap d)
                         }
             else dRemoveEdgeBundle v1 v2 et d
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ IM.adjust (M.insert x bndl) y nm

-- | Remove an entire edge bundle between two vertices.
dRemoveEdgeBundle :: Vertex -> Vertex -> DoubledEdgeType -> DoubledDiagram -> DoubledDiagram
dRemoveEdgeBundle v1 v2 et d =
  let isQuantum = case et of DHadamard -> True; DSimple Quantum -> True; DSimple Classical -> False
      k = normalizeEdge v1 v2
  in if isQuantum
     then let d' = d { _qEdges = M.delete k (_qEdges d) }
          in if v1 == v2
             then d' { _qNeighborMap = IM.adjust (M.delete v1) v1 (_qNeighborMap d') }
             else d' { _qNeighborMap = IM.adjust (M.delete v2) v1 $
                                     IM.adjust (M.delete v1) v2 $
                                     _qNeighborMap d'
                       }
     else let d' = d { _cEdges = M.delete k (_cEdges d) }
          in if v1 == v2
             then d' { _cNeighborMap = IM.adjust (M.delete v1) v1 (_cNeighborMap d') }
             else d' { _cNeighborMap = IM.adjust (M.delete v2) v1 $
                                     IM.adjust (M.delete v1) v2 $
                                     _cNeighborMap d'
                       }

-- | Merge an edge bundle into an existing edge.
dMergeEdges :: Vertex -> Vertex -> DoubledEdgeType -> DoubledEdgeBundle -> DoubledDiagram -> DoubledDiagram
dMergeEdges v1 v2 et b d =
  let isQuantum = case et of DHadamard -> True; DSimple Quantum -> True; DSimple Classical -> False
      k = normalizeEdge v1 v2
  in if isQuantum
     then let oldB = M.findWithDefault dEmptyBundle k (_qEdges d)
              newB = DoubledEdgeBundle
                       (qSimpleCount oldB + qSimpleCount b)
                       (qHadamardCount oldB + qHadamardCount b)
                       (cSimpleCount oldB + cSimpleCount b)
          in d { _qEdges = M.insert k newB (_qEdges d)
               , _qNeighborMap = updateNM v1 v2 newB (_qNeighborMap d)
               }
     else let oldB = M.findWithDefault dEmptyBundle k (_cEdges d)
              newB = DoubledEdgeBundle
                       (qSimpleCount oldB + qSimpleCount b)
                       (qHadamardCount oldB + qHadamardCount b)
                       (cSimpleCount oldB + cSimpleCount b)
          in d { _cEdges = M.insert k newB (_cEdges d)
               , _cNeighborMap = updateNM v1 v2 newB (_cNeighborMap d)
               }
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ IM.adjust (M.insert x bndl) y nm

-- | Replace an edge bundle, or remove it if empty.
dUpdateEdge :: Vertex -> Vertex -> DoubledEdgeType -> DoubledEdgeBundle -> DoubledDiagram -> DoubledDiagram
dUpdateEdge v1 v2 et b d =
  if dBundleHasEdges b
  then dMergeEdges v1 v2 et b d  -- simplified: merge replaces
  else dRemoveEdgeBundle v1 v2 et d

-- | Adjust a vertex type.
dAdjustVertex :: Vertex -> (DoubledVertexType -> DoubledVertexType) -> DoubledDiagram -> DoubledDiagram
dAdjustVertex v f d = d { _dVertices = IM.adjust f v (_dVertices d) }

-- | Remove a vertex and all incident edges.
dRemoveVertex :: Vertex -> DoubledDiagram -> DoubledDiagram
dRemoveVertex v d =
  case IM.lookup v (_dVertices d) of
    Nothing -> d
    Just _  ->
      let qNs = maybe [] M.keys (IM.lookup v (_qNeighborMap d))
          cNs = maybe [] M.keys (IM.lookup v (_cNeighborMap d))
          d1 = foldl (\acc w -> dRemoveEdgeBundle v w (DSimple Quantum) acc) d qNs
          d2 = foldl (\acc w -> dRemoveEdgeBundle v w (DSimple Classical) acc) d1 cNs
          d3 = d2 { _qEdges = M.filterWithKey (\(a, b) _ -> a /= v && b /= v) (_qEdges d2)
                  , _cEdges = M.filterWithKey (\(a, b) _ -> a /= v && b /= v) (_cEdges d2)
                  , _dVertices = IM.delete v (_dVertices d2)
                  , _qNeighborMap = IM.delete v (_qNeighborMap d2)
                  , _cNeighborMap = IM.delete v (_cNeighborMap d2)
                  , _dInputs = filter (/= v) (_dInputs d2)
                  , _dOutputs = filter (/= v) (_dOutputs d2)
                  , _dClassicalOutputs = filter (/= v) (_dClassicalOutputs d2)
                  , _dObservableOutputs = filter (/= v) (_dObservableOutputs d2)
                  }
      in d3

-- | Look up a vertex type.
dLookupVertex :: Vertex -> DoubledDiagram -> Maybe DoubledVertexType
dLookupVertex v d = IM.lookup v (_dVertices d)

-- | Get all vertices.
dAllVertices :: DoubledDiagram -> [Vertex]
dAllVertices = IM.keys . _dVertices

-- | Get quantum neighbors.
dQNeighbors :: Vertex -> DoubledDiagram -> [Vertex]
dQNeighbors v = M.keys . fromMaybe M.empty . IM.lookup v . _qNeighborMap

-- | Get classical neighbors.
dCNeighbors :: Vertex -> DoubledDiagram -> [Vertex]
dCNeighbors v = M.keys . fromMaybe M.empty . IM.lookup v . _cNeighborMap

-- | Get all neighbors.
dAllNeighbors :: Vertex -> DoubledDiagram -> [Vertex]
dAllNeighbors v d = dQNeighbors v d ++ dCNeighbors v d

-- | Get degree of a vertex (counting parallel edges separately).
dDegree :: Vertex -> DoubledDiagram -> Int
dDegree v d =
  let qB = M.elems $ fromMaybe M.empty (IM.lookup v (_qNeighborMap d))
      cB = M.elems $ fromMaybe M.empty (IM.lookup v (_cNeighborMap d))
  in sum [ qSimpleCount b + qHadamardCount b | b <- qB ] +
     sum [ cSimpleCount b | b <- cB ]

-- | Number of vertices.
dNumVertices :: DoubledDiagram -> Int
dNumVertices = IM.size . _dVertices

-- | Number of edges (counting parallel edges separately).
dNumEdges :: DoubledDiagram -> Int
dNumEdges d =
  let qCount = sum [ qSimpleCount b + qHadamardCount b | b <- M.elems (_qEdges d) ]
      cCount = sum [ cSimpleCount b | b <- M.elems (_cEdges d) ]
  in qCount + cCount

-- | Check well-formedness.
--
--   All quantum input/output boundaries must have quantum degree 1 and no
--   classical edges.  Classical output boundaries must have classical degree 1.
dIsWellFormed :: DoubledDiagram -> Bool
dIsWellFormed d =
  all (isBoundaryWithQD 1) (_dInputs d) &&
  all (isBoundaryWithQD 1) (_dOutputs d) &&
  all (isBoundaryWithCD 1) (_dClassicalOutputs d) &&
  all (isBoundaryWithCD 1) (_dObservableOutputs d) &&
  all (\v -> isJust (IM.lookup v (_dVertices d))) allIO
  where
    allIO = _dInputs d ++ _dOutputs d ++ _dClassicalOutputs d ++ _dObservableOutputs d

    isBoundaryWithQD deg v =
      case IM.lookup v (_dVertices d) of
        Just (DBoundary _) ->
          qDeg v == deg && cDeg v == 0
        _ -> False

    isBoundaryWithCD deg v =
      case IM.lookup v (_dVertices d) of
        Just (DBoundary _) ->
          cDeg v == deg && qDeg v == 0
        _ -> False

    qDeg v = sum [ qSimpleCount b + qHadamardCount b
                 | b <- M.elems $ fromMaybe M.empty (IM.lookup v (_qNeighborMap d))
                 ]
    cDeg v = sum [ cSimpleCount b
                 | b <- M.elems $ fromMaybe M.empty (IM.lookup v (_cNeighborMap d))
                 ]
