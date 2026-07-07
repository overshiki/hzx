{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Concrete implementation of parametric ZX diagrams and their operations.
module HZX.Core.Diagram.Parametric.Instances
  ( -- * Diagram type (underscore-prefixed fields)
    ParamDiagram(..)
    -- * Construction
  , pEmpty
  , pIdentityDiagram
  , pAllocVertex
  , pAllocVertices
    -- * Basic operations
  , pMergeEdges
  , pUpdateEdge
  , pRemoveEdgeBundle
  , pAddEdge
  , pRemoveEdge
  , pLookupVertex
  , pAllVertices
  , pNeighborBundles
  , pAdjustVertex
  , pRemoveVertex
    -- * Queries
  , pNumVertices
  , pNumEdges
  , pIsWellFormed
  , pVertexType
  , pNeighbors
  , pDegree
  , pAreConnected
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

import HZX.Core.Diagram.Types
  ( Vertex, BoundaryType(..), EdgeType(..), EdgeBundle(..), emptyBundle
  , bundleHasEdges, addEdgeToBundle, removeEdgeFromBundle, normalizeEdge
  , simpleCount, hadamardCount )
import HZX.Core.Diagram.Parametric.Types
  ( ParamDiagram(..), ParamVertexType(..), ParamScalar(..), ParamPhase(..) )
import HZX.Core.Diagram.Parametric.Scalar (paramScalarOne)

-- | Empty parametric diagram.
pEmpty :: ParamDiagram
pEmpty = ParamDiagram IM.empty M.empty IM.empty [] [] 0 paramScalarOne

-- | Allocate a single vertex.
pAllocVertex :: ParamVertexType -> ParamDiagram -> (Vertex, ParamDiagram)
pAllocVertex ty d =
  let v = _pNextId d
      d' = d { _pVertices = IM.insert v ty (_pVertices d)
             , _pNeighborMap = IM.insert v M.empty (_pNeighborMap d)
             , _pNextId = v + 1
             }
  in (v, d')

-- | Allocate multiple vertices.
pAllocVertices :: [ParamVertexType] -> ParamDiagram -> ([Vertex], ParamDiagram)
pAllocVertices tys d = foldl go ([], d) tys
  where
    go (vs, d') ty = let (v, d'') = pAllocVertex ty d' in (vs ++ [v], d'')

-- | Remove an edge bundle between two vertices.
pRemoveEdgeBundle :: Vertex -> Vertex -> ParamDiagram -> ParamDiagram
pRemoveEdgeBundle v1 v2 d =
  let k = normalizeEdge v1 v2
      d' = d { _pEdges = M.delete k (_pEdges d) }
  in if v1 == v2
     then d' { _pNeighborMap = IM.adjust (M.delete v1) v1 (_pNeighborMap d') }
     else d' { _pNeighborMap = IM.adjust (M.delete v2) v1 $
                            IM.adjust (M.delete v1) v2 $
                            _pNeighborMap d'
               }

-- | Merge an edge bundle into an existing edge between two vertices.
pMergeEdges :: Vertex -> Vertex -> EdgeBundle -> ParamDiagram -> ParamDiagram
pMergeEdges v1 v2 b d =
  let k = normalizeEdge v1 v2
      oldB = M.findWithDefault emptyBundle k (_pEdges d)
      newB = EdgeBundle (simpleCount oldB + simpleCount b)
                        (hadamardCount oldB + hadamardCount b)
  in d { _pEdges = M.insert k newB (_pEdges d)
       , _pNeighborMap = updateNM v1 v2 newB (_pNeighborMap d)
       }
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $
                    IM.adjust (M.insert x bndl) y nm

-- | Replace the edge bundle between two vertices, or remove it if empty.
pUpdateEdge :: Vertex -> Vertex -> EdgeBundle -> ParamDiagram -> ParamDiagram
pUpdateEdge v1 v2 b d =
  let k = normalizeEdge v1 v2
  in if bundleHasEdges b
     then d { _pEdges = M.insert k b (_pEdges d)
            , _pNeighborMap = updateNM v1 v2 b (_pNeighborMap d)
            }
     else pRemoveEdgeBundle v1 v2 d
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $
                    IM.adjust (M.insert x bndl) y nm

-- | Add an edge between two vertices.
pAddEdge :: Vertex -> Vertex -> EdgeType -> ParamDiagram -> ParamDiagram
pAddEdge v1 v2 et d
  | v1 == v2 =
      let k = (v1, v1)
          b = addEdgeToBundle et (M.findWithDefault emptyBundle k (_pEdges d))
      in d { _pEdges = M.insert k b (_pEdges d)
           , _pNeighborMap = IM.adjust (M.insert v1 b) v1 (_pNeighborMap d)
           }
  | otherwise =
      let k = normalizeEdge v1 v2
          b = addEdgeToBundle et (M.findWithDefault emptyBundle k (_pEdges d))
      in d { _pEdges = M.insert k b (_pEdges d)
           , _pNeighborMap = IM.adjust (M.insert v2 b) v1 $
                           IM.adjust (M.insert v1 b) v2 $
                           _pNeighborMap d
           }

-- | Remove an edge between two vertices.
pRemoveEdge :: Vertex -> Vertex -> EdgeType -> ParamDiagram -> ParamDiagram
pRemoveEdge v1 v2 et d =
  let k = normalizeEdge v1 v2
  in case M.lookup k (_pEdges d) of
       Nothing -> d
       Just b  ->
         case removeEdgeFromBundle et b of
           Nothing  -> d
           Just b'  ->
             if bundleHasEdges b'
             then d { _pEdges = M.insert k b' (_pEdges d)
                    , _pNeighborMap = updateNM v1 v2 b' (_pNeighborMap d)
                    }
             else pRemoveEdgeBundle v1 v2 d
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ IM.adjust (M.insert x bndl) y nm

-- | Adjust a vertex type.
pAdjustVertex :: Vertex -> (ParamVertexType -> ParamVertexType) -> ParamDiagram -> ParamDiagram
pAdjustVertex v f d = d { _pVertices = IM.adjust f v (_pVertices d) }

-- | Remove a vertex and all incident edges.
pRemoveVertex :: Vertex -> ParamDiagram -> ParamDiagram
pRemoveVertex v d =
  case IM.lookup v (_pVertices d) of
    Nothing -> d
    Just _  ->
      let ns = maybe [] M.keys (IM.lookup v (_pNeighborMap d))
          d' = foldl (\acc w -> pRemoveEdgeBundle v w acc) d ns
      in d' { _pVertices = IM.delete v (_pVertices d')
            , _pNeighborMap = IM.delete v (_pNeighborMap d')
            , _pInputs = filter (/= v) (_pInputs d')
            , _pOutputs = filter (/= v) (_pOutputs d')
            , _pScalar = _pScalar d'
            }

-- | Look up a vertex type.
pLookupVertex :: Vertex -> ParamDiagram -> Maybe ParamVertexType
pLookupVertex v d = IM.lookup v (_pVertices d)

-- | Get all vertices.
pAllVertices :: ParamDiagram -> [Vertex]
pAllVertices = IM.keys . _pVertices

-- | Get neighbor bundles of a vertex.
pNeighborBundles :: Vertex -> ParamDiagram -> M.Map Vertex EdgeBundle
pNeighborBundles v d = fromMaybe M.empty (IM.lookup v (_pNeighborMap d))

-- | Get neighbors of a vertex.
pNeighbors :: Vertex -> ParamDiagram -> [Vertex]
pNeighbors v = M.keys . pNeighborBundles v

-- | Get degree of a vertex.
pDegree :: Vertex -> ParamDiagram -> Int
pDegree v d = sum [ simpleCount b + hadamardCount b
                  | b <- M.elems (pNeighborBundles v d)
                  ]

-- | Check whether two vertices are connected.
pAreConnected :: Vertex -> Vertex -> ParamDiagram -> Bool
pAreConnected v1 v2 d = M.member (normalizeEdge v1 v2) (_pEdges d)

-- | Number of vertices.
pNumVertices :: ParamDiagram -> Int
pNumVertices = IM.size . _pVertices

-- | Number of edges (counting parallel edges separately).
pNumEdges :: ParamDiagram -> Int
pNumEdges = sum . map edgeCount . M.elems . _pEdges
  where
    edgeCount b = simpleCount b + hadamardCount b

-- | Check whether a parametric diagram is well-formed.
--
--   All input and output boundaries must be boundary vertices with degree 1.
pIsWellFormed :: ParamDiagram -> Bool
pIsWellFormed d =
  all isBoundaryWithDegree1 (_pInputs d) &&
  all isBoundaryWithDegree1 (_pOutputs d) &&
  all (\v -> isJust (IM.lookup v (_pVertices d))) allIO
  where
    allIO = _pInputs d ++ _pOutputs d
    isBoundaryWithDegree1 v =
      case IM.lookup v (_pVertices d) of
        Just (ParamBoundary _) -> pDegree v d == 1
        _                      -> False

-- | Look up the type of a vertex.
pVertexType :: ParamDiagram -> Vertex -> Maybe ParamVertexType
pVertexType d v = pLookupVertex v d

-- | Create an identity parametric diagram with n parallel wires.
pIdentityDiagram :: Int -> ParamDiagram
pIdentityDiagram n = go n pEmpty
  where
    go 0 d = d
    go k d =
      let (inV, d1) = pAllocVertex (ParamBoundary Rough) d
          (outV, d2) = pAllocVertex (ParamBoundary Rough) d1
          d3 = pAddEdge inV outV Simple d2
      in go (k - 1) (d3 { _pInputs = _pInputs d3 ++ [inV]
                        , _pOutputs = _pOutputs d3 ++ [outV]
                        })
