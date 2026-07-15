{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HZX.Core.Diagram.Instances
  ( -- * Diagram Type (with underscore-prefixed fields)
    Diagram(..)
    -- * Construction
  , empty
  , identityDiagram
    -- * Basic Operations
  , allocVertex
  , allocVertices
  , mergeEdges
  , updateEdge
  , removeEdgeBundle
    -- * Utilities
  , numVertices
  , numEdges
  , isWellFormed
  , vertexType
  , getBoundaryType
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

import HZX.Core.Diagram.Class
import HZX.Core.Diagram.Types
import HZX.Core.Scalar
import HZX.Rewrite.Generic (Rewritable(..))

-- | Concrete ZX diagram implementation with underscore-prefixed fields.
--
-- The underscore prefix avoids name conflicts with the 'Diagrammatic' 
-- type class methods while providing clean accessor names.
data Diagram = Diagram
  { _vertices    :: !(IM.IntMap VertexType)
  , _edges       :: !(M.Map EdgeKey EdgeBundle)
  , _neighborMap :: !(IM.IntMap (M.Map Vertex EdgeBundle))
  , _inputs      :: ![Vertex]
  , _outputs     :: ![Vertex]
  , _nextId      :: !Vertex
  , _scalar      :: !Scalar
  } deriving (Eq, Show)

-- | The Diagram instance of Diagrammatic.
-- 
-- Uses the default implementations for derived methods.
instance Diagrammatic Diagram where
  -- Minimal implementations
  lookupVertex v d = IM.lookup v (_vertices d)
  
  allVertices = IM.keys . _vertices
  
  adjustVertex v f d = d { _vertices = IM.adjust f v (_vertices d) }
  
  removeVertex v d =
    case IM.lookup v (_vertices d) of
      Nothing -> d
      Just _  ->
        let ns = maybe [] M.keys (IM.lookup v (_neighborMap d))
            d' = foldl (\acc w -> removeEdgeBundle v w acc) d ns
        in d' { _vertices = IM.delete v (_vertices d')
              , _neighborMap = IM.delete v (_neighborMap d')
              , _inputs = filter (/= v) (_inputs d')
              , _outputs = filter (/= v) (_outputs d')
              , _scalar = _scalar d'
              }
  
  neighborBundles v d = fromMaybe M.empty (IM.lookup v (_neighborMap d))
  
  addEdge v1 v2 et d
    | v1 == v2 =
        let k = (v1, v1)
            b = addEdgeToBundle et (M.findWithDefault emptyBundle k (_edges d))
        in d { _edges = M.insert k b (_edges d)
             , _neighborMap = IM.adjust (M.insert v1 b) v1 (_neighborMap d)
             }
    | otherwise =
        let k = normalizeEdge v1 v2
            b = addEdgeToBundle et (M.findWithDefault emptyBundle k (_edges d))
        in d { _edges = M.insert k b (_edges d)
             , _neighborMap = IM.adjust (M.insert v2 b) v1 $
                             IM.adjust (M.insert v1 b) v2 $
                             _neighborMap d
             }
  
  removeEdge v1 v2 et d =
    let k = normalizeEdge v1 v2
    in case M.lookup k (_edges d) of
         Nothing -> d
         Just b  ->
           case removeEdgeFromBundle et b of
             Nothing  -> d
             Just b'  ->
               if bundleHasEdges b'
               then d { _edges = M.insert k b' (_edges d)
                      , _neighborMap = updateNM v1 v2 b' (_neighborMap d)
                      }
               else removeEdgeBundle v1 v2 d
    where
      updateNM x y b nm
        | x == y    = IM.adjust (M.insert x b) x nm
        | otherwise = IM.adjust (M.insert y b) x $ IM.adjust (M.insert x b) y nm
  
  inputs = _inputs
  outputs = _outputs
  scalar = _scalar
  mapScalar f d = d { _scalar = f (_scalar d) }

-- | Concrete diagrams are rewritable using the generic combinators.
instance Rewritable Diagram where
  type RVertex Diagram = VertexType
  type REdge Diagram = EdgeBundle
  rAllVertices = IM.keys . _vertices
  rLookupVertex v d = IM.lookup v (_vertices d)
  rAllEdges d = M.toList (_edges d)

-- | Empty diagram.
empty :: Diagram
empty = Diagram IM.empty M.empty IM.empty [] [] 0 scalarOne

-- | Add a single vertex to the diagram.
allocVertex :: VertexType -> Diagram -> (Vertex, Diagram)
allocVertex ty d =
  let v = _nextId d
      d' = d { _vertices = IM.insert v ty (_vertices d)
             , _neighborMap = IM.insert v M.empty (_neighborMap d)
             , _nextId = v + 1
             }
  in (v, d')

-- | Add multiple vertices to the diagram.
allocVertices :: [VertexType] -> Diagram -> ([Vertex], Diagram)
allocVertices tys d = foldl go ([], d) tys
  where
    go (vs, d') ty = let (v, d'') = allocVertex ty d' in (vs ++ [v], d'')

-- | Remove an edge bundle between two vertices.
removeEdgeBundle :: Vertex -> Vertex -> Diagram -> Diagram
removeEdgeBundle v1 v2 d =
  let k = normalizeEdge v1 v2
      d' = d { _edges = M.delete k (_edges d) }
  in if v1 == v2
     then d' { _neighborMap = IM.adjust (M.delete v1) v1 (_neighborMap d') }
     else d' { _neighborMap = IM.adjust (M.delete v2) v1 $
                            IM.adjust (M.delete v1) v2 $
                            _neighborMap d'
               }

-- | Merge an edge bundle into an existing edge between two vertices.
mergeEdges :: Vertex -> Vertex -> EdgeBundle -> Diagram -> Diagram
mergeEdges v1 v2 b d =
  let k = normalizeEdge v1 v2
      oldB = M.findWithDefault emptyBundle k (_edges d)
      newB = EdgeBundle (simpleCount oldB + simpleCount b) 
                        (hadamardCount oldB + hadamardCount b)
  in d { _edges = M.insert k newB (_edges d)
       , _neighborMap = updateNM v1 v2 newB (_neighborMap d)
       }
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ 
                    IM.adjust (M.insert x bndl) y nm

-- | Replace the edge bundle between two vertices, or remove it if empty.
updateEdge :: Vertex -> Vertex -> EdgeBundle -> Diagram -> Diagram
updateEdge v1 v2 b d =
  let k = normalizeEdge v1 v2
  in if bundleHasEdges b
     then d { _edges = M.insert k b (_edges d)
            , _neighborMap = updateNM v1 v2 b (_neighborMap d)
            }
     else removeEdgeBundle v1 v2 d
  where
    updateNM x y bndl nm
      | x == y    = IM.adjust (M.insert x bndl) x nm
      | otherwise = IM.adjust (M.insert y bndl) x $ 
                    IM.adjust (M.insert x bndl) y nm

-- | Get the number of vertices.
numVertices :: Diagram -> Int
numVertices = IM.size . _vertices

-- | Get the number of edges (counting parallel edges separately).
numEdges :: Diagram -> Int
numEdges = sum . map edgeCount . M.elems . _edges
  where
    edgeCount b = simpleCount b + hadamardCount b

-- | Check if a diagram is well-formed.
isWellFormed :: Diagram -> Bool
isWellFormed d =
  all isBoundaryWithDegree1 (_inputs d) &&
  all isBoundaryWithDegree1 (_outputs d) &&
  all (\v -> isJust (IM.lookup v (_vertices d))) allIO
  where
    allIO = _inputs d ++ _outputs d
    isBoundaryWithDegree1 v =
      case IM.lookup v (_vertices d) of
        Just (Boundary _) -> degree v d == 1
        _                 -> False

-- | Create an identity diagram with n parallel wires.
identityDiagram :: Int -> Diagram
identityDiagram n = go n empty
  where
    go 0 d = d
    go k d =
      let (inV, d1) = allocVertex (Boundary Rough) d
          (outV, d2) = allocVertex (Boundary Rough) d1
          d3 = addEdge inV outV Simple d2
      in go (k - 1) (d3 { _inputs = _inputs d3 ++ [inV]
                        , _outputs = _outputs d3 ++ [outV]
                        , _scalar = _scalar d3
                        })

-- | Look up the type of a vertex.
vertexType :: Diagram -> Vertex -> Maybe VertexType
vertexType d v = lookupVertex v d

-- | Get the boundary type of a vertex, if it is a boundary.
getBoundaryType :: Diagram -> Vertex -> Maybe BoundaryType
getBoundaryType d v = case lookupVertex v d of
  Just (Boundary bt) -> Just bt
  _                  -> Nothing
