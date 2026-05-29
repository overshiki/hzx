{-# LANGUAGE DefaultSignatures, LambdaCase #-}

module HZX.Core.Diagram.Class
  ( -- * Type Class
    Diagrammatic(..)
    -- * Helper Functions
  , firstMatch
  , allVertexPairs
  ) where

import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ratio (Rational)

import HZX.Core.Diagram.Types
import HZX.Core.Scalar (Scalar)

-- | The Diagrammatic type class abstracts over ZX diagram structures.
--
-- Laws:
-- * lookupVertex v d = Just t  ==>  v `elem` allVertices d
-- * lookupVertex v d = Nothing ==>  v `notElem` allVertices d
-- * After removeVertex v d: lookupVertex v d' = Nothing
-- * After adjustVertex v f d: lookupVertex v d' = f <$> lookupVertex v d
--
class Diagrammatic d where
  -- -----------------------------------------------------------------
  -- Minimal Complete Definition
  -- -----------------------------------------------------------------
  
  -- | Look up a vertex's type. Returns Nothing if vertex doesn't exist.
  lookupVertex :: Vertex -> d -> Maybe VertexType
  
  -- | Get all vertices in the diagram.
  allVertices :: d -> [Vertex]
  
  -- | Adjust a vertex's type by applying a function.
  adjustVertex :: Vertex -> (VertexType -> VertexType) -> d -> d
  
  -- | Remove a vertex and all its incident edges.
  removeVertex :: Vertex -> d -> d
  
  -- | Get neighbors of a vertex with their edge bundles.
  neighborBundles :: Vertex -> d -> M.Map Vertex EdgeBundle
  
  -- | Add an edge between two vertices.
  addEdge :: Vertex -> Vertex -> EdgeType -> d -> d
  
  -- | Remove an edge between two vertices.
  removeEdge :: Vertex -> Vertex -> EdgeType -> d -> d
  
  -- | Get input boundary vertices.
  inputs :: d -> [Vertex]
  
  -- | Get output boundary vertices.
  outputs :: d -> [Vertex]
  
  -- | Get the diagram's scalar factor.
  scalar :: d -> Scalar
  
  -- | Update the diagram's scalar factor.
  mapScalar :: (Scalar -> Scalar) -> d -> d
  
  -- -----------------------------------------------------------------
  -- Predicates (with default implementations)
  -- -----------------------------------------------------------------
  
  -- | Check if vertex is a Z spider.
  isZSpider :: Vertex -> d -> Bool
  default isZSpider :: Vertex -> d -> Bool
  isZSpider v d = case lookupVertex v d of
    Just (Z _) -> True
    _          -> False
  
  -- | Check if vertex is an X spider.
  isXSpider :: Vertex -> d -> Bool
  default isXSpider :: Vertex -> d -> Bool
  isXSpider v d = case lookupVertex v d of
    Just (X _) -> True
    _          -> False
  
  -- | Check if vertex is a boundary.
  isBoundary :: Vertex -> d -> Bool
  default isBoundary :: Vertex -> d -> Bool
  isBoundary v d = case lookupVertex v d of
    Just (Boundary _) -> True
    _                 -> False
  
  -- | Check if vertex is an HBox.
  isHBox :: Vertex -> d -> Bool
  default isHBox :: Vertex -> d -> Bool
  isHBox v d = lookupVertex v d == Just HBox
  
  -- | Check if vertex is a rough boundary.
  isRoughBoundary :: Vertex -> d -> Bool
  default isRoughBoundary :: Vertex -> d -> Bool
  isRoughBoundary v d = case lookupVertex v d of
    Just (Boundary Rough) -> True
    _                     -> False
  
  -- | Check if vertex is a smooth boundary.
  isSmoothBoundary :: Vertex -> d -> Bool
  default isSmoothBoundary :: Vertex -> d -> Bool
  isSmoothBoundary v d = case lookupVertex v d of
    Just (Boundary Smooth) -> True
    _                      -> False
  
  -- | Check if vertex is a Z spider with the given phase.
  isZSpiderWithPhase :: Vertex -> Rational -> d -> Bool
  default isZSpiderWithPhase :: Vertex -> Rational -> d -> Bool
  isZSpiderWithPhase v p d = lookupVertex v d == Just (Z p)
  
  -- | Check if vertex has Pauli phase (0 or π).
  isPauliSpider :: Vertex -> d -> Bool
  default isPauliSpider :: Vertex -> d -> Bool
  isPauliSpider v d = case lookupVertex v d of
    Just (Z p) -> isPauliPhase p
    Just (X p) -> isPauliPhase p
    _          -> False
    where
      isPauliPhase p = p == 0 || p == 1 || p == (-1)
  
  -- -----------------------------------------------------------------
  -- Phase Extraction (with default implementations)
  -- -----------------------------------------------------------------
  
  -- | Get phase of a Z spider.
  getZPhase :: Vertex -> d -> Maybe Rational
  default getZPhase :: Vertex -> d -> Maybe Rational
  getZPhase v d = case lookupVertex v d of
    Just (Z p) -> Just p
    _          -> Nothing
  
  -- | Get phase of an X spider.
  getXPhase :: Vertex -> d -> Maybe Rational
  default getXPhase :: Vertex -> d -> Maybe Rational
  getXPhase v d = case lookupVertex v d of
    Just (X p) -> Just p
    _          -> Nothing
  
  -- -----------------------------------------------------------------
  -- Neighbor Queries (with default implementations)
  -- -----------------------------------------------------------------
  
  -- | Get list of neighbor vertices.
  neighbors :: Vertex -> d -> [Vertex]
  default neighbors :: Vertex -> d -> [Vertex]
  neighbors v d = M.keys (neighborBundles v d)
  
  -- | Get degree of a vertex (total edge count).
  degree :: Vertex -> d -> Int
  default degree :: Vertex -> d -> Int
  degree v d = sum [ simpleCount b + hadamardCount b 
                   | b <- M.elems (neighborBundles v d) ]
  
  -- | Check if two vertices are connected.
  areConnected :: Vertex -> Vertex -> d -> Bool
  default areConnected :: Vertex -> Vertex -> d -> Bool
  areConnected v1 v2 d = M.member v2 (neighborBundles v1 d)
  
  -- | Check if edge between two vertices has a Hadamard component.
  hasHadamardEdge :: Vertex -> Vertex -> d -> Bool
  default hasHadamardEdge :: Vertex -> Vertex -> d -> Bool
  hasHadamardEdge v1 v2 d = case M.lookup v2 (neighborBundles v1 d) of
    Just b  -> hadamardCount b > 0
    Nothing -> False
  
  -- | Check if edge between two vertices has a simple component.
  hasSimpleEdge :: Vertex -> Vertex -> d -> Bool
  default hasSimpleEdge :: Vertex -> Vertex -> d -> Bool
  hasSimpleEdge v1 v2 d = case M.lookup v2 (neighborBundles v1 d) of
    Just b  -> simpleCount b > 0
    Nothing -> False
  
  -- -----------------------------------------------------------------
  -- Search Operations (with default implementations)
  -- -----------------------------------------------------------------
  
  -- | Find all vertices satisfying a predicate.
  findVertices :: (VertexType -> Bool) -> d -> [Vertex]
  default findVertices :: (VertexType -> Bool) -> d -> [Vertex]
  findVertices pred d = 
    [ v | v <- allVertices d
        , Just t <- [lookupVertex v d]
        , pred t ]
  
  -- | Find all Z spiders with their phases.
  findZSpiders :: d -> [(Vertex, Rational)]
  default findZSpiders :: d -> [(Vertex, Rational)]
  findZSpiders d = 
    [ (v, p) | v <- allVertices d
             , Just (Z p) <- [lookupVertex v d] ]
  
  -- | Find all X spiders with their phases.
  findXSpiders :: d -> [(Vertex, Rational)]
  default findXSpiders :: d -> [(Vertex, Rational)]
  findXSpiders d = 
    [ (v, p) | v <- allVertices d
             , Just (X p) <- [lookupVertex v d] ]
  
  -- | Find all Pauli spiders (phase 0 or π).
  findPauliSpiders :: d -> [Vertex]
  default findPauliSpiders :: d -> [Vertex]
  findPauliSpiders d = filter (\v -> isPauliSpider v d) (allVertices d)
  
  -- -----------------------------------------------------------------
  -- Update Operations (with default implementations)
  -- -----------------------------------------------------------------
  
  -- | Set a vertex's type (replace).
  setVertexType :: Vertex -> VertexType -> d -> d
  default setVertexType :: Vertex -> VertexType -> d -> d
  setVertexType v t d = adjustVertex v (const t) d
  
  -- | Adjust a Z spider's phase.
  adjustZPhase :: Vertex -> (Rational -> Rational) -> d -> d
  default adjustZPhase :: Vertex -> (Rational -> Rational) -> d -> d
  adjustZPhase v f d = adjustVertex v (\case Z p -> Z (f p); t -> t) d
  
  -- | Adjust an X spider's phase.
  adjustXPhase :: Vertex -> (Rational -> Rational) -> d -> d
  default adjustXPhase :: Vertex -> (Rational -> Rational) -> d -> d
  adjustXPhase v f d = adjustVertex v (\case X p -> X (f p); t -> t) d

-- | Try all candidates and return the first successful match.
firstMatch :: (a -> Maybe b) -> [a] -> Maybe b
firstMatch f = listToMaybe . mapMaybe f

-- | Get all unordered pairs of vertices.
allVertexPairs :: Diagrammatic d => d -> [(Vertex, Vertex)]
allVertexPairs d = 
  let vs = allVertices d
  in [(v1, v2) | v1 <- vs, v2 <- vs, v1 < v2]
