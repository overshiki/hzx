{-# LANGUAGE DeriveGeneric #-}

module HZX.Core.Diagram.Types
  ( -- * Core Types
    Vertex
  , BoundaryType(..)
  , VertexType(..)
  , EdgeType(..)
  , EdgeBundle(..)
  , EdgeKey
    -- * Bundle Operations
  , emptyBundle
  , bundleHasEdges
  , addEdgeToBundle
  , removeEdgeFromBundle
    -- * Edge Operations
  , normalizeEdge
  ) where

import GHC.Generics (Generic)

type Vertex = Int

data BoundaryType
  = Rough     -- ^ Z-type boundary (horizontal, connects to green spiders)
  | Smooth    -- ^ X-type boundary (vertical, connects to red spiders)
  deriving (Eq, Ord, Show, Generic)

data VertexType
  = Boundary BoundaryType
  | Z Rational
  | X Rational
  | HBox
  deriving (Eq, Ord, Show, Generic)

data EdgeType
  = Simple
  | Hadamard
  deriving (Eq, Ord, Show, Generic)

data EdgeBundle = EdgeBundle
  { simpleCount :: !Int
  , hadamardCount :: !Int
  } deriving (Eq, Ord, Show, Generic)

type EdgeKey = (Vertex, Vertex)

emptyBundle :: EdgeBundle
emptyBundle = EdgeBundle 0 0

bundleHasEdges :: EdgeBundle -> Bool
bundleHasEdges b = simpleCount b > 0 || hadamardCount b > 0

addEdgeToBundle :: EdgeType -> EdgeBundle -> EdgeBundle
addEdgeToBundle Simple   b = b { simpleCount   = simpleCount b + 1 }
addEdgeToBundle Hadamard b = b { hadamardCount = hadamardCount b + 1 }

removeEdgeFromBundle :: EdgeType -> EdgeBundle -> Maybe EdgeBundle
removeEdgeFromBundle Simple b =
  let n = simpleCount b - 1 in
  if n < 0 then Nothing else Just (b { simpleCount = n })
removeEdgeFromBundle Hadamard b =
  let n = hadamardCount b - 1 in
  if n < 0 then Nothing else Just (b { hadamardCount = n })

normalizeEdge :: Vertex -> Vertex -> EdgeKey
normalizeEdge v w = if v <= w then (v, w) else (w, v)
