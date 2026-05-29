-- |
-- Module      : HZX.Core.Diagram
-- Description : ZX diagram data structure and operations
--
-- This module provides the Diagram type and makes it an instance of
-- the 'Diagrammatic' type class. The record fields use underscore prefix
-- to avoid conflicts with class methods.

module HZX.Core.Diagram
  ( -- * Re-exports from Types
    Vertex
  , BoundaryType(..)
  , VertexType(..)
  , EdgeType(..)
  , EdgeBundle(..)
  , EdgeKey
  , emptyBundle
  , bundleHasEdges
  , addEdgeToBundle
  , removeEdgeFromBundle
  , normalizeEdge
    -- * Diagrammatic type class
  , Diagrammatic
    -- * Class methods (explicitly qualified to avoid conflicts)
  , lookupVertex
  , allVertices
  , adjustVertex
  , removeVertex
  , neighborBundles
  , addEdge
  , removeEdge
  , inputs
  , outputs
  , scalar
  , mapScalar
  , isZSpider
  , isXSpider
  , isBoundary
  , isHBox
  , isRoughBoundary
  , isSmoothBoundary
  , isZSpiderWithPhase
  , isPauliSpider
  , getZPhase
  , getXPhase
  , neighbors
  , degree
  , areConnected
  , hasHadamardEdge
  , hasSimpleEdge
  , findVertices
  , findZSpiders
  , findXSpiders
  , findPauliSpiders
  , setVertexType
  , adjustZPhase
  , adjustXPhase
  , firstMatch
  , allVertexPairs
    -- * Re-exports from Instances
  , Diagram(..)  -- Note: fields have underscore prefix (_vertices, _inputs, etc.)
  , empty
  , identityDiagram
  , allocVertex
  , allocVertices
  , mergeEdges
  , updateEdge
  , removeEdgeBundle
  , numVertices
  , numEdges
  , isWellFormed
  , vertexType
  , getBoundaryType
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram.Types
import HZX.Core.Diagram.Instances
import HZX.Core.Scalar (Scalar)

-- Re-export all class methods from the instance
import HZX.Core.Diagram.Class 
  ( Diagrammatic
  , lookupVertex
  , allVertices
  , adjustVertex
  , removeVertex
  , neighborBundles
  , addEdge
  , removeEdge
  , inputs
  , outputs
  , scalar
  , mapScalar
  , isZSpider
  , isXSpider
  , isBoundary
  , isHBox
  , isRoughBoundary
  , isSmoothBoundary
  , isZSpiderWithPhase
  , isPauliSpider
  , getZPhase
  , getXPhase
  , neighbors
  , degree
  , areConnected
  , hasHadamardEdge
  , hasSimpleEdge
  , findVertices
  , findZSpiders
  , findXSpiders
  , findPauliSpiders
  , setVertexType
  , adjustZPhase
  , adjustXPhase
  , firstMatch
  , allVertexPairs
  )
