{-# LANGUAGE DeriveGeneric #-}

-- | Core types for doubled ZX diagrams.
--
--   Doubled diagrams extend ordinary ZX diagrams with classical information
--   flow.  Quantum wires connect Z/X/H spiders, while classical wires connect
--   COPY and XOR spiders.  Measurements are represented by spiders that
--   consume a quantum wire and produce a classical wire.
module HZX.Core.Diagram.Doubled.Types
  ( DoubledEdgeType(..)
  , EdgeKind(..)
  , DoubledEdgeBundle(..)
  , DoubledVertexType(..)
  , DoubledDiagram(..)
  ) where

import GHC.Generics (Generic)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram.Types (Vertex, BoundaryType(..), EdgeKey)
import HZX.Core.Diagram.Parametric.Types (ParamPhase, ParamScalar)

-- | Whether an edge carries quantum or classical information.
data EdgeKind = Quantum | Classical
  deriving (Eq, Ord, Show, Generic)

-- | Edge types in a doubled diagram.
--
--   Quantum edges can be simple or Hadamard.  Classical edges are always
--   simple (classical information is copied/XORed, not Hadamard-transformed).
data DoubledEdgeType
  = DSimple EdgeKind
  | DHadamard
  deriving (Eq, Ord, Show, Generic)

-- | A bundle of doubled edges between two vertices.
--
--   Quantum simple edges, quantum Hadamard edges, and classical simple edges
--   are counted separately.
data DoubledEdgeBundle = DoubledEdgeBundle
  { qSimpleCount    :: !Int
  , qHadamardCount  :: !Int
  , cSimpleCount    :: !Int
  } deriving (Eq, Ord, Show, Generic)

-- | Vertex types in a doubled ZX diagram.
data DoubledVertexType
  = DBoundary BoundaryType       -- ^ Input/output boundary
  | DZ ParamPhase                -- ^ Doubled Z spider
  | DX ParamPhase                -- ^ Doubled X spider
  | DHBox                        -- ^ Doubled Hadamard box
  | DCopy                        -- ^ Classical COPY spider
  | DXor                         -- ^ Classical XOR spider (addition mod 2)
  | DMeasureZ                    -- ^ Z-basis measurement: quantum -> classical
  | DMeasureX                    -- ^ X-basis measurement: quantum -> classical
  deriving (Eq, Show, Generic)

-- | A doubled ZX diagram.
--
--   Quantum and classical edges are stored in separate maps to enforce the
--   wire-kind discipline at the type level.
data DoubledDiagram = DoubledDiagram
  { _dVertices        :: !(IM.IntMap DoubledVertexType)
  , _qEdges           :: !(M.Map EdgeKey DoubledEdgeBundle)
  , _cEdges           :: !(M.Map EdgeKey DoubledEdgeBundle)
  , _qNeighborMap     :: !(IM.IntMap (M.Map Vertex DoubledEdgeBundle))
  , _cNeighborMap     :: !(IM.IntMap (M.Map Vertex DoubledEdgeBundle))
  , _dInputs            :: ![Vertex]
  , _dOutputs           :: ![Vertex]
  , _dClassicalOutputs  :: ![Vertex]
  , _dObservableOutputs :: ![Vertex]
  , _dNextId            :: !Vertex
  , _dScalar            :: !ParamScalar
  } deriving (Eq, Show, Generic)
