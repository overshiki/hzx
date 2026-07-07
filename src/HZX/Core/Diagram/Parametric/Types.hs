{-# LANGUAGE DeriveGeneric #-}

-- | Core types for parametric ZX diagrams.
--
--   Parametric diagrams generalize concrete ZX diagrams by allowing spider
--   phases to depend on symbolic parameters.  In the intended use case for
--   QEC simulation these parameters are binary variables sampled from Pauli
--   noise channels, so phases have the form
--
--     (constant + sum_i coeff_i * e_i) * π
--
--   where e_i ∈ {0, 1}.
module HZX.Core.Diagram.Parametric.Types
  ( ParamPhase(..)
  , ParamScalar(..)
  , ParamVertexType(..)
  , ParamDiagram(..)
  ) where

import GHC.Generics (Generic)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram.Types
  ( Vertex, BoundaryType(..), EdgeBundle, EdgeKey )

-- | A symbolic phase.
--
--   The represented phase is
--
--     (ppConstant + sum_{(k,c) ∈ ppParams} c * e_k) * π
--
--   where the parameters e_k are binary (0 or 1).
data ParamPhase = ParamPhase
  { ppConstant :: !Rational          -- ^ Constant offset (multiple of π)
  , ppParams   :: !(M.Map String Rational)  -- ^ Parameter name → coefficient
  } deriving (Eq, Ord, Show, Generic)

-- | A scalar factor in a parametric diagram.
--
--   Represents (sqrt2Power) * e^(i * phase).
data ParamScalar = ParamScalar
  { psSqrt2Power :: !Int
  , psPhase      :: !ParamPhase
  } deriving (Eq, Show, Generic)

-- | Vertex types in a parametric ZX diagram.
data ParamVertexType
  = ParamBoundary BoundaryType
  | ParamZ ParamPhase
  | ParamX ParamPhase
  | ParamHBox
  deriving (Eq, Show, Generic)

-- | A parametric ZX diagram.
data ParamDiagram = ParamDiagram
  { _pVertices    :: !(IM.IntMap ParamVertexType)
  , _pEdges       :: !(M.Map EdgeKey EdgeBundle)
  , _pNeighborMap :: !(IM.IntMap (M.Map Vertex EdgeBundle))
  , _pInputs      :: ![Vertex]
  , _pOutputs     :: ![Vertex]
  , _pNextId      :: !Vertex
  , _pScalar      :: !ParamScalar
  } deriving (Eq, Show, Generic)
