{-# LANGUAGE TupleSections #-}

-- | Core types for graph-theoretic ZX circuit extraction.
module HZX.Circuit.Extraction.Types
  ( GFlow(..)
  , QubitIndex
  , ExtractionState(..)
  , ExtractionResult(..)
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram
import HZX.Circuit

-- | A qubit index used during extraction.
type QubitIndex = Int

-- | Focused generalized flow (gflow) on a graph-like ZX diagram.
--   For each non-output vertex we record the set of vertices that correct it
--   and its position in the extraction order (larger number = extracted later,
--   i.e. closer to the inputs).
data GFlow = GFlow
  { gOrder       :: !(IM.IntMap Int)
  , gCorrections :: !(IM.IntMap (M.Map Vertex ()))
  } deriving (Eq, Show)

-- | Mutable state maintained while extracting a circuit.
data ExtractionState = ExtractionState
  { esDiagram  :: !Diagram
  -- ^ Remaining ZX diagram.  Vertices are removed as they are extracted.
  , esFrontier :: !(IM.IntMap Vertex)
  -- ^ Current frontier: qubit index -> vertex that represents the
  --   "current output" of that qubit line.
  , esQubitOf  :: !(IM.IntMap QubitIndex)
  -- ^ Reverse map from a frontier / interior vertex to its logical qubit.
  , esGates    :: ![Gate]
  -- ^ Extracted gates accumulated in reverse extraction order.
  , esGFlow    :: !(Maybe GFlow)
  -- ^ Optional focused gflow for the original graph-like diagram.
  } deriving (Show)

-- | Result of extraction.
data ExtractionResult
  = Extracted
      { extractedCircuit :: !Circuit
      , extractedGFlow   :: !(Maybe GFlow)
      }
  | NotExtractable !String
  | ExtractionError !String
  deriving (Eq, Show)
