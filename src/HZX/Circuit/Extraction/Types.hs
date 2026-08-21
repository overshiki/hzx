{-# LANGUAGE TupleSections #-}

-- | Core types for graph-theoretic ZX circuit extraction.
module HZX.Circuit.Extraction.Types
  ( GFlow(..)
  , ExtractionState(..)
  , ExtractionResult(..)
  , QubitIndex
  , emptyExtractionState
  , addGate
  , addGates
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
  , esOutputs  :: !(IM.IntMap Vertex)
  -- ^ Original output boundaries, indexed by qubit.
  , esCircuit  :: ![Gate]
  -- ^ Extracted gates accumulated in reverse order.
  } deriving (Show)

-- | Result of extraction.
data ExtractionResult
  = Extracted !Circuit
  | NotExtractable !String
  | ExtractionError !String
  deriving (Eq, Show)

-- | Build an initial extraction state from a graph-like diagram.
emptyExtractionState :: Diagram -> ExtractionState
emptyExtractionState d =
  ExtractionState
    { esDiagram  = d
    , esFrontier = IM.fromList (zip [0..] (outputs d))
    , esOutputs  = IM.fromList (zip [0..] (outputs d))
    , esCircuit  = []
    }

-- | Prepend a single gate to the extracted circuit.
addGate :: Gate -> ExtractionState -> ExtractionState
addGate g st = st { esCircuit = g : esCircuit st }

-- | Prepend several gates to the extracted circuit.
addGates :: [Gate] -> ExtractionState -> ExtractionState
addGates gs st = st { esCircuit = gs ++ esCircuit st }
