{-# LANGUAGE TupleSections #-}

module HZX.Circuit.FromDiagram
  ( diagramToCircuit
  , diagramToCircuitWith
  , diagramToCircuitResult
  , diagramToCircuitResultWith
  ) where

import HZX.Core.Diagram (Diagram)
import HZX.Circuit (Circuit(..))
import HZX.Circuit.Extraction
  ( extractCircuitWith
  , ExtractionConfig
  , defaultExtractionConfig
  )
import HZX.Circuit.Extraction.Types (ExtractionResult(..))

-- | Extract a quantum circuit from a ZX diagram.
--
--   This is the Stage-2 graph-theoretic extractor.  It converts the diagram
--   to graph-like form and then extracts gates using the default
--   frontier-based algorithm.  If extraction fails, an empty circuit is
--   returned as a safe fallback.
diagramToCircuit :: Diagram -> Circuit
diagramToCircuit = diagramToCircuitWith defaultExtractionConfig

-- | Like 'diagramToCircuit', but allows the caller to choose the extraction
--   mode and other hyper-parameters.
diagramToCircuitWith :: ExtractionConfig -> Diagram -> Circuit
diagramToCircuitWith cfg d =
  case diagramToCircuitResultWith cfg d of
    Extracted circ _ -> circ
    _                -> Circuit []

-- | Like 'diagramToCircuit', but preserves the full extraction result so
--   callers can distinguish success from "not extractable" and unexpected
--   errors.
diagramToCircuitResult :: Diagram -> ExtractionResult
diagramToCircuitResult = diagramToCircuitResultWith defaultExtractionConfig

-- | Generalised version of 'diagramToCircuitResult' with an explicit
--   extraction configuration.
diagramToCircuitResultWith :: ExtractionConfig -> Diagram -> ExtractionResult
diagramToCircuitResultWith = extractCircuitWith
