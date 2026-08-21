{-# LANGUAGE TupleSections #-}

module HZX.Circuit.FromDiagram
  ( diagramToCircuit
  ) where

import HZX.Core.Diagram (Diagram)
import HZX.Circuit (Circuit(..))
import HZX.Circuit.Extraction (extractCircuit)

-- | Extract a quantum circuit from a ZX diagram.
--
--   This is the Stage-2 graph-theoretic extractor.  It converts the diagram
--   to graph-like form and then extracts gates using a frontier-based
--   algorithm with GF(2) Gaussian elimination for CNOTs.  If extraction
--   fails, an empty circuit is returned as a safe fallback.
diagramToCircuit :: Diagram -> Circuit
diagramToCircuit d =
  case extractCircuit d of
    Left _    -> Circuit []
    Right circ -> circ
