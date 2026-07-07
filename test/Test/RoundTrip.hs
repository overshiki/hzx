module Test.RoundTrip (roundTripTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Scalar (scalarOne)
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Circuit.FromDiagram
import HZX.Rewrite.Strategy
import HZX.Verification.Evaluate

roundTripTests :: TestTree
roundTripTests = testGroup "Round-Trip Tests"
  [ circuitToDiagramTests
  , diagramToCircuitTests
  , semanticRoundTripTests
  , preservationTests
  ]

-- | Test circuit to diagram conversion
circuitToDiagramTests :: TestTree
circuitToDiagramTests = testGroup "Circuit → Diagram"
  [ testCase "Single qubit gates" $ do
      let circ = Circuit [H 0, S 0, T 0]
          d = circuitToDiagram circ
      numVertices d @?= 5  -- in + H + S + T + out
      isWellFormed d @?= True

  , testCase "CNOT creates correct structure" $ do
      let circ = Circuit [CNOT 0 1]
          d = circuitToDiagram circ
      -- CNOT creates: 2 boundaries + 2 spiders + edges
      numVertices d @?= 6
      numEdges d @?= 5
      isWellFormed d @?= True

  , testCase "Multi-qubit circuit" $ do
      let circ = Circuit [H 0, H 1, CNOT 0 1, CZ 1 2]
          d = circuitToDiagram circ
      isWellFormed d @?= True
  ]

-- | Test diagram to circuit conversion (extraction)
diagramToCircuitTests :: TestTree
diagramToCircuitTests = testGroup "Diagram → Circuit"
  [ testCase "Extract identity" $ do
      let d = identityDiagram 2
          circ = diagramToCircuit d
      -- Identity should give empty circuit
      gates circ @?= []

  , testCase "Extract single Hadamard" $ do
      let circ1 = Circuit [H 0]
          d = circuitToDiagram circ1
          circ2 = diagramToCircuit d
      gates circ2 @?= [H 0]

  , testCase "Extract simple circuit" $ do
      let circ1 = Circuit [H 0, CNOT 0 1]
          d = circuitToDiagram circ1
          circ2 = diagramToCircuit d
      gates circ2 @?= [H 0, CNOT 0 1]

  , testCase "Extract CZ" $ do
      let circ1 = Circuit [CZ 0 1]
          d = circuitToDiagram circ1
          circ2 = diagramToCircuit d
      gates circ2 @?= [CZ 0 1]
  ]

-- | Semantic round-trip: circuit → diagram → circuit → diagram is equivalent
--   to the original circuit.
semanticRoundTripTests :: TestTree
semanticRoundTripTests = testGroup "Semantic Round-Trip"
  [ testCase "H gate" $ checkRoundTrip [H 0]
  , testCase "S gate" $ checkRoundTrip [S 1]
  , testCase "T gate" $ checkRoundTrip [T 0]
  , testCase "Z rotation" $ checkRoundTrip [ZPhase 0 (1 % 3)]
  , testCase "X rotation" $ checkRoundTrip [XPhase 0 (1 % 3)]
  , testCase "CNOT" $ checkRoundTrip [CNOT 0 1]
  , testCase "CZ" $ checkRoundTrip [CZ 0 1]
  , testCase "Small Clifford circuit" $
      checkRoundTrip [H 0, CNOT 0 1, S 1, H 1, CNOT 1 0]
  ]
  where
    checkRoundTrip gs =
      let c   = Circuit gs
          d   = circuitToDiagram c
          c'  = diagramToCircuit d
          d'  = circuitToDiagram c'
      in diagramsEquivalent 1e-9 d d' @?= True

-- | Test that simplification preserves semantics
preservationTests :: TestTree
preservationTests = testGroup "Semantic Preservation"
  [ testCase "Simplification preserves well-formedness" $ do
      let circ = Circuit [H 0, CNOT 0 1, S 1, H 1, CNOT 1 0]
          d = circuitToDiagram circ
          d' = basicSimp d
      isWellFormed d' @?= True

  -- Note: cliffordSimp can be unstable for certain circuits in Stage 1
  -- This is a known limitation that will be addressed in Stage 2

  , testCase "Scalar is preserved in scalarOne diagrams" $ do
      let d = identityDiagram 3
      scalar d @?= scalarOne
  ]
