module Test.Doubled (doubledTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))
import qualified Data.Map as M

import HZX.Core.Diagram.Types (BoundaryType(..))
import HZX.Core.Diagram.Parametric.Types (ParamPhase(..))
import HZX.Core.Diagram.Doubled.Types
import HZX.Core.Diagram.Doubled.Instances
import HZX.Core.Diagram.Doubled.Rewrite
import HZX.Core.Diagram.Doubled.Strategy
import HZX.Core.Diagram.Doubled.Components
import HZX.Circuit (Gate(..), MeasurementBasis(..))
import HZX.Circuit.Parametric (ParametricCircuit(..), ParametricOp(..), NoiseChannel(..))
import HZX.Circuit.ToDoubledDiagram (circuitToDoubledDiagram)
import HZX.IO.STIM.Doubled

doubledTests :: TestTree
doubledTests = testGroup "Doubled ZX"
  [ constructionTests
  , conversionTests
  , circuitConversionTests
  , classicalRuleTests
  , componentTests
  , rewriteTests
  ]

-- ---------------------------------------------------------------------------
-- Construction tests
-- ---------------------------------------------------------------------------

constructionTests :: TestTree
constructionTests = testGroup "Construction"
  [ testCase "Empty doubled diagram" $ do
      dNumVertices dEmpty @?= 0
      dIsWellFormed dEmpty @?= True

  , testCase "Identity diagram is well-formed" $ do
      let (inVs, d0) = dAllocVertices (replicate 2 (DBoundary Rough)) dEmpty
          (outVs, d1) = dAllocVertices (replicate 2 (DBoundary Rough)) d0
          d2 = d1 { _dInputs = inVs, _dOutputs = outVs }
          d3 = foldl (\d (i,o) -> dAddEdge i o (DSimple Quantum) d) d2 (zip inVs outVs)
      dIsWellFormed d3 @?= True
      dNumVertices d3 @?= 4

  , testCase "Measurement spider connects quantum and classical wires" $ do
      let (inV, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (m, d1) = dAllocVertex DMeasureZ d0
          (outV, d2) = dAllocVertex (DBoundary Rough) d1
          d3 = dAddEdge inV m (DSimple Quantum) d2
          d4 = dAddEdge m outV (DSimple Classical) d3
      dIsWellFormed d4 @?= True
  ]

-- ---------------------------------------------------------------------------
-- STIM conversion tests
-- ---------------------------------------------------------------------------

conversionTests :: TestTree
conversionTests = testGroup "STIM Conversion"
  [ testCase "Convert simple measurement circuit" $
      case parseSTIMDoubled "H 0\nM 0\n" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True
          not (null (_dClassicalOutputs d)) @? "Expected classical output"

  , testCase "Convert detector circuit" $
      case parseSTIMDoubled "H 0\nH 1\nM 0 1\nDETECTOR rec[-1] rec[-2]" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True
          length (_dClassicalOutputs d) @?= 1

  , testCase "Convert observable circuit" $
      case parseSTIMDoubled "H 0\nM 0\nOBSERVABLE_INCLUDE(0) rec[-1]" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True
          length (_dObservableOutputs d) @?= 1

  , testCase "Convert noisy circuit" $
      case parseSTIMDoubled "DEPOLARIZE1(0.03) 0\nH 0\nM 0\n" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True

  , testCase "Detector builds XOR tree" $
      case parseSTIMDoubled "H 0\nH 1\nM 0 1\nDETECTOR rec[-1] rec[-2]" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True
          length (_dClassicalOutputs d) @?= 1
          let xorCount = length [ v | v <- dAllVertices d
                                    , Just DXor <- [dLookupVertex v d] ]
          xorCount @?= 1

  , testCase "Observable builds XOR tree" $
      case parseSTIMDoubled "H 0\nM 0\nOBSERVABLE_INCLUDE(0) rec[-1]" of
        Left err -> assertFailure err
        Right d -> do
          dIsWellFormed d @?= True
          length (_dObservableOutputs d) @?= 1
          -- A single measurement needs no XOR spider; it is wired directly.
          let xorCount = length [ v | v <- dAllVertices d
                                    , Just DXor <- [dLookupVertex v d] ]
          xorCount @?= 0

  , testCase "Classical COPY spider is well-formed" $
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (copy, d1) = dAllocVertex DCopy d0
          (b, d2) = dAllocVertex (DBoundary Rough) d1
          (c, d3) = dAllocVertex (DBoundary Rough) d2
          d4 = dAddEdge a copy (DSimple Classical) d3
          d5 = dAddEdge copy b (DSimple Classical) d4
          d6 = dAddEdge copy c (DSimple Classical) d5
      in dIsWellFormed d6 @?= True

  , testCase "Classical XOR spider is well-formed" $
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (b, d1) = dAllocVertex (DBoundary Rough) d0
          (xor, d2) = dAllocVertex DXor d1
          (c, d3) = dAllocVertex (DBoundary Rough) d2
          d4 = dAddEdge a xor (DSimple Classical) d3
          d5 = dAddEdge b xor (DSimple Classical) d4
          d6 = dAddEdge xor c (DSimple Classical) d5
      in dIsWellFormed d6 @?= True
  ]

-- ---------------------------------------------------------------------------
-- Circuit conversion tests
-- ---------------------------------------------------------------------------

circuitConversionTests :: TestTree
circuitConversionTests = testGroup "Circuit Conversion"
  [ testCase "Identity via HH" $ do
      let pc = ParametricCircuit [ POGate (H 0), POGate (H 0) ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      length (_dOutputs d) @?= 1
      length (_dClassicalOutputs d) @?= 0

  , testCase "Hadamard circuit" $ do
      let pc = ParametricCircuit [ POGate (H 0) ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      dNumVertices d @?= 3

  , testCase "CNOT circuit" $ do
      let pc = ParametricCircuit [ POGate (CNOT 0 1) ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      dNumVertices d @?= 6
      dNumEdges d @?= 5

  , testCase "Measurement produces classical output" $ do
      let pc = ParametricCircuit [ POGate (Measure ZBasis 0) ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      length (_dClassicalOutputs d) @?= 1
      length (_dOutputs d) @?= 0

  , testCase "Reset reinitializes a wire" $ do
      let pc = ParametricCircuit
            [ POGate (Measure ZBasis 0)
            , POGate (Reset ZBasis 0)
            ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      length (_dClassicalOutputs d) @?= 1
      length (_dOutputs d) @?= 1

  , testCase "Noise inserts parametric Pauli spiders" $ do
      let pc = ParametricCircuit
            [ PONoise (XError "e0" 0 0.01)
            , POGate (H 0)
            , POGate (Measure ZBasis 0)
            ]
          d = circuitToDoubledDiagram pc
      dIsWellFormed d @?= True
      let xSpiders = [ v | v <- dAllVertices d
                         , Just (DX (ParamPhase _ ps)) <- [dLookupVertex v d]
                         , not (M.null ps) ]
      not (null xSpiders) @? "Expected a parametric X spider"
  ]

-- ---------------------------------------------------------------------------
-- Classical rule tests
-- ---------------------------------------------------------------------------

classicalRuleTests :: TestTree
classicalRuleTests = testGroup "Classical Rules"
  [ testCase "COPY identity removal" $ do
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (copy, d1) = dAllocVertex DCopy d0
          (b, d2) = dAllocVertex (DBoundary Rough) d1
          d3 = dAddEdge a copy (DSimple Classical) d2
          d4 = dAddEdge copy b (DSimple Classical) d3
          d5 = dRunStrategy dCopyIdentity d4
      dNumVertices d5 @?= 2

  , testCase "XOR identity removal" $ do
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (xor, d1) = dAllocVertex DXor d0
          (b, d2) = dAllocVertex (DBoundary Rough) d1
          d3 = dAddEdge a xor (DSimple Classical) d2
          d4 = dAddEdge xor b (DSimple Classical) d3
          d5 = dRunStrategy dXorIdentity d4
      dNumVertices d5 @?= 2

  , testCase "COPY fusion" $ do
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (c1, d1) = dAllocVertex DCopy d0
          (c2, d2) = dAllocVertex DCopy d1
          (b, d3) = dAllocVertex (DBoundary Rough) d2
          d4 = dAddEdge a c1 (DSimple Classical) d3
          d5 = dAddEdge c1 c2 (DSimple Classical) d4
          d6 = dAddEdge c2 b (DSimple Classical) d5
          d7 = dRunStrategy dCopyFusion d6
      dNumVertices d7 @?= 3

  , testCase "Measurement terminal COPY elimination" $ do
      let (qIn, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (m, d1) = dAllocVertex DMeasureZ d0
          (copy, d2) = dAllocVertex DCopy d1
          (o1, d3) = dAllocVertex (DBoundary Rough) d2
          (o2, d4) = dAllocVertex (DBoundary Rough) d3
          d5 = dAddEdge qIn m (DSimple Quantum) d4
          d6 = dAddEdge m copy (DSimple Classical) d5
          d7 = dAddEdge copy o1 (DSimple Classical) d6
          d8 = dAddEdge copy o2 (DSimple Classical) d7
          d9 = d8 { _dClassicalOutputs = [o1, o2] }
          d10 = dRunStrategy dMeasureCopy d9
      dNumVertices d10 @?= 4
      all (\v -> v `elem` dCNeighbors m d10) [o1, o2] @? "Measurement should be wired to both outputs"
  ]

-- ---------------------------------------------------------------------------
-- Component extraction tests
-- ---------------------------------------------------------------------------

componentTests :: TestTree
componentTests = testGroup "Components"
  [ testCase "Detector component is detected" $
      case parseSTIMDoubled "H 0\nH 1\nM 0 1\nDETECTOR rec[-1] rec[-2]" of
        Left err -> assertFailure err
        Right d -> do
          let comps = dClassicalComponents d
          length comps @?= 1
          case comps of
            [c] -> dIsDetectorComponent d c @? "Expected a detector component"
            _   -> assertFailure "Expected exactly one classical component"
  ]

-- ---------------------------------------------------------------------------
-- Rewrite rule tests
-- ---------------------------------------------------------------------------

rewriteTests :: TestTree
rewriteTests = testGroup "Rewrite"
  [ testCase "Doubled spider fusion" $ do
      let (a, d0) = dAllocVertex (DBoundary Rough) dEmpty
          (z1, d1) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d0
          (z2, d2) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d1
          (b, d3) = dAllocVertex (DBoundary Rough) d2
          d4 = dAddEdge a z1 (DSimple Quantum) d3
          d5 = dAddEdge z1 z2 (DSimple Quantum) d4
          d6 = dAddEdge z2 b (DSimple Quantum) d5
          d7 = dBasicSimp d6
      dNumVertices d7 @?= 3  -- boundary + fused spider + boundary

  , testCase "Doubled Hadamard edge simplification" $ do
      let (a, d0) = dAllocVertex (DZ (ParamPhase 0 M.empty)) dEmpty
          (h, d1) = dAllocVertex DHBox d0
          (b, d2) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d1
          d3 = dAddEdge a h (DSimple Quantum) d2
          d4 = dAddEdge h b (DSimple Quantum) d3
          d5 = dBasicSimp d4
      dNumVertices d5 @?= 1
  ]
