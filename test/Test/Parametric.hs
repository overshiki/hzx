module Test.Parametric (parametricTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))
import qualified Data.Map as M
import qualified Data.IntMap as IM

import HZX.Core.Diagram
import HZX.Core.Diagram.Parametric.Types
import HZX.Core.Diagram.Parametric.Instances
import HZX.Core.Diagram.Parametric.Phase
import HZX.Core.Diagram.Parametric.Rewrite
import HZX.Core.Diagram.Parametric.Strategy
import HZX.Circuit (Gate(..), MeasurementBasis(..), Circuit(..))
import HZX.Circuit.Parametric
import HZX.Circuit.ToParametricDiagram
import HZX.IO.STIM.Parametric

parametricTests :: TestTree
parametricTests = testGroup "Parametric ZX"
  [ phaseTests
  , diagramTests
  , rewriteTests
  , stimTests
  , evaluationTests
  ]

-- ---------------------------------------------------------------------------
-- Phase arithmetic tests
-- ---------------------------------------------------------------------------

phaseTests :: TestTree
phaseTests = testGroup "Phase"
  [ testCase "Add constant phases" $
      addParamPhase (ParamPhase (1 % 2) M.empty) (ParamPhase (1 % 4) M.empty)
        @?= ParamPhase (3 % 4) M.empty

  , testCase "Merge parameter terms" $
      addParamPhase (ParamPhase 0 (M.singleton "e0" 1))
                    (ParamPhase 0 (M.singleton "e0" 1))
        @?= ParamPhase 0 (M.singleton "e0" 2)

  , testCase "Detect symbolic zero" $
      isParamZero (ParamPhase 0 M.empty) @?= True

  , testCase "Detect symbolic π" $
      isParamPi (ParamPhase 1 M.empty) @?= True

  , testCase "Parameterized phase is not π" $
      isParamPi (ParamPhase 0 (M.singleton "e0" 1)) @?= False

  , testCase "Evaluate parameterized phase" $
      evalParamPhase (M.singleton "e0" True) (ParamPhase 0 (M.singleton "e0" 1))
        @?= 1

  , testCase "Add phase with multiple parameters" $
      addParamPhase (ParamPhase 0 (M.fromList [("e0", 1), ("e1", 1)]))
                    (ParamPhase (1 % 2) (M.singleton "e0" 1))
        @?= ParamPhase (1 % 2) (M.fromList [("e0", 2), ("e1", 1)])

  , testCase "Detect Pauli phase with parameter" $
      isParamPauli (ParamPhase 0 (M.singleton "e0" 1)) @?= True
  ]

-- ---------------------------------------------------------------------------
-- Diagram construction tests
-- ---------------------------------------------------------------------------

diagramTests :: TestTree
diagramTests = testGroup "Diagram"
  [ testCase "Empty parametric diagram" $ do
      pNumVertices pEmpty @?= 0
      pIsWellFormed pEmpty @?= True

  , testCase "Identity diagram is well-formed" $ do
      let d = pIdentityDiagram 2
      pIsWellFormed d @?= True
      length (_pInputs d) @?= 2
      length (_pOutputs d) @?= 2
      pNumVertices d @?= 4

  , testCase "Allocate parametric spider" $ do
      let (v, d) = pAllocVertex (ParamZ (ParamPhase (1 % 2) M.empty)) pEmpty
      pNumVertices d @?= 1
      pVertexType d v @?= Just (ParamZ (ParamPhase (1 % 2) M.empty))
  ]

-- ---------------------------------------------------------------------------
-- Rewrite rule tests
-- ---------------------------------------------------------------------------

rewriteTests :: TestTree
rewriteTests = testGroup "Rewrite"
  [ testCase "Spider fusion with parameters" $ do
      let d = buildTwoZSpiders (ParamPhase 0 (M.singleton "e0" 1))
                               (ParamPhase 0 (M.singleton "e1" 1))
          d' = pRunStrategy pSpiderFusion d
      pNumVertices d' @?= 2  -- input boundary + fused spider

  , testCase "Identity removal" $ do
      let d = buildPhaselessSpiderChain
          d' = pRunStrategy pIdentityRemoval d
      pNumVertices d' @?= 2

  , testCase "Hadamard edge simplification" $ do
      let d = buildHBoxBetweenTwoSpiders
          d' = pRunStrategy pHadamardEdgeSimp d
      pNumEdges d' @?= 1

  , testCase "Color change" $ do
      let d = buildXSpider
          d' = pRunStrategy pColorChange d
      case pAllVertices d' of
        [v] -> pVertexType d' v @?= Just (ParamZ (ParamPhase 0 M.empty))
        _   -> assertFailure "Expected one vertex after color change"

  , testCase "Basic simplification on Clifford circuit" $ do
      let d = parametricCircuitToDiagram $ ParametricCircuit
            [ POGate (H 0)
            , POGate (CNOT 0 1)
            , POGate (S 1)
            , POGate (H 1)
            , POGate (CNOT 1 0)
            ]
          d' = pCliffordSimp d
      pIsWellFormed d' @?= True

  , testCase "Local complementation on Pauli-parameterized star" $ do
      let d = buildPauliStar
          d' = pRunStrategy pLocalComplementation d
      pIsWellFormed d' @?= True
      pNumVertices d' @?= 3

  , testCase "Pivot on Pauli-parameterized edge" $ do
      let d = buildPauliPivot
          d' = pRunStrategy pPivot d
      pIsWellFormed d' @?= True
      pNumVertices d' @?= 2
  ]

-- ---------------------------------------------------------------------------
-- STIM integration tests
-- ---------------------------------------------------------------------------

stimTests :: TestTree
stimTests = testGroup "STIM"
  [ testCase "Parse DEPOLARIZE1 noise" $
      case parseSTIMParametric "DEPOLARIZE1(0.03) 0\nH 0\nM 0\n" of
        Left err -> assertFailure err
        Right pc -> do
          length (pcOps pc) @?= 3

  , testCase "Noise produces parametric spiders" $
      case parseSTIMParametric "DEPOLARIZE1(0.03) 0\nH 0\nM 0\n" of
        Left err -> assertFailure err
        Right pc -> do
          let d = parametricCircuitToDiagram pc
          pIsWellFormed d @?= True
          let xSpiders = [ v | (v, ParamX (ParamPhase _ ps)) <- IM.toList (_pVertices d)
                             , not (M.null ps) ]
              zSpiders = [ v | (v, ParamZ (ParamPhase _ ps)) <- IM.toList (_pVertices d)
                             , not (M.null ps) ]
          (not (null xSpiders) || not (null zSpiders))
            @? "Expected at least one parametric Pauli spider"
  ]

-- ---------------------------------------------------------------------------
-- Evaluation tests
-- ---------------------------------------------------------------------------

evaluationTests :: TestTree
evaluationTests = testGroup "Evaluation"
  [ testCase "Evaluate noiseless diagram" $ do
      let pc = ParametricCircuit
            [ POGate (H 0)
            , POGate (CNOT 0 1)
            ]
          d = parametricCircuitToDiagram pc
          d0 = evalParametricDiagram M.empty d
          d1 = pCliffordSimp d
          d1_0 = evalParametricDiagram M.empty d1
      isWellFormed d0 @?= True
      isWellFormed d1_0 @?= True

  , testCase "Evaluate with bit flip" $ do
      let pc = ParametricCircuit
            [ PONoise (XError "e0" 0 0.01)
            , POGate (H 0)
            , POGate (Measure ZBasis 0)
            ]
          d = parametricCircuitToDiagram pc
          d0 = evalParametricDiagram (M.singleton "e0" False) d
          d1 = evalParametricDiagram (M.singleton "e0" True) d
      isWellFormed d0 @?= True
      isWellFormed d1 @?= True
      numVertices d0 @?= numVertices d1

  , testCase "Evaluate DEPOLARIZE1 under all assignments" $ do
      let pc = ParametricCircuit
            [ PONoise (Depolarize1 "px" "pz" 0 0.01 0.01 0.01)
            , POGate (H 0)
            , POGate (Measure ZBasis 0)
            ]
          d = parametricCircuitToDiagram pc
          eval px pz = evalParametricDiagram (M.fromList [("px", px), ("pz", pz)]) d
      all isWellFormed [eval px pz | px <- [False, True], pz <- [False, True]] @?= True
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

buildTwoZSpiders :: ParamPhase -> ParamPhase -> ParamDiagram
buildTwoZSpiders p1 p2 =
  let (inV, d0) = pAllocVertex (ParamBoundary Rough) pEmpty
      (v1, d1) = pAllocVertex (ParamZ p1) d0
      (v2, d2) = pAllocVertex (ParamZ p2) d1
      d3 = pAddEdge inV v1 Simple d2
      d4 = pAddEdge v1 v2 Simple d3
  in d4

buildPhaselessSpiderChain :: ParamDiagram
buildPhaselessSpiderChain =
  let (a, d0) = pAllocVertex (ParamBoundary Rough) pEmpty
      (z, d1) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d0
      (b, d2) = pAllocVertex (ParamBoundary Rough) d1
      d3 = pAddEdge a z Simple d2
      d4 = pAddEdge z b Simple d3
  in d4

buildHBoxBetweenTwoSpiders :: ParamDiagram
buildHBoxBetweenTwoSpiders =
  let (a, d0) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) pEmpty
      (h, d1) = pAllocVertex ParamHBox d0
      (b, d2) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d1
      d3 = pAddEdge a h Simple d2
      d4 = pAddEdge h b Simple d3
  in d4

buildXSpider :: ParamDiagram
buildXSpider =
  let (v, d) = pAllocVertex (ParamX (ParamPhase 0 M.empty)) pEmpty
  in d { _pInputs = [v], _pOutputs = [v] }

buildPauliStar :: ParamDiagram
buildPauliStar =
  let (c, d0) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) pEmpty
      (l1, d1) = pAllocVertex (ParamZ (ParamPhase 0 (M.singleton "e1" 1))) d0
      (l2, d2) = pAllocVertex (ParamZ (ParamPhase 0 (M.singleton "e2" 1))) d1
      (l3, d3) = pAllocVertex (ParamZ (ParamPhase 0 (M.singleton "e3" 1))) d2
      d4 = pAddEdge c l1 Hadamard d3
      d5 = pAddEdge c l2 Hadamard d4
      d6 = pAddEdge c l3 Hadamard d5
  in d6

buildPauliPivot :: ParamDiagram
buildPauliPivot =
  let (u, d0) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) pEmpty
      (v, d1) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d0
      (a, d2) = pAllocVertex (ParamZ (ParamPhase 0 (M.singleton "ea" 1))) d1
      (b, d3) = pAllocVertex (ParamZ (ParamPhase 0 (M.singleton "eb" 1))) d2
      d4 = pAddEdge u v Hadamard d3
      d5 = pAddEdge u a Simple d4
      d6 = pAddEdge v b Simple d5
  in d6
