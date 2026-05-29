module Test.Core (coreTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Complex (Complex(..), magnitude)
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase
import HZX.Core.Scalar
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.IO.QASM

coreTests :: TestTree
coreTests = testGroup "Core Tests"
  [ diagramTests
  , phaseTests
  , scalarTests
  , qasmTests
  ]

-- | Test Diagram construction and basic operations
diagramTests :: TestTree
diagramTests = testGroup "Diagram"
  [ testCase "Empty diagram has 0 vertices" $ do
      numVertices empty @?= 0
      numEdges empty @?= 0
      scalar empty @?= scalarOne
  
  , testCase "Add vertex increases count" $ do
      let (v, d) = allocVertex (Z 0) empty
      numVertices d @?= 1
      vertexType d v @?= Just (Z 0)
  
  , testCase "Add edge creates connection" $ do
      let (v1, d1) = allocVertex (Z 0) empty
          (v2, d2) = allocVertex (X 0) d1
          d3 = addEdge v1 v2 Simple d2
      numEdges d3 @?= 1
      neighbors v1 d3 @?= [v2]
      neighbors v2 d3 @?= [v1]
  
  , testCase "Identity diagram has correct structure" $ do
      let d = identityDiagram 3
      length (inputs d) @?= 3
      length (outputs d) @?= 3
      numVertices d @?= 6  -- 3 inputs + 3 outputs
      numEdges d @?= 3     -- 3 wires
      isWellFormed d @?= True
  
  , testCase "Well-formedness detects issues" $ do
      let (v1, d1) = allocVertex (Boundary Rough) empty
          d2 = d1 { _inputs = [v1] }  -- Boundary with no edge
      isWellFormed d2 @?= False
  ]

-- | Test phase arithmetic
phaseTests :: TestTree
phaseTests = testGroup "Phase"
  [ testCase "Phase addition" $ do
      addPhase (1 % 2) (1 % 4) @?= (3 % 4)
      addPhase (1 % 1) (1 % 1) @?= (2 % 1)
  
  , testCase "Phase negation" $ do
      negatePhase (1 % 2) @?= (-1 % 2)
      negatePhase 0 @?= 0
  
  , testCase "Phase to string" $ do
      phaseToString (1 % 2) @?= "1/2π"
      phaseToString 0 @?= "0"
      phaseToString (3 % 1) @?= "3π"
  ]

-- | Test scalar operations
scalarTests :: TestTree
scalarTests = testGroup "Scalar"
  [ testCase "Scalar multiplication" $ do
      let s1 = sqrt2Pow 2  -- (√2)^2 = 2
          s2 = sqrt2Pow 1  -- √2
          result = mulScalar s1 s2
      sqrt2Power (result :: Scalar) @?= 3
  
  , testCase "Phase factor" $ do
      let s = phaseFactor (1 % 2)  -- i
      sqrt2Power (s :: Scalar) @?= 0
      phase (s :: Scalar) @?= (1 % 2 :: Rational)
      phase s @?= (1 % 2 :: Rational)
  
  , testCase "Scalar one is identity" $ do
      mulScalar scalarOne (sqrt2Pow 2) @?= sqrt2Pow 2
  
  , testCase "Scalar to complex" $ do
      let s1 = scalarOne
          c1 = scalarToComplex s1
      c1 @?= (1.0 :+ 0.0)
      
      let s2 = phaseFactor (1 % 1)  -- -1
          c2 = scalarToComplex s2
      magnitude (c2 - ((-1.0) :+ 0.0)) < 0.001 @? "Phase π should be -1"
  ]

-- | Test QASM parsing and serialization
qasmTests :: TestTree
qasmTests = testGroup "QASM"
  [ testCase "Parse simple circuit" $ do
      let qasm = unlines
            [ "OPENQASM 2.0;"
            , "qreg q[2];"
            , "h q[0];"
            , "cx q[0], q[1];"
            ]
      case parseQASM qasm of
        Left err -> assertFailure $ "Parse error: " ++ err
        Right (Circuit gates) -> do
          length gates @?= 2
          head gates @?= H 0
          gates !! 1 @?= CNOT 0 1
  
  , testCase "Serialize and parse round-trip" $ do
      let circ = Circuit [H 0, CNOT 0 1, S 1]
          qasm = serializeQASM circ
      case parseQASM qasm of
        Left err -> assertFailure $ "Round-trip parse error: " ++ err
        Right circ' -> numQubits circ' @?= numQubits circ
  
  , testCase "Parse rz gate with phase" $ do
      let qasm = "OPENQASM 2.0;\nqreg q[1];\nrz(pi/2) q[0];"
      case parseQASM qasm of
        Left err -> assertFailure $ "Parse error: " ++ err
        Right (Circuit gates) -> do
          length gates @?= 1
          head gates @?= ZPhase 0 (1 % 2)
  ]
