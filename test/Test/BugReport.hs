{-# LANGUAGE ScopedTypeVariables #-}

module Test.BugReport (bugReportTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))
import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram
import HZX.Core.Scalar (scalarOne)
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Circuit.FromDiagram
import HZX.Circuit.Extraction (extractCircuit)
import HZX.Circuit.Extraction.Types (ExtractionResult(..))
import HZX.IO.QASM
import HZX.Rewrite.Strategy

bugReportTests :: TestTree
bugReportTests = testGroup "Bug Report Regressions"
  [ qasmParserTests
  , extractionIdentityTests
  , extractionResultTests
  ]

qasmParserTests :: TestTree
qasmParserTests = testGroup "QASM parser"
  [ testCase "Parse tdg" $
      parseQASM "OPENQASM 2.0;\nqreg q[1];\ntdg q[0];"
        @?= Right (Circuit [ZPhase 0 (-1 % 4)])

  , testCase "Parse sdg" $
      parseQASM "OPENQASM 2.0;\nqreg q[1];\nsdg q[0];"
        @?= Right (Circuit [ZPhase 0 (-1 % 2)])

  , testCase "Parse include declaration" $
      parseQASM "OPENQASM 2.0;\ninclude \"qelib1.inc\";\nqreg q[1];\nh q[0];"
        @?= Right (Circuit [H 0])

  , testCase "Parse creg declaration" $
      parseQASM "OPENQASM 2.0;\nqreg q[1];\ncreg c[1];\nh q[0];"
        @?= Right (Circuit [H 0])
  ]

extractionIdentityTests :: TestTree
extractionIdentityTests = testGroup "Extraction identities"
  [ identityCase "HH = I"       (Circuit [H 0, H 0])                  (Circuit [])
  , identityCase "CNOT^2 = I"   (Circuit [CNOT 0 1, CNOT 0 1])        (Circuit [])
  , identityCase "SWAP^2 = I"   (Circuit [SWAP 0 1, SWAP 0 1])        (Circuit [])
  , identityCase "SS = Z"       (Circuit [S 0, S 0])                  (Circuit [ZPhase 0 (1 % 1)])
  , identityCase "Tdg extracts" (Circuit [ZPhase 0 (-1 % 4)])         (Circuit [ZPhase 0 (-1 % 4)])
  ]
  where
    identityCase name input expected =
      testCase name $
        diagramToCircuit (basicSimp (circuitToDiagram input)) @?= expected

extractionResultTests :: TestTree
extractionResultTests = testGroup "Extraction result handling"
  [ testCase "HH extraction reports success" $
      case diagramToCircuitResult (basicSimp (circuitToDiagram (Circuit [H 0, H 0]))) of
        Extracted _ _ -> return ()
        other         -> assertFailure $ "expected Extracted, got " ++ show other

  , testCase "Diagram with mismatched inputs/outputs reports an error" $
      let malformed = Diagram (IM.fromList [(0, Boundary Rough)])
                              M.empty IM.empty [0] [] 1 scalarOne
      in case extractCircuit malformed of
           ExtractionError _ -> return ()
           other             -> assertFailure $ "expected ExtractionError, got " ++ show other
  ]
