module Test.STIM (stimTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))

import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Core.Diagram
import HZX.IO.STIM
import HZX.Rewrite.Strategy

stimTests :: TestTree
stimTests = testGroup "STIM Integration"
  [ parseTests
  , conversionTests
  , diagramTests
  , simplificationTests
  , unsupportedTests
  ]

-- ---------------------------------------------------------------------------
-- Parsing tests
-- ---------------------------------------------------------------------------

parseTests :: TestTree
parseTests = testGroup "Parse"
  [ testCase "Parse H gate" $
      parseSTIM "H 0\n" @?= Right (Circuit [H 0])

  , testCase "Parse S gate" $
      parseSTIM "S 1\n" @?= Right (Circuit [S 1])

  , testCase "Parse CNOT gate" $
      parseSTIM "CNOT 0 1\n" @?= Right (Circuit [CNOT 0 1])

  , testCase "Parse CZ gate" $
      parseSTIM "CZ 2 3\n" @?= Right (Circuit [CZ 2 3])

  , testCase "Parse SWAP gate" $
      parseSTIM "SWAP 0 1\n" @?= Right (Circuit [SWAP 0 1])

  , testCase "Parse Pauli gates" $
      parseSTIM "X 0\nY 1\nZ 2\n" @?=
        Right (Circuit [ XPhase 0 (1 % 1)
                       , XPhase 1 (1 % 2), ZPhase 1 (1 % 1)
                       , ZPhase 2 (1 % 1)
                       ])

  , testCase "Parse measurement" $
      parseSTIM "M 0 1\n" @?=
        Right (Circuit [Measure ZBasis 0, Measure ZBasis 1])

  , testCase "Parse X-basis measurement" $
      parseSTIM "MX 0\n" @?= Right (Circuit [Measure XBasis 0])

  , testCase "Parse reset" $
      parseSTIM "R 0\nRX 1\nRZ 2\n" @?=
        Right (Circuit [ Reset ZBasis 0
                       , Reset XBasis 1
                       , Reset ZBasis 2
                       ])

  , testCase "Parse measure-reset" $
      parseSTIM "MR 0\nMRX 1\n" @?=
        Right (Circuit [ Measure ZBasis 0, Reset ZBasis 0
                       , Measure XBasis 1, Reset XBasis 1
                       ])

  , testCase "Parse TICK annotation" $
      parseSTIM "TICK\nH 0\n" @?= Right (Circuit [H 0])

  , testCase "Parse detector annotation" $
      parseSTIM "DETECTOR(0, 0) rec[-1]\nH 0\n" @?= Right (Circuit [H 0])

  , testCase "Parse noise channel" $
      parseSTIM "DEPOLARIZE1(0.001) 0\nH 0\n" @?= Right (Circuit [H 0])
  ]

-- ---------------------------------------------------------------------------
-- Conversion tests
-- ---------------------------------------------------------------------------

conversionTests :: TestTree
conversionTests = testGroup "Conversion"
  [ testCase "Bell state preparation" $
      parseSTIM "H 0\nCNOT 0 1\n" @?=
        Right (Circuit [H 0, CNOT 0 1])

  , testCase "Reset and measure" $
      parseSTIM "R 0\nH 0\nM 0\n" @?=
        Right (Circuit [Reset ZBasis 0, H 0, Measure ZBasis 0])
  ]

-- ---------------------------------------------------------------------------
-- Diagram generation tests
-- ---------------------------------------------------------------------------

diagramTests :: TestTree
diagramTests = testGroup "Diagram"
  [ testCase "Simple circuit diagram is well-formed" $ do
      let circ = Circuit [H 0, CNOT 0 1]
          d = circuitToDiagram circ
      isWellFormed d @?= True
      length (inputs d) @?= 2
      length (outputs d) @?= 2

  , testCase "Measured qubit has no output boundary" $ do
      case parseSTIM "H 0\nM 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = circuitToDiagram circ
          isWellFormed d @?= True
          length (inputs d) @?= 1
          length (outputs d) @?= 0

  , testCase "Partial measurement keeps other outputs" $ do
      case parseSTIM "H 0\nCNOT 0 1\nM 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = circuitToDiagram circ
          isWellFormed d @?= True
          length (inputs d) @?= 2
          length (outputs d) @?= 1

  , testCase "Reset creates a fresh wire" $ do
      case parseSTIM "R 0\nH 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = circuitToDiagram circ
          isWellFormed d @?= True
          length (inputs d) @?= 1
          length (outputs d) @?= 1

  , testCase "Measure then reset produces output" $ do
      case parseSTIM "M 0\nR 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = circuitToDiagram circ
          isWellFormed d @?= True
          length (inputs d) @?= 1
          length (outputs d) @?= 1
  ]

-- ---------------------------------------------------------------------------
-- Simplification tests
-- ---------------------------------------------------------------------------

simplificationTests :: TestTree
simplificationTests = testGroup "Simplification"
  [ testCase "basicSimp works on STIM-converted diagram" $ do
      case parseSTIM "H 0\nH 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = basicSimp (circuitToDiagram circ)
          isWellFormed d @?= True

  , testCase "cliffordSimp works on STIM-converted diagram" $ do
      case parseSTIM "H 0\nS 0\nH 0\n" of
        Left err -> assertFailure err
        Right circ -> do
          let d = cliffordSimp (circuitToDiagram circ)
          isWellFormed d @?= True
  ]

-- ---------------------------------------------------------------------------
-- Unsupported construct tests
-- ---------------------------------------------------------------------------

unsupportedTests :: TestTree
unsupportedTests = testGroup "Unsupported"
  [ testCase "MPP fails hard" $
      case parseSTIM "MPP X0*X1\n" of
        Left _  -> return ()
        Right _ -> assertFailure "Expected failure for MPP"

  , testCase "MXX fails hard" $
      case parseSTIM "MXX 0 1\n" of
        Left _  -> return ()
        Right _ -> assertFailure "Expected failure for MXX"

  , testCase "Classical control fails hard" $
      case parseSTIM "CNOT rec[-1] 0\n" of
        Left _  -> return ()
        Right _ -> assertFailure "Expected failure for classical control"
  ]
