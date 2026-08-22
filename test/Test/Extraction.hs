{-# LANGUAGE ScopedTypeVariables #-}

module Test.Extraction (extractionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))

import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Circuit.FromDiagram
import HZX.Circuit.Extraction
  ( ExtractionConfig(..)
  , ExtractionMode(..)
  , defaultExtractionConfig
  )
import HZX.Circuit.Extraction.Types (ExtractionResult(..))
import HZX.IO.QASM
import HZX.Rewrite.Strategy

extractionTests :: TestTree
extractionTests = testGroup "Extraction modes"
  [ modeSmokeTests
  , modeIdentityTests
  , gflowSpecificTests
  ]

frontierCfg :: ExtractionConfig
frontierCfg = defaultExtractionConfig { ecMode = FrontierMode }

gflowCfg :: ExtractionConfig
gflowCfg = defaultExtractionConfig { ecMode = GFlowMode }

-- | Helper: extract a circuit from a diagram using the given mode and
--   simplification strategy.
extractWith :: ExtractionConfig -> Strategy -> Circuit -> ExtractionResult
extractWith cfg strat circ = diagramToCircuitResultWith cfg (strat (circuitToDiagram circ))

modeSmokeTests :: TestTree
modeSmokeTests = testGroup "Mode smoke tests"
  [ testCase "Frontier mode reports no gflow" $
      case extractWith frontierCfg basicSimp smallCliffordT of
        Extracted _ Nothing -> return ()
        other               -> assertFailure $ "expected Extracted with no gflow, got " ++ show other

  , testCase "GFlow mode reports a gflow" $
      case extractWith gflowCfg basicSimp smallCliffordT of
        Extracted _ (Just _) -> return ()
        other                -> assertFailure $ "expected Extracted with gflow, got " ++ show other

  , testCase "Unknown mode string rejected at CLI level" $
      -- This is a pure parse test; the CLI itself is not invoked.
      case parseExtractionMode "quantum" of
        Left _  -> return ()
        Right _ -> assertFailure "expected parse failure for bogus mode"
  ]
  where
    parseExtractionMode s =
      case map (\c -> if c == 'A' then 'a' else c) s of  -- crude lower-case
        "frontier" -> Right FrontierMode
        "gflow"    -> Right GFlowMode
        _          -> Left "unknown"

modeIdentityTests :: TestTree
modeIdentityTests = testGroup "Both modes agree on simple identities"
  [ identityCase "HH = I"       basicSimp (Circuit [H 0, H 0])                  (Circuit [])
  , identityCase "CNOT^2 = I"   cliffordSimp (Circuit [CNOT 0 1, CNOT 0 1])    (Circuit [])
  , identityCase "SWAP^2 = I"   cliffordSimp (Circuit [SWAP 0 1, SWAP 0 1])    (Circuit [])
  , identityCase "SS = Z"       basicSimp (Circuit [S 0, S 0])                  (Circuit [ZPhase 0 (1 % 1)])
  , identityCase "Tdg extracts" basicSimp (Circuit [ZPhase 0 (-1 % 4)])         (Circuit [ZPhase 0 (-1 % 4)])
  ]
  where
    identityCase name strat input expected =
      testCase name $ do
        let frontierResult = extractWith frontierCfg strat input
            gflowResult    = extractWith gflowCfg    strat input
        case (frontierResult, gflowResult) of
          (Extracted cf _, Extracted cg _) -> do
            cf @?= expected
            cg @?= expected
          (otherF, otherG) ->
            assertFailure $ "expected both modes to succeed; frontier=" ++ show otherF
                         ++ ", gflow=" ++ show otherG

gflowSpecificTests :: TestTree
gflowSpecificTests = testGroup "GFlow mode specific"
  [ testCase "GFlow mode extracts a small Clifford+T circuit" $
      case extractWith gflowCfg basicSimp smallCliffordT of
        Extracted _ (Just _) -> return ()
        other                -> assertFailure $ "expected GFlow success, got " ++ show other

  , testCase "GFlow mode extracts a Bell state preparation" $
      case extractWith gflowCfg cliffordSimp bellPrep of
        Extracted _ (Just _) -> return ()
        other                -> assertFailure $ "expected GFlow success, got " ++ show other
  ]

smallCliffordT :: Circuit
smallCliffordT = Circuit [H 0, CNOT 0 1, T 1, CNOT 1 2, H 2]

bellPrep :: Circuit
bellPrep = Circuit [H 0, CNOT 0 1]
