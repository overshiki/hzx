module Test.Verification (verificationTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Complex (Complex(..), magnitude)

import HZX.Core.Diagram
import HZX.Core.Scalar (sqrt2Pow)
import HZX.Circuit (Circuit(..), Gate(..))
import HZX.Circuit.ToDiagram (circuitToDiagram)
import HZX.Rewrite.Strategy (basicSimp, cliffordSimp)
import HZX.Verification.Evaluate

verificationTests :: TestTree
verificationTests = testGroup "Verification"
  [ testCase "H then H is identity" $
      let c = Circuit [H 0, H 0]
          d = circuitToDiagram c
      in diagramsEquivalent 1e-9 d (mkIdentityDiagram 1) @?= True

  , testCase "CNOT original evaluates" $
      let c = Circuit [CNOT 0 1]
          d = circuitToDiagram c
      in case evalDiagram d of
           Left err -> assertFailure err
           Right _  -> return ()

  , testCase "CNOT then CNOT is identity" $
      let c = Circuit [CNOT 0 1, CNOT 0 1]
          d = circuitToDiagram c
      in diagramsEquivalent 1e-9 d (mkIdentityDiagram 2) @?= True

  , testCase "S then S is Z" $
      let c1 = Circuit [S 0, S 0]
          c2 = Circuit [ZPhase 0 1]
      in diagramsEquivalent 1e-9 (circuitToDiagram c1) (circuitToDiagram c2) @?= True

  , testCase "basicSimp preserves semantics" $
      let c = Circuit [H 0, CNOT 0 1, S 1, H 1, CNOT 1 0]
          d = circuitToDiagram c
          d' = basicSimp d
      in diagramsEquivalent 1e-9 d d' @?= True

  , testCase "cliffordSimp preserves semantics" $
      let c = Circuit [H 0, CNOT 0 1, S 0, H 0, CNOT 0 1]
          d = circuitToDiagram c
          d' = cliffordSimp d
      in diagramsEquivalent 1e-9 d d' @?= True

  , testCase "Scalar field scales evaluation" $
      let d0 = mkIdentityDiagram 1
          d1 = mapScalar (const (sqrt2Pow 1)) d0
      in case (evalDiagram d0, evalDiagram d1) of
           (Right m0, Right m1) ->
             let eye = [[1, 0], [0, 1]] :: [[Complex Double]]
                 expected = map (map (sqrt 2 *)) eye
             in all (all (\(a, b) -> magnitude (a - b) < 1e-9)) (zipWith zip m1 expected) @?
                "Expected sqrt(2) * identity"
           _ -> assertFailure "evalDiagram failed"
  ]

mkIdentityDiagram :: Int -> Diagram
mkIdentityDiagram n =
  let (inVs, d0) = allocVertices (replicate n (Boundary Rough)) empty
      (outVs, d1) = allocVertices (replicate n (Boundary Rough)) d0
      d2 = d1 { _inputs = inVs, _outputs = outVs }
  in foldl (\d (i, o) -> addEdge i o Simple d) d2 (zip inVs outVs)
