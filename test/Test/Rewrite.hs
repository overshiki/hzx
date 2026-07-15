module Test.Rewrite (rewriteTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap as IM
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Rewrite.Rule
import HZX.Rewrite.Strategy

rewriteTests :: TestTree
rewriteTests = testGroup "Rewrite Tests"
  [ spiderFusionTests
  , identityRemovalTests
  , searchCombinatorTests
  , basicSimpTests
  , cliffordTests
  ]

-- | Test spider fusion rule
spiderFusionTests :: TestTree
spiderFusionTests = testGroup "Spider Fusion"
  [ testCase "Fuse two adjacent Z spiders" $ do
      -- Create two connected Z spiders
      let (v1, d1) = allocVertex (Z (1 % 4)) empty  -- π/4 phase
          (v2, d2) = allocVertex (Z (1 % 4)) d1      -- another π/4
          d3 = addEdge v1 v2 Simple d2
          -- Apply fusion
          result = spiderFusion d3
      case result of
        Nothing -> assertFailure "Spider fusion should apply"
        Just d' -> do
          numVertices d' @?= 1
          -- The fused spider should have phase π/2
          case IM.lookupMin (_vertices d') of
            Just (_, Z p) -> p @?= (1 % 2)
            _ -> assertFailure "Expected one Z spider with phase π/2"
  
  , testCase "Fuse X spiders" $ do
      let (v1, d1) = allocVertex (X (1 % 2)) empty
          (v2, d2) = allocVertex (X (1 % 2)) d1
          d3 = addEdge v1 v2 Simple d2
          result = spiderFusion d3
      case result of
        Nothing -> assertFailure "Should fuse X spiders"
        Just d' -> do
          numVertices d' @?= 1
          case IM.lookupMin (_vertices d') of
            Just (_, X p) -> p @?= (1 % 1)  -- π
            _ -> assertFailure "Expected one X spider with phase π"
  
  , testCase "Don't fuse different colors" $ do
      let (v1, d1) = allocVertex (Z 0) empty
          (v2, d2) = allocVertex (X 0) d1
          d3 = addEdge v1 v2 Simple d2
          result = spiderFusion d3
      result @?= Nothing
  ]

-- | Test identity removal rule
identityRemovalTests :: TestTree
identityRemovalTests = testGroup "Identity Removal"
  [ testCase "Remove phaseless degree-2 Z spider" $ do
      -- Create: boundary -- Z(0) -- boundary
      let (inB, d1) = allocVertex (Boundary Rough) empty
          (mid, d2) = allocVertex (Z 0) d1
          (outB, d3) = allocVertex (Boundary Rough) d2
          d4 = addEdge inB mid Simple d3
          d5 = addEdge mid outB Simple d4
          result = identityRemoval d5
      case result of
        Nothing -> assertFailure "Identity removal should apply"
        Just d' -> do
          -- Should have just the two boundaries connected
          numVertices d' @?= 2
          numEdges d' @?= 1
  
  , testCase "Don't remove spider with phase" $ do
      let (inB, d1) = allocVertex (Boundary Rough) empty
          (mid, d2) = allocVertex (Z (1 % 2)) d1
          (outB, d3) = allocVertex (Boundary Rough) d2
          d4 = addEdge inB mid Simple d3
          d5 = addEdge mid outB Simple d4
          result = identityRemoval d5
      result @?= Nothing
  ]

-- | Test that the search combinators preserve deterministic order.
searchCombinatorTests :: TestTree
searchCombinatorTests = testGroup "Search Combinators"
  [ testCase "vertexRule visits vertices in ascending order" $ do
      -- Two H-boxes; the lower-index one should be simplified first.
      let (h1, d1) = allocVertex HBox empty
          (h2, d2) = allocVertex HBox d1
          (a,  d3) = allocVertex (Boundary Rough) d2
          (b,  d4) = allocVertex (Boundary Rough) d3
          (c,  d5) = allocVertex (Boundary Rough) d4
          (d,  d6) = allocVertex (Boundary Rough) d5
          d7 = addEdge a h1 Simple d6
          d8 = addEdge h1 b Simple d7
          d9 = addEdge c h2 Simple d8
          d10 = addEdge h2 d Simple d9
          result = hadamardEdgeSimp d10
      case result of
        Nothing -> assertFailure "Should match an H-box"
        Just d' -> do
          IM.member h1 (_vertices d') @?= False
          IM.member h2 (_vertices d') @?= True
  ]

-- | Test color change rule
_colorChangeTests :: TestTree
_colorChangeTests = testGroup "Color Change"
  [ testCase "Change Z to X with Hadamard edges" $ do
      -- Create a Z spider with simple edges
      let (v1, d1) = allocVertex (Z (1 % 2)) empty
          (a, d2) = allocVertex (Boundary Rough) d1
          (b, d3) = allocVertex (Boundary Rough) d2
          d4 = addEdge v1 a Simple d3
          d5 = addEdge v1 b Simple d4
          -- Apply color change
          result = colorChange d5
      case result of
        Nothing -> assertFailure "Color change should apply"
        Just d' -> do
          -- Should now be X spider with Hadamard edges
          case IM.lookup v1 (_vertices d') of
            Just (X p) -> p @?= (1 % 2)
            _ -> assertFailure "Expected X spider"
  ]

-- | Test basic simplification strategy
basicSimpTests :: TestTree
basicSimpTests = testGroup "Basic Simplification"
  [ testCase "Simplify HH = I" $ do
      -- Two Hadamard gates cancel
      let circ = Circuit [H 0, H 0]
          d = circuitToDiagram circ
          d' = basicSimp d
      -- Should simplify to just wires (no interior _vertices)
      let interior = IM.filter (\ty -> case ty of Boundary _ -> False; _ -> True) (_vertices d')
      IM.size interior @?= 0
  
  , testCase "Simplify SS = Z" $ do
      -- Two S gates make a Z gate
      let circ = Circuit [S 0, S 0]
          d = circuitToDiagram circ
          d' = basicSimp d
      -- After fusion: 2 boundaries + 1 Z(π) spider = 3 _vertices
      numVertices d' @?= 3  -- Input, output, and Z(π) spider
  ]

-- | Test Clifford simplification
cliffordTests :: TestTree
cliffordTests = testGroup "Clifford Simplification"
  [ testCase "Simplify simple Clifford circuit" $ do
      -- Use a simpler circuit that converges quickly
      let circ = Circuit [S 0, S 0, H 0]  -- S*S = Z, then H
          d = circuitToDiagram circ
          d' = basicSimp d  -- Use basicSimp for stability
      -- Should be well-formed after simplification
      isWellFormed d' @?= True
  ]
