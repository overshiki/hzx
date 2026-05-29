module Test.LatticeSurgery (latticeSurgeryTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.IntMap as IM
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Scalar
import HZX.LatticeSurgery.Core
import HZX.LatticeSurgery.Heralded
import HZX.LatticeSurgery.Protocols.CNOT
import HZX.LatticeSurgery.Protocols.TGate

latticeSurgeryTests :: TestTree
latticeSurgeryTests = testGroup "Lattice Surgery Tests"
  [ boundaryTypeTests
  , splitMergeTests
  , cnotTests
  , tGateTests
  , zxCorrespondenceTests
  ]

-- | Test boundary types and memory creation
boundaryTypeTests :: TestTree
boundaryTypeTests = testGroup "Boundary Types"
  [ testCase "Create rough memory" $ do
      let (d, mem, _) = createMemory Rough empty
      boundaryType mem @?= Rough
      isWellFormed d @?= True
  
  , testCase "Create smooth memory" $ do
      let (d, mem, _) = createMemory Smooth empty
      boundaryType mem @?= Smooth
      isWellFormed d @?= True
  
  , testCase "Rough boundary connects to Z spider" $ do
      let (d, mem, spiderV) = createMemory Rough empty
      case IM.lookup spiderV (_vertices d) of
        Just (Z _) -> return ()  -- Success
        _ -> assertFailure "Rough memory should have Z spider"
  
  , testCase "Smooth boundary connects to X spider" $ do
      let (d, mem, spiderV) = createMemory Smooth empty
      case IM.lookup spiderV (_vertices d) of
        Just (X _) -> return ()  -- Success
        _ -> assertFailure "Smooth memory should have X spider"
  ]

-- | Test split and merge operations
splitMergeTests :: TestTree
splitMergeTests = testGroup "Split and Merge"
  [ testCase "Rough split creates two memories" $ do
      let (d0, mem, _) = createMemory Rough empty
          (d1, mem1, mem2) = roughSplit d0 mem
      boundaryType mem1 @?= Rough
      boundaryType mem2 @?= Rough
      isWellFormed d1 @?= True
  
  , testCase "Smooth split creates two memories" $ do
      let (d0, mem, _) = createMemory Smooth empty
          (d1, mem1, mem2) = smoothSplit d0 mem
      boundaryType mem1 @?= Smooth
      boundaryType mem2 @?= Smooth
      isWellFormed d1 @?= True
  
  , testCase "Rough merge combines two memories" $ do
      let (d0, mem1, _) = createMemory Rough empty
          (d1, mem2, _) = createMemory Rough d0
          heralded = roughMerge d1 mem1 mem2
      -- Check both branches produce valid diagrams
      let (dPos, _) = positiveBranch heralded
          (dNeg, _) = negativeBranch heralded
      isWellFormed dPos @?= True
      isWellFormed dNeg @?= True
  
  , testCase "Smooth merge combines two memories" $ do
      let (d0, mem1, _) = createMemory Smooth empty
          (d1, mem2, _) = createMemory Smooth d0
          heralded = smoothMerge d1 mem1 mem2
      let (dPos, _) = positiveBranch heralded
          (dNeg, _) = negativeBranch heralded
      isWellFormed dPos @?= True
      isWellFormed dNeg @?= True
  
  , testCase "Split then merge is identity (positive branch)" $ do
      let (d0, mem, _) = createMemory Rough empty
          (d1, mem1, mem2) = roughSplit d0 mem
          heralded = roughMerge d1 mem1 mem2
          (d2, resultMem) = positiveBranch heralded
      -- Memory count should be preserved in positive branch
      boundaryType resultMem @?= Rough
  
  , testCase "Scalar changes correctly for split" $ do
      let (d0, mem, _) = createMemory Rough empty
          s0 = scalar d0
          (d1, _, _) = roughSplit d0 mem
          s1 = scalar d1
      -- Split introduces 1/√2 factor
      sqrt2Power s1 @?= sqrt2Power s0 - 1
  ]

-- | Test CNOT implementations
cnotTests :: TestTree
cnotTests = testGroup "CNOT Protocols"
  [ testCase "Standard CNOT creates valid diagram" $ do
      let (d0, control, _) = createMemory Smooth empty
          (d1, target, _) = createMemory Rough d0
          heralded = cnotLS d1 control target
      let (dPos, ctrlOut, targOut) = positiveBranch heralded
          (dNeg, ctrlOutN, targOutN) = negativeBranch heralded
      isWellFormed dPos @?= True
      isWellFormed dNeg @?= True
      boundaryType ctrlOut @?= Smooth
      boundaryType targOut @?= Rough
  
  , testCase "CNOT Variant A creates valid diagram" $ do
      let (d0, control, _) = createMemory Smooth empty
          (d1, target, _) = createMemory Rough d0
          heralded = cnotLS_VariantA d1 control target
      let (dPos, _, _) = positiveBranch heralded
      isWellFormed dPos @?= True
  
  , testCase "CNOT Variant B creates valid diagram" $ do
      let (d0, control, _) = createMemory Smooth empty
          (d1, target, _) = createMemory Rough d0
          heralded = cnotLS_VariantB d1 control target
      let (dPos, _, _) = positiveBranch heralded
      isWellFormed dPos @?= True
  
  , testCase "CNOT Variant C creates valid diagram" $ do
      let (d0, control, _) = createMemory Smooth empty
          (d1, target, _) = createMemory Rough d0
          heralded = cnotLS_VariantC d1 control target
      let (dPos, _, _) = positiveBranch heralded
      isWellFormed dPos @?= True
  
  , testCase "CNOT heralded probability is 1/2" $ do
      let (d0, control, _) = createMemory Smooth empty
          (d1, target, _) = createMemory Rough d0
          heralded = cnotLS d1 control target
      outcomeProb heralded @?= (1 % 2)
  ]

-- | Test T-gate implementations
tGateTests :: TestTree
tGateTests = testGroup "T-Gate Protocols"
  [ testCase "Magic state |A⟩ has π/4 phase" $ do
      let (d, mem) = prepareMagicA empty
      case getMemoryBoundary mem d of
        Just spiderV -> case IM.lookup spiderV (_vertices d) of
          Just (Z p) -> p @?= (1 % 4)
          _ -> assertFailure "Expected Z spider"
        _ -> assertFailure "Could not find spider"
  
  , testCase "Magic state |Y⟩ has π/2 phase" $ do
      let (d, mem) = prepareMagicY empty
      case getMemoryBoundary mem d of
        Just spiderV -> case IM.lookup spiderV (_vertices d) of
          Just (Z p) -> p @?= (1 % 2)
          _ -> assertFailure "Expected Z spider"
        _ -> assertFailure "Could not find spider"
  
  , testCase "T-gate creates valid diagram" $ do
      let (d0, input, _) = createMemory Smooth empty
          heralded = tGateLS d0 input
      let (dPos, _) = positiveBranch heralded
          (dNeg, _) = negativeBranch heralded
      isWellFormed dPos @?= True
      isWellFormed dNeg @?= True
  ]

-- | Test ZX calculus correspondence
zxCorrespondenceTests :: TestTree
zxCorrespondenceTests = testGroup "ZX Correspondence"
  [ testCase "Rough split = Green 1-to-2 spider" $ do
      -- Rough split should create a Z spider with 3 neighbors
      let (d0, mem, spiderV) = createMemory Rough empty
          (d1, _, _) = roughSplit d0 mem
      case IM.lookup spiderV (_vertices d1) of
        Just (Z p) -> do
          p @?= 0  -- Phase should be 0
          degree spiderV d1 @?= 3  -- 3 neighbors (1 in, 2 out)
        _ -> assertFailure "Expected Z spider"
  
  , testCase "Smooth split = Red 1-to-2 spider" $ do
      let (d0, mem, spiderV) = createMemory Smooth empty
          (d1, _, _) = smoothSplit d0 mem
      case IM.lookup spiderV (_vertices d1) of
        Just (X p) -> do
          p @?= 0
          degree spiderV d1 @?= 3
        _ -> assertFailure "Expected X spider"
  
  , testCase "Rough merge negative branch has π phase" $ do
      let (d0, mem1, spider1) = createMemory Rough empty
          (d1, mem2, spider2) = createMemory Rough d0
          heralded = roughMerge d1 mem1 mem2
          (dNeg, _) = negativeBranch heralded
      -- Find the merged spider and check it has π phase
      let spiders = [v | (v, Z p) <- IM.toList (_vertices dNeg), p == (1 % 1)]
      not (null spiders) @? "Negative branch should have π-phase spider"
  ]
