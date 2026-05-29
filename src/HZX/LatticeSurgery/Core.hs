{-# LANGUAGE TupleSections #-}

module HZX.LatticeSurgery.Core
  ( -- * Types
    Memory(..)
  , LSOperation(..)
  , SplitType(..)
  , MergeType(..)
    -- * Primitive Operations
  , roughSplit
  , smoothSplit
  , roughMerge
  , smoothMerge
    -- * Memory Management
  , createMemory
  , getMemoryBoundary
  ) where

import qualified Data.IntMap as IM
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase (addPhase)
import HZX.Core.Scalar (sqrt2Pow, mulScalar)
import HZX.LatticeSurgery.Heralded
import HZX.LatticeSurgery.PauliFrame

-- | A logical qubit memory in lattice surgery.
data Memory = Memory
  { memoryId :: Int
  , boundaryType :: BoundaryType
  , inputBoundary :: Vertex
  , outputBoundary :: Vertex
  } deriving (Eq, Show)

-- | Types of split operations.
data SplitType = RoughSplit | SmoothSplit
  deriving (Eq, Show)

-- | Types of merge operations.
data MergeType = RoughMerge | SmoothMerge
  deriving (Eq, Show)

-- | Lattice surgery operations for protocol construction.
data LSOperation
  = Split SplitType Memory (Memory, Memory)
  | Merge MergeType (Memory, Memory) Memory Outcome
  deriving (Eq, Show)

-- | Create a new logical qubit memory.
-- For rough boundary: Z-type logical qubit (|0>, |1> basis)
-- For smooth boundary: X-type logical qubit (|+>, |-> basis)
createMemory :: BoundaryType -> Diagram -> (Diagram, Memory, Vertex)
createMemory bt d =
  let memId = _nextId d  -- Use next ID as memory identifier
      (inV, d1) = allocVertex (Boundary bt) d
      (outV, d2) = allocVertex (Boundary bt) d1
      -- Connect input to output through the appropriate spider type
      spiderType = case bt of
        Rough -> Z 0   -- Green spider (Z basis)
        Smooth -> X 0  -- Red spider (X basis)
      (spiderV, d3) = allocVertex spiderType d2
      d4 = addEdge inV spiderV Simple d3
      d5 = addEdge spiderV outV Simple d4
      mem = Memory memId bt inV outV
  in (d5, mem, spiderV)

-- | Get the boundary vertex of a memory.
getMemoryBoundary :: Memory -> Diagram -> Maybe Vertex
getMemoryBoundary mem d =
  -- Find the spider connected to the output boundary
  case neighbors (outputBoundary mem) d of
    [v] -> Just v
    _ -> Nothing

-- | Perform a rough split on a Z-type memory.
-- Corresponds to green 1-to-2 spider with α=0 in ZX calculus.
-- Copies X_L and distributes Z_L as Z_L^(1) ⊗ Z_L^(2).
roughSplit :: Diagram -> Memory -> (Diagram, Memory, Memory)
roughSplit d mem
  | boundaryType mem /= Rough = error "roughSplit requires Rough boundary memory"
  | otherwise =
      let -- Find the existing spider
          Just spiderV = getMemoryBoundary mem d
          -- Create two new output boundaries
          (outV1, d1) = allocVertex (Boundary Rough) d
          (outV2, d2) = allocVertex (Boundary Rough) d1
          -- Remove old output boundary connection
          d3 = removeEdge spiderV (outputBoundary mem) Simple d2
          -- Connect spider to new boundaries
          d4 = addEdge spiderV outV1 Simple d3
          d5 = addEdge spiderV outV2 Simple d4
          -- Create new memories
          mem1 = Memory (memoryId mem * 2) Rough (inputBoundary mem) outV1
          mem2 = Memory (memoryId mem * 2 + 1) Rough (inputBoundary mem) outV2
          -- Update scalar: split introduces factor of 1/√2
          d6 = mapScalar (`mulScalar` sqrt2Pow (-1)) d5
      in (d6, mem1, mem2)

-- | Perform a smooth split on an X-type memory.
-- Corresponds to red 1-to-2 spider with α=0 in ZX calculus.
-- Copies Z_L and distributes X_L as X_L^(1) ⊗ X_L^(2).
smoothSplit :: Diagram -> Memory -> (Diagram, Memory, Memory)
smoothSplit d mem
  | boundaryType mem /= Smooth = error "smoothSplit requires Smooth boundary memory"
  | otherwise =
      let -- Find the existing spider
          Just spiderV = getMemoryBoundary mem d
          -- Create two new output boundaries
          (outV1, d1) = allocVertex (Boundary Smooth) d
          (outV2, d2) = allocVertex (Boundary Smooth) d1
          -- Remove old output boundary connection
          d3 = removeEdge spiderV (outputBoundary mem) Simple d2
          -- Connect spider to new boundaries
          d4 = addEdge spiderV outV1 Simple d3
          d5 = addEdge spiderV outV2 Simple d4
          -- Create new memories
          mem1 = Memory (memoryId mem * 2) Smooth (inputBoundary mem) outV1
          mem2 = Memory (memoryId mem * 2 + 1) Smooth (inputBoundary mem) outV2
          -- Update scalar: split introduces factor of 1/√2
          d6 = mapScalar (`mulScalar` sqrt2Pow (-1)) d5
      in (d6, mem1, mem2)

-- | Perform a rough merge on two memories.
-- Corresponds to green 2-to-1 spider with α=0 in ZX calculus.
-- Measures X_L^(1) ⊗ X_L^(2), produces Z_L^(3) = Z_L^(1) ⊗ Z_L^(2).
-- Non-deterministic: +1 outcome (positive branch) or -1 outcome (negative branch).
-- 
-- Note: Inputs can be mixed boundary types. The output is always Rough (Z-type).
roughMerge :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory)
roughMerge d mem1 mem2 =
  let -- Find the existing spiders
      Just spiderV1 = getMemoryBoundary mem1 d
      Just spiderV2 = getMemoryBoundary mem2 d
      -- Create new merged output boundary
      (outV, d1) = allocVertex (Boundary Rough) d
      -- Create merged spider (always Z/green for rough merge)
      (mergedSpider, d2) = allocVertex (Z 0) d1
      -- Connect spiders to merged spider
      d3 = addEdge spiderV1 mergedSpider Simple d2
      d4 = addEdge spiderV2 mergedSpider Simple d3
      d5 = addEdge mergedSpider outV Simple d4
      -- Create new memory (output is always Rough/Z-type)
      mergedMem = Memory (memoryId mem1 + memoryId mem2) Rough 
                        (inputBoundary mem1) outV
      -- Update scalar: merge introduces factor of 1/√2
      d6 = mapScalar (`mulScalar` sqrt2Pow (-1)) d5
      
      -- Positive branch (+1 outcome): no correction needed
      posResult = (d6, mergedMem)
      
      -- Negative branch (-1 outcome): need X correction on output
      -- In ZX: add π-phase to green spider (X operator)
      negDiagram = d6 { _vertices = IM.adjust (\ty -> case ty of Z p -> Z (addPhase p (1 % 1)); _ -> ty) 
                                              mergedSpider (_vertices d6) }
      negResult = (negDiagram, mergedMem)
  in Heralded posResult negResult (1 % 2)

-- | Perform a smooth merge on two memories.
-- Corresponds to red 2-to-1 spider with α=0 in ZX calculus.
-- Measures Z_L^(1) ⊗ Z_L^(2), produces X_L^(3) = X_L^(1) ⊗ X_L^(2).
-- Non-deterministic: +1 outcome (positive branch) or -1 outcome (negative branch).
--
-- Note: Inputs can be mixed boundary types. The output is always Smooth (X-type).
smoothMerge :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory)
smoothMerge d mem1 mem2 =
  let -- Find the existing spiders
      Just spiderV1 = getMemoryBoundary mem1 d
      Just spiderV2 = getMemoryBoundary mem2 d
      -- Create new merged output boundary
      (outV, d1) = allocVertex (Boundary Smooth) d
      -- Create merged spider (always X/red for smooth merge)
      (mergedSpider, d2) = allocVertex (X 0) d1
      -- Connect spiders to merged spider
      d3 = addEdge spiderV1 mergedSpider Simple d2
      d4 = addEdge spiderV2 mergedSpider Simple d3
      d5 = addEdge mergedSpider outV Simple d4
      -- Create new memory (output is always Smooth/X-type)
      mergedMem = Memory (memoryId mem1 + memoryId mem2) Smooth
                        (inputBoundary mem1) outV
      -- Update scalar: merge introduces factor of 1/√2
      d6 = mapScalar (`mulScalar` sqrt2Pow (-1)) d5
      
      -- Positive branch (+1 outcome): no correction needed
      posResult = (d6, mergedMem)
      
      -- Negative branch (-1 outcome): need Z correction on output
      -- In ZX: add π-phase to red spider (Z operator)
      negDiagram = d6 { _vertices = IM.adjust (\ty -> case ty of X p -> X (addPhase p (1 % 1)); _ -> ty) 
                                              mergedSpider (_vertices d6) }
      negResult = (negDiagram, mergedMem)
  in Heralded posResult negResult (1 % 2)
