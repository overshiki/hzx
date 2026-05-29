{-# LANGUAGE TupleSections #-}

module HZX.LatticeSurgery.Protocols.CNOT
  ( -- * Standard CNOT
    cnotLS
    -- * CNOT Variants
  , cnotLS_VariantA
  , cnotLS_VariantB
  , cnotLS_VariantC
  ) where

import qualified Data.IntMap as IM
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase (addPhase)
import HZX.Core.Scalar (sqrt2Pow, mulScalar)
import HZX.LatticeSurgery.Core
import HZX.LatticeSurgery.Heralded

-- | Standard lattice surgery CNOT.
-- 
-- Protocol (from Horsman et al. 2012):
-- 1. Smooth split of control qubit
-- 2. Rough merge of one daughter with target
--
-- ZX Representation:
--       control                    target
--          |                        |
--         [X] (split)               [Z]
--        /   \                       |
--       /     \________             |
--      |               \            |
--      |               [Z] (merge)  |
--      |                |           |
--    (ctrl1)         result        target
--
-- This is topologically equivalent to the ZX CNOT diagram.
cnotLS :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory, Memory)
cnotLS d control target
  | boundaryType control /= Smooth =
      error "cnotLS: control must be Smooth boundary (X-type)"
  | boundaryType target /= Rough =
      error "cnotLS: target must be Rough boundary (Z-type)"
  | otherwise =
      -- Step 1: Smooth split of control
      let (d1, ctrl1, ctrl2) = smoothSplit d control
      in -- Step 2: Rough merge of ctrl1 with target
         -- This produces the CNOT effect
         let heraldedMerge = roughMerge d1 ctrl1 target
         in fmap (\(d2, resultMem) -> (d2, ctrl2, resultMem)) heraldedMerge

-- | CNOT Variant A: Rough split of target, smooth merge with control.
-- 
-- ZX Representation:
--       control                 target
--          |                      |
--         [X]                   [Z] (split)
--          |                   /   \
--          |________          /     \
--                   \        |       |
--                   [X] (merge)    (targ1)
--                    |
--                 result
--
-- Topologically equivalent to standard CNOT, just different qubit roles.
cnotLS_VariantA :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory, Memory)
cnotLS_VariantA d control target
  | boundaryType control /= Smooth =
      error "cnotLS_VariantA: control must be Smooth boundary"
  | boundaryType target /= Rough =
      error "cnotLS_VariantA: target must be Rough boundary"
  | otherwise =
      -- Step 1: Rough split of target
      let (d1, targ1, targ2) = roughSplit d target
      in -- Step 2: Smooth merge of targ1 with control
         let heraldedMerge = smoothMerge d1 control targ1
         in fmap (\(d2, resultMem) -> (d2, resultMem, targ2)) heraldedMerge

-- | CNOT Variant B: Bell pair creation then merges.
--
-- Protocol:
-- 1. Create Bell pair |Φ+⟩ = |00⟩ + |11⟩ = |++⟩ + |--⟩
-- 2. Rough merge one half with control
-- 3. Smooth merge other half with target
--
-- ZX Representation:
--    control       Bell pair        target
--       |         ___|___           |
--      [X]_______/       \_____    [Z]
--       |     rough      smooth    |
--       |      merge      merge    |
--       |        |         |       |
--    result    (half)   (half)   target
--
-- This variant is useful for distributed quantum computing.
cnotLS_VariantB :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory, Memory)
cnotLS_VariantB d control target
  | boundaryType control /= Smooth =
      error "cnotLS_VariantB: control must be Smooth boundary"
  | boundaryType target /= Rough =
      error "cnotLS_VariantB: target must be Rough boundary"
  | otherwise =
      -- Step 1: Create Bell pair via rough split of |+⟩ state
      let (d0, bellMem, _) = createMemory Rough d  -- Start with rough memory
          (d1, bell1, bell2) = roughSplit d0 bellMem
      in -- Step 2: Smooth merge bell1 with control
         let heraldedMerge1 = smoothMerge d1 control bell1
         in -- For simplicity, we use the positive branch of first merge
            -- and create the second merge
            let (d2, ctrlResult) = positiveBranch heraldedMerge1
                heraldedMerge2 = roughMerge d2 bell2 target
            in fmap (\(d3, targResult) -> (d3, ctrlResult, targResult)) heraldedMerge2

-- | CNOT Variant C: Double split (split both control and target).
--
-- Protocol:
-- 1. Smooth split of control
-- 2. Rough split of target
-- 3. Bell projection on one daughter of each
--
-- This variant demonstrates multi-memory operations.
cnotLS_VariantC :: Diagram -> Memory -> Memory -> Heralded (Diagram, Memory, Memory)
cnotLS_VariantC d control target
  | boundaryType control /= Smooth =
      error "cnotLS_VariantC: control must be Smooth boundary"
  | boundaryType target /= Rough =
      error "cnotLS_VariantC: target must be Rough boundary"
  | otherwise =
      -- Step 1: Split both qubits
      let (d1, ctrl1, ctrl2) = smoothSplit d control
          (d2, targ1, targ2) = roughSplit d1 target
      in -- Step 2: Rough merge ctrl1 with targ1 (Bell projection)
         let heraldedMerge = roughMerge d2 ctrl1 targ1
         in fmap (\(d3, projResult) -> (d3, ctrl2, targ2)) heraldedMerge
