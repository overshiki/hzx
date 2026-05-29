module HZX.Circuit.ToDiagram where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Circuit

-- | Convert a quantum circuit to a ZX-diagram.
circuitToDiagram :: Circuit -> Diagram
circuitToDiagram circ =
  let n = numQubits circ
      (inVs, d0) = allocVertices (replicate n (Boundary Rough)) empty
      (outVs, d1) = allocVertices (replicate n (Boundary Rough)) d0
      d2 = d1 { _inputs = inVs, _outputs = outVs }
      frontiers = IM.fromList (zip [0..n-1] inVs)
      (finalFs, d3) = foldl (\(fs, d') g -> applyGate fs d' g) (frontiers, d2) (gates circ)
      -- Connect final frontiers to output boundaries
      d4 = IM.foldlWithKey (\dacc q v -> addEdge v (outVs !! q) Simple dacc) d3 finalFs
  in d4

applyGate :: IntMap Vertex -> Diagram -> Gate -> (IntMap Vertex, Diagram)
applyGate fs d (H q) =
  let v = fs IM.! q
      (spider, d1) = allocVertex HBox d
      d2 = addEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (S q) =
  let v = fs IM.! q
      (spider, d1) = allocVertex (Z (1 % 2)) d
      d2 = addEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (T q) =
  let v = fs IM.! q
      (spider, d1) = allocVertex (Z (1 % 4)) d
      d2 = addEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (ZPhase q p) =
  let v = fs IM.! q
      (spider, d1) = allocVertex (Z p) d
      d2 = addEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (XPhase q p) =
  let v = fs IM.! q
      (spider, d1) = allocVertex (X p) d
      d2 = addEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (CNOT c t) =
  let cv = fs IM.! c
      tv = fs IM.! t
      (ctrl, d1) = allocVertex (Z 0) d
      (trgt, d2) = allocVertex (X 0) d1
      d3 = addEdge cv ctrl Simple d2
      d4 = addEdge tv trgt Simple d3
      d5 = addEdge ctrl trgt Simple d4
  in (IM.insert c ctrl $ IM.insert t trgt fs, d5)

applyGate fs d (CZ q1 q2) =
  let v1 = fs IM.! q1
      v2 = fs IM.! q2
      (z1, d1) = allocVertex (Z 0) d
      (z2, d2) = allocVertex (Z 0) d1
      d3 = addEdge v1 z1 Simple d2
      d4 = addEdge v2 z2 Simple d3
      d5 = addEdge z1 z2 Simple d4
  in (IM.insert q1 z1 $ IM.insert q2 z2 fs, d5)

applyGate fs d (SWAP q1 q2) =
  -- Decompose SWAP into 3 CNOTs
  let (fs1, d1) = applyGate fs d (CNOT q1 q2)
      (fs2, d2) = applyGate fs1 d1 (CNOT q2 q1)
      (fs3, d3) = applyGate fs2 d2 (CNOT q1 q2)
  in (fs3, d3)
