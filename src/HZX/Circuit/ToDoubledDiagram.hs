{-# LANGUAGE TupleSections #-}

-- | Convert parametric circuits to doubled ZX diagrams.
module HZX.Circuit.ToDoubledDiagram
  ( circuitToDoubledDiagram
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio ((%))

import HZX.Circuit (Gate(..), MeasurementBasis(..), Circuit(..), finallyMeasuredQubits)
import HZX.Circuit.Parametric
  ( ParametricCircuit(..), ParametricOp(..), NoiseChannel(..), pcNumQubits )
import HZX.Core.Diagram.Types (Vertex, BoundaryType(..))
import HZX.Core.Diagram.Parametric.Types (ParamPhase(..))
import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..), DoubledEdgeType(..), EdgeKind(..) )
import HZX.Core.Diagram.Doubled.Instances
  ( dEmpty, dAllocVertex, dAllocVertices, dAddEdge )

-- | Convert a parametric circuit to a doubled ZX diagram.
circuitToDoubledDiagram :: ParametricCircuit -> DoubledDiagram
circuitToDoubledDiagram pc =
  let n = pcNumQubits pc
      gatesOnly = [g | POGate g <- pcOps pc]
      finallyMeasured = finallyMeasuredQubits (Circuit gatesOnly)
      outQs = filter (`S.notMember` finallyMeasured) [0 .. n - 1]
      (inVs, d0) = dAllocVertices (replicate n (DBoundary Rough)) dEmpty
      (outVs, d1) = dAllocVertices (replicate (length outQs) (DBoundary Rough)) d0
      outMap = IM.fromList (zip outQs outVs)
      d2 = d1 { _dInputs = inVs, _dOutputs = outVs }
      frontiers = IM.fromList (zip [0 .. n - 1] inVs)
      (finalFs, d3) = foldl (\(fs, d') op -> applyOp fs d' op) (frontiers, d2) (pcOps pc)
      d4 = IM.foldlWithKey (\dacc q v ->
              case IM.lookup q outMap of
                Just outV -> dAddEdge v outV (DSimple Quantum) dacc
                Nothing   -> dacc) d3 finalFs
  in d4

applyOp :: IntMap Vertex -> DoubledDiagram -> ParametricOp -> (IntMap Vertex, DoubledDiagram)
applyOp fs d (POGate g) = applyGate fs d g
applyOp fs d (PONoise nc) = applyNoise fs d nc

applyGate :: IntMap Vertex -> DoubledDiagram -> Gate -> (IntMap Vertex, DoubledDiagram)
applyGate fs d (H q) = insertQuantumSpider fs d q DHBox
applyGate fs d (S q) = insertQuantumSpider fs d q (DZ (ParamPhase (1 % 2) M.empty))
applyGate fs d (T q) = insertQuantumSpider fs d q (DZ (ParamPhase (1 % 4) M.empty))
applyGate fs d (ZPhase q p) = insertQuantumSpider fs d q (DZ (ParamPhase p M.empty))
applyGate fs d (XPhase q p) = insertQuantumSpider fs d q (DX (ParamPhase p M.empty))

applyGate fs d (CNOT c t) =
  case (IM.lookup c fs, IM.lookup t fs) of
    (Just cv, Just tv) ->
      let (ctrl, d1) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d
          (trgt, d2) = dAllocVertex (DX (ParamPhase 0 M.empty)) d1
          d3 = dAddEdge cv ctrl (DSimple Quantum) d2
          d4 = dAddEdge tv trgt (DSimple Quantum) d3
          d5 = dAddEdge ctrl trgt (DSimple Quantum) d4
      in (IM.insert c ctrl $ IM.insert t trgt fs, d5)
    _ -> error "CNOT on inactive qubit"

applyGate fs d (CZ q1 q2) =
  case (IM.lookup q1 fs, IM.lookup q2 fs) of
    (Just v1, Just v2) ->
      let (z1, d1) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d
          (z2, d2) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d1
          d3 = dAddEdge v1 z1 (DSimple Quantum) d2
          d4 = dAddEdge v2 z2 (DSimple Quantum) d3
          d5 = dAddEdge z1 z2 (DSimple Quantum) d4
      in (IM.insert q1 z1 $ IM.insert q2 z2 fs, d5)
    _ -> error "CZ on inactive qubit"

applyGate fs d (SWAP q1 q2) =
  let (fs1, d1) = applyGate fs d (CNOT q1 q2)
      (fs2, d2) = applyGate fs1 d1 (CNOT q2 q1)
      (fs3, d3) = applyGate fs2 d2 (CNOT q1 q2)
  in (fs3, d3)

applyGate fs d (Measure basis q) =
  case IM.lookup q fs of
    Nothing -> (fs, d)
    Just v ->
      let mty = case basis of ZBasis -> DMeasureZ; XBasis -> DMeasureX
          (m, d1) = dAllocVertex mty d
          (outV, d2) = dAllocVertex (DBoundary Rough) d1
          d3 = dAddEdge v m (DSimple Quantum) d2
          d4 = dAddEdge m outV (DSimple Classical) d3
      in ( IM.delete q fs
         , d4 { _dClassicalOutputs = _dClassicalOutputs d4 ++ [outV] }
         )

applyGate fs d (Reset basis q) =
  let (d1, fs1) = case IM.lookup q fs of
        Just v ->
          let (term, d') = dAllocVertex (DBoundary Rough) d
          in (dAddEdge v term (DSimple Quantum) d', IM.delete q fs)
        Nothing -> (d, fs)
      prep = case basis of ZBasis -> DZ (ParamPhase 0 M.empty); XBasis -> DX (ParamPhase 0 M.empty)
      (r, d2) = dAllocVertex prep d1
  in (IM.insert q r fs1, d2)

applyNoise :: IntMap Vertex -> DoubledDiagram -> NoiseChannel -> (IntMap Vertex, DoubledDiagram)
applyNoise fs d (XError param q _) =
  insertDoubledPauli fs d q (DX (ParamPhase 0 (M.singleton param 1)))
applyNoise fs d (ZError param q _) =
  insertDoubledPauli fs d q (DZ (ParamPhase 0 (M.singleton param 1)))
applyNoise fs d (Depolarize1 px pz q _ _ _) =
  let (fs1, d1) = insertDoubledPauli fs d q (DX (ParamPhase 0 (M.singleton px 1)))
  in insertDoubledPauli fs1 d1 q (DZ (ParamPhase 0 (M.singleton pz 1)))

insertQuantumSpider :: IntMap Vertex -> DoubledDiagram -> Int -> DoubledVertexType
                    -> (IntMap Vertex, DoubledDiagram)
insertQuantumSpider fs d q ty =
  case IM.lookup q fs of
    Nothing -> (fs, d)
    Just v ->
      let (spider, d1) = dAllocVertex ty d
          d2 = dAddEdge v spider (DSimple Quantum) d1
      in (IM.insert q spider fs, d2)

insertDoubledPauli :: IntMap Vertex -> DoubledDiagram -> Int -> DoubledVertexType
                   -> (IntMap Vertex, DoubledDiagram)
insertDoubledPauli = insertQuantumSpider
