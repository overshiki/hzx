{-# LANGUAGE TupleSections #-}

-- | Convert parametric circuits to parametric ZX diagrams.
module HZX.Circuit.ToParametricDiagram
  ( parametricCircuitToDiagram
  , evalParametricDiagram
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio ((%))

import HZX.Circuit (Gate(..), MeasurementBasis(..), Circuit(..), finallyMeasuredQubits)
import HZX.Circuit.Parametric
  ( ParametricCircuit(..), ParametricOp(..), NoiseChannel(..), pcNumQubits )
import HZX.Core.Diagram.Types (Vertex, EdgeType(..), BoundaryType(..))
import HZX.Core.Diagram.Parametric.Types
  ( ParamDiagram(..), ParamVertexType(..), ParamPhase(..) )
import HZX.Core.Diagram.Parametric.Instances
  ( pEmpty, pAllocVertex, pAllocVertices, pAddEdge )
import HZX.Core.Diagram.Parametric.Phase (evalParamPhase)
import HZX.Core.Diagram.Parametric.Scalar (evalParamScalar)
import qualified HZX.Core.Diagram as D

-- | Convert a parametric circuit to a parametric ZX diagram.
parametricCircuitToDiagram :: ParametricCircuit -> ParamDiagram
parametricCircuitToDiagram pc =
  let n = pcNumQubits pc
      gatesOnly = [g | POGate g <- pcOps pc]
      finallyMeasured = finallyMeasuredQubits (Circuit gatesOnly)
      finallyMeasuredSet = S.fromList (S.toList finallyMeasured)
      outQs = filter (`S.notMember` finallyMeasuredSet) [0 .. n - 1]
      (inVs, d0) = pAllocVertices (replicate n (ParamBoundary Rough)) pEmpty
      (outVs, d1) = pAllocVertices (replicate (length outQs) (ParamBoundary Rough)) d0
      outMap = IM.fromList (zip outQs outVs)
      d2 = d1 { _pInputs = inVs, _pOutputs = outVs }
      frontiers = IM.fromList (zip [0 .. n - 1] inVs)
      (finalFs, d3) = foldl (\(fs, d') op -> applyOp fs d' op) (frontiers, d2) (pcOps pc)
      d4 = IM.foldlWithKey (\dacc q v ->
              case IM.lookup q outMap of
                Just outV -> pAddEdge v outV Simple dacc
                Nothing   -> dacc) d3 finalFs
  in d4

applyOp :: IntMap Vertex -> ParamDiagram -> ParametricOp -> (IntMap Vertex, ParamDiagram)
applyOp fs d (POGate g) = applyGate fs d g
applyOp fs d (PONoise nc) = applyNoise fs d nc

applyGate :: IntMap Vertex -> ParamDiagram -> Gate -> (IntMap Vertex, ParamDiagram)
applyGate fs d (H q) =
  let v = fs IM.! q
      (spider, d1) = pAllocVertex ParamHBox d
      d2 = pAddEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (S q) =
  let v = fs IM.! q
      (spider, d1) = pAllocVertex (ParamZ (ParamPhase (1 % 2) M.empty)) d
      d2 = pAddEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (T q) =
  let v = fs IM.! q
      (spider, d1) = pAllocVertex (ParamZ (ParamPhase (1 % 4) M.empty)) d
      d2 = pAddEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (ZPhase q p) =
  let v = fs IM.! q
      (spider, d1) = pAllocVertex (ParamZ (ParamPhase p M.empty)) d
      d2 = pAddEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (XPhase q p) =
  let v = fs IM.! q
      (spider, d1) = pAllocVertex (ParamX (ParamPhase p M.empty)) d
      d2 = pAddEdge v spider Simple d1
  in (IM.insert q spider fs, d2)

applyGate fs d (CNOT c t) =
  let cv = fs IM.! c
      tv = fs IM.! t
      (ctrl, d1) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d
      (trgt, d2) = pAllocVertex (ParamX (ParamPhase 0 M.empty)) d1
      d3 = pAddEdge cv ctrl Simple d2
      d4 = pAddEdge tv trgt Simple d3
      d5 = pAddEdge ctrl trgt Simple d4
  in (IM.insert c ctrl $ IM.insert t trgt fs, d5)

applyGate fs d (CZ q1 q2) =
  let v1 = fs IM.! q1
      v2 = fs IM.! q2
      (z1, d1) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d
      (z2, d2) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d1
      d3 = pAddEdge v1 z1 Simple d2
      d4 = pAddEdge v2 z2 Simple d3
      d5 = pAddEdge z1 z2 Simple d4
  in (IM.insert q1 z1 $ IM.insert q2 z2 fs, d5)

applyGate fs d (SWAP q1 q2) =
  let (fs1, d1) = applyGate fs d (CNOT q1 q2)
      (fs2, d2) = applyGate fs1 d1 (CNOT q2 q1)
      (fs3, d3) = applyGate fs2 d2 (CNOT q1 q2)
  in (fs3, d3)

applyGate fs d (Measure _basis q) =
  let v = fs IM.! q
      (m, d1) = pAllocVertex (ParamBoundary Rough) d
      d2 = pAddEdge v m Simple d1
  in (IM.delete q fs, d2)

applyGate fs d (Reset basis q) =
  let (d1, fs1) = case IM.lookup q fs of
        Just v ->
          let (m, d') = pAllocVertex (ParamBoundary Rough) d
          in (pAddEdge v m Simple d', IM.delete q fs)
        Nothing -> (d, fs)
      prep = case basis of
        ZBasis -> ParamZ (ParamPhase 0 M.empty)
        XBasis -> ParamX (ParamPhase 0 M.empty)
      (r, d2) = pAllocVertex prep d1
  in (IM.insert q r fs1, d2)

applyNoise :: IntMap Vertex -> ParamDiagram -> NoiseChannel -> (IntMap Vertex, ParamDiagram)
applyNoise fs d (XError param q _) =
  insertParamPauli fs d q (ParamX (ParamPhase 0 (M.singleton param 1)))

applyNoise fs d (ZError param q _) =
  insertParamPauli fs d q (ParamZ (ParamPhase 0 (M.singleton param 1)))

applyNoise fs d (Depolarize1 px pz q _ _ _) =
  -- DEPOLARIZE1 is represented by two binary variables e_x and e_z.
  -- Outcomes: (0,0)=I, (1,0)=X, (0,1)=Z, (1,1)=Y (= XZ).
  let phaseX = ParamPhase 0 (M.singleton px 1)
      phaseZ = ParamPhase 0 (M.singleton pz 1)
      (fs1, d1) = insertParamPauli fs d q (ParamX phaseX)
      (fs2, d2) = insertParamPauli fs1 d1 q (ParamZ phaseZ)
  in (fs2, d2)

insertParamPauli :: IntMap Vertex -> ParamDiagram -> Int -> ParamVertexType -> (IntMap Vertex, ParamDiagram)
insertParamPauli fs d q ty =
  case IM.lookup q fs of
    Just v ->
      let (spider, d1) = pAllocVertex ty d
          d2 = pAddEdge v spider Simple d1
      in (IM.insert q spider fs, d2)
    Nothing -> (fs, d)

-- | Evaluate a parametric diagram under a concrete noise assignment,
--   producing an ordinary 'Diagram'.
evalParametricDiagram :: M.Map String Bool -> ParamDiagram -> D.Diagram
evalParametricDiagram env pd =
  let vs = IM.map convertVertex (_pVertices pd)
      sc = evalParamScalar env (_pScalar pd)
  in D.Diagram
       { D._vertices = vs
       , D._edges = _pEdges pd
       , D._neighborMap = _pNeighborMap pd
       , D._inputs = _pInputs pd
       , D._outputs = _pOutputs pd
       , D._nextId = _pNextId pd
       , D._scalar = sc
       }
  where
    convertVertex (ParamBoundary bt) = D.Boundary bt
    convertVertex (ParamZ p) = D.Z (evalParamPhase env p)
    convertVertex (ParamX p) = D.X (evalParamPhase env p)
    convertVertex ParamHBox = D.HBox
