{-# LANGUAGE LambdaCase #-}

-- | STIM to doubled-diagram converter.
--
--   This module translates STIM circuits directly into doubled ZX diagrams,
--   representing measurements as classical wires and detectors / observables
--   as XOR trees over measurement outcomes.
module HZX.IO.STIM.Doubled
  ( parseSTIMDoubled
  , stimToDoubledDiagram
  ) where

import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec (runParser, errorBundlePretty)

import qualified StimParser.Expr as SE
import qualified StimParser.Parse as SP

import HZX.Circuit (MeasurementBasis(..))
import HZX.Core.Diagram.Types (Vertex, BoundaryType(..))
import HZX.Core.Diagram.Parametric.Types (ParamPhase(..))
import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..), DoubledEdgeType(..)
  , EdgeKind(..) )
import HZX.Core.Diagram.Doubled.Instances
  ( dEmpty, dAllocVertex, dAllocVertices, dAddEdge )


-- | Parse a STIM string into a doubled ZX diagram.
parseSTIMDoubled :: String -> Either String DoubledDiagram
parseSTIMDoubled s =
  let input = if "!!!Start" `isPrefixOf` s then s else "!!!Start " ++ s
  in case runParser SP.parseStim "" input of
       Left err  -> Left (errorBundlePretty err)
       Right ast -> stimToDoubledDiagram ast

-- | Convert a parsed STIM AST into a doubled ZX diagram.
stimToDoubledDiagram :: SE.Stim -> Either String DoubledDiagram
stimToDoubledDiagram ast = do
  -- First pass: determine number of qubits and which qubits are finally measured.
  let (n, measuredQs) = analyzeStim ast
  let (inVs, d0) = dAllocVertices (replicate n (DBoundary Rough)) dEmpty
      outQs = filter (`notElem` measuredQs) [0 .. n - 1]
      (outVs, d1) = dAllocVertices (replicate (length outQs) (DBoundary Rough)) d0
      outMap = IM.fromList (zip outQs outVs)
      d2 = d1 { _dInputs = inVs, _dOutputs = outVs }
      frontiers = IM.fromList (zip [0 .. n - 1] inVs)
      cs0 = ConvState frontiers [] d2 outMap S.empty
  csFinal <- convertStim ast cs0
  let d = _csDiagram csFinal
      d' = IM.foldlWithKey (\dacc q v ->
             case IM.lookup q (_csOutMap csFinal) of
               Just outV -> dAddEdge v outV (DSimple Quantum) dacc
               Nothing   -> dacc) d (_csFrontiers csFinal)
      stack = _csMeasureStack csFinal
      consumed = _csConsumed csFinal
      unconsumed = [ stack !! i | i <- [0 .. length stack - 1]
                                 , not (i `S.member` consumed) ]
      d'' = d' { _dClassicalOutputs = _dClassicalOutputs d' ++ reverse unconsumed }
  return d''

data ConvState = ConvState
  { _csFrontiers :: !(IM.IntMap Vertex)
  , _csMeasureStack :: ![Vertex]
  , _csDiagram :: !DoubledDiagram
  , _csOutMap :: !(IM.IntMap Vertex)
  , _csConsumed :: !(S.Set Int)
  }

-- | Determine the number of qubits and the set of finally-measured qubits.
analyzeStim :: SE.Stim -> (Int, [Int])
analyzeStim stim = go stim (0, [])
  where
    go (SE.StimG g) (n, ms) =
      let n' = max n (gateMaxQ g + 1)
      in (n', ms)
    go (SE.StimM m) (n, ms) =
      let qs = measureQubits m
          n' = max n (maximumDef 0 qs + 1)
          ms' = foldl (\acc q -> if q `notElem` acc then q : acc else acc) ms qs
      in (n', ms')
    go (SE.StimNoise n) (nq, ms) =
      let qs = noiseQubits n
          nq' = max nq (maximumDef 0 qs + 1)
      in (nq', ms)
    go (SE.StimAnn _) s = s
    go (SE.StimGpp _) s = s
    go (SE.StimList ss) s = foldl (flip go) s ss
    go (SE.StimRepeat k s) (n, ms) = go (SE.StimList (replicate k s)) (n, ms)

    maximumDef d [] = d
    maximumDef _ xs = maximum xs

    gateMaxQ (SE.Gate _ _ qs) = maximumDef (-1) [ q | SE.Q q <- qs ]
    measureQubits (SE.Measure _ _ _ qs) = [ q | SE.Q q <- qs ]
    noiseQubits (SE.NoiseNormal _ _ _ _ qs) = [ q | SE.Q q <- qs ]
    noiseQubits (SE.NoiseE _ _ _ ps) = [ q | SE.PauliInd _ q <- ps ]

convertStim :: SE.Stim -> ConvState -> Either String ConvState
convertStim = \case
  SE.StimG g       -> convertGate g
  SE.StimM m       -> convertMeasure m
  SE.StimGpp (SE.Gpp gty _ _ _) ->
    \_ -> Left $ "Unsupported generalized Pauli-product gate: " ++ show gty
  SE.StimNoise n   -> convertNoise n
  SE.StimAnn a     -> convertAnn a
  SE.StimList ss   -> \cs -> foldM (\c s -> convertStim s c) cs ss
  SE.StimRepeat k s -> convertStim (SE.StimList (replicate k s))

qIndex :: SE.Q -> Either String Int
qIndex (SE.Q i) = Right i
qIndex q        = Left $ "Unsupported qubit reference: " ++ show q

qIndices :: [SE.Q] -> Either String [Int]
qIndices = mapM qIndex

-- ---------------------------------------------------------------------------
-- Gate conversion
-- ---------------------------------------------------------------------------

convertGate :: SE.Gate -> ConvState -> Either String ConvState
convertGate (SE.Gate ty _ qs) cs = do
  is <- qIndices qs
  case (ty, is) of
    (SE.H,   [q])    -> applyH q cs
    (SE.S,   [q])    -> applyS q cs
    (SE.X,   [q])    -> applyX q cs
    (SE.Y,   [q])    -> applyY q cs
    (SE.Z,   [q])    -> applyZ q cs
    (SE.CNOT, [c,t]) -> applyCNOT c t cs
    (SE.CZ,  [q1,q2]) -> applyCZ q1 q2 cs
    (SE.SWAP,[q1,q2]) -> applySWAP q1 q2 cs
    _ -> Left $ "Unsupported gate or wrong arity: " ++ show ty ++ " on qubits " ++ show is

applyH :: Int -> ConvState -> Either String ConvState
applyH q cs =
  case IM.lookup q (_csFrontiers cs) of
    Nothing -> Left $ "Qubit " ++ show q ++ " is not active"
    Just v ->
      let (h, d1) = dAllocVertex DHBox (_csDiagram cs)
          d2 = dAddEdge v h (DSimple Quantum) d1
      in Right $ cs { _csFrontiers = IM.insert q h (_csFrontiers cs)
                    , _csDiagram = d2
                    }

applyS :: Int -> ConvState -> Either String ConvState
applyS q cs = applyPhaseSpider q (DZ (ParamPhase (1 % 2) M.empty)) cs

applyX :: Int -> ConvState -> Either String ConvState
applyX q cs = applyPhaseSpider q (DX (ParamPhase 1 M.empty)) cs

applyY :: Int -> ConvState -> Either String ConvState
applyY q cs = applyPhaseSpider q (DZ (ParamPhase 1 M.empty)) cs >>=
              applyPhaseSpider q (DX (ParamPhase (1 % 2) M.empty))

applyZ :: Int -> ConvState -> Either String ConvState
applyZ q cs = applyPhaseSpider q (DZ (ParamPhase 1 M.empty)) cs

applyPhaseSpider :: Int -> DoubledVertexType -> ConvState -> Either String ConvState
applyPhaseSpider q ty cs =
  case IM.lookup q (_csFrontiers cs) of
    Nothing -> Left $ "Qubit " ++ show q ++ " is not active"
    Just v ->
      let (spider, d1) = dAllocVertex ty (_csDiagram cs)
          d2 = dAddEdge v spider (DSimple Quantum) d1
      in Right $ cs { _csFrontiers = IM.insert q spider (_csFrontiers cs)
                    , _csDiagram = d2
                    }

applyCNOT :: Int -> Int -> ConvState -> Either String ConvState
applyCNOT c t cs =
  case (IM.lookup c (_csFrontiers cs), IM.lookup t (_csFrontiers cs)) of
    (Just cv, Just tv) ->
      let (ctrl, d1) = dAllocVertex (DZ (ParamPhase 0 M.empty)) (_csDiagram cs)
          (trgt, d2) = dAllocVertex (DX (ParamPhase 0 M.empty)) d1
          d3 = dAddEdge cv ctrl (DSimple Quantum) d2
          d4 = dAddEdge tv trgt (DSimple Quantum) d3
          d5 = dAddEdge ctrl trgt (DSimple Quantum) d4
      in Right $ cs { _csFrontiers = IM.insert c ctrl $ IM.insert t trgt (_csFrontiers cs)
                    , _csDiagram = d5
                    }
    _ -> Left "CNOT on inactive qubit"

applyCZ :: Int -> Int -> ConvState -> Either String ConvState
applyCZ q1 q2 cs =
  case (IM.lookup q1 (_csFrontiers cs), IM.lookup q2 (_csFrontiers cs)) of
    (Just v1, Just v2) ->
      let (z1, d1) = dAllocVertex (DZ (ParamPhase 0 M.empty)) (_csDiagram cs)
          (z2, d2) = dAllocVertex (DZ (ParamPhase 0 M.empty)) d1
          d3 = dAddEdge v1 z1 (DSimple Quantum) d2
          d4 = dAddEdge v2 z2 (DSimple Quantum) d3
          d5 = dAddEdge z1 z2 (DSimple Quantum) d4
      in Right $ cs { _csFrontiers = IM.insert q1 z1 $ IM.insert q2 z2 (_csFrontiers cs)
                    , _csDiagram = d5
                    }
    _ -> Left "CZ on inactive qubit"

applySWAP :: Int -> Int -> ConvState -> Either String ConvState
applySWAP q1 q2 cs =
  applyCNOT q1 q2 cs >>= applyCNOT q2 q1 >>= applyCNOT q1 q2

-- ---------------------------------------------------------------------------
-- Measurement conversion
-- ---------------------------------------------------------------------------

convertMeasure :: SE.Measure -> ConvState -> Either String ConvState
convertMeasure (SE.Measure ty _ _ qs) cs = do
  is <- qIndices qs
  case ty of
    SE.M   -> foldM (\c q -> applyMeasure DMeasureZ q c) cs is
    SE.MZ  -> foldM (\c q -> applyMeasure DMeasureZ q c) cs is
    SE.MX  -> foldM (\c q -> applyMeasure DMeasureX q c) cs is
    SE.MR  -> foldM (\c q -> applyMeasure DMeasureZ q c >>= applyReset ZBasis q) cs is
    SE.MRX -> foldM (\c q -> applyMeasure DMeasureX q c >>= applyReset XBasis q) cs is
    SE.MRZ -> foldM (\c q -> applyMeasure DMeasureZ q c >>= applyReset ZBasis q) cs is
    _      -> Left $ "Unsupported measurement type: " ++ show ty

applyMeasure :: DoubledVertexType -> Int -> ConvState -> Either String ConvState
applyMeasure mty q cs =
  case IM.lookup q (_csFrontiers cs) of
    Nothing -> Left $ "Qubit " ++ show q ++ " is not active"
    Just v ->
      let (m, d1) = dAllocVertex mty (_csDiagram cs)
          (outV, d2) = dAllocVertex (DBoundary Rough) d1
          d3 = dAddEdge v m (DSimple Quantum) d2
          d4 = dAddEdge m outV (DSimple Classical) d3
      in Right $ cs { _csFrontiers = IM.delete q (_csFrontiers cs)
                    , _csMeasureStack = outV : _csMeasureStack cs
                    , _csDiagram = d4
                    }

-- ---------------------------------------------------------------------------
-- Reset conversion
-- ---------------------------------------------------------------------------

applyReset :: MeasurementBasis -> Int -> ConvState -> Either String ConvState
applyReset basis q cs =
  let prep = case basis of
        ZBasis -> DZ (ParamPhase 0 M.empty)
        XBasis -> DX (ParamPhase 0 M.empty)
      (r, d1) = dAllocVertex prep (_csDiagram cs)
  in Right $ cs { _csFrontiers = IM.insert q r (_csFrontiers cs)
                , _csDiagram = d1
                }

-- ---------------------------------------------------------------------------
-- Noise conversion
-- ---------------------------------------------------------------------------

convertNoise :: SE.Noise -> ConvState -> Either String ConvState
convertNoise (SE.NoiseNormal ty _ _ phs qs) cs = do
  is <- qIndices qs
  case (ty, is) of
    (SE.X_ERROR, [q]) ->
      case phs of
        [_] -> applyParamPauli q (DX (ParamPhase 0 (M.singleton ("xerr_" ++ show q) 1))) cs
        _   -> Left "X_ERROR expects one probability"
    (SE.Z_ERROR, [q]) ->
      case phs of
        [_] -> applyParamPauli q (DZ (ParamPhase 0 (M.singleton ("zerr_" ++ show q) 1))) cs
        _   -> Left "Z_ERROR expects one probability"
    (SE.DEPOLARIZE1, [q]) ->
      case phs of
        [_] ->
          let px = "dx_" ++ show q
              pz = "dz_" ++ show q
          in applyParamPauli q (DX (ParamPhase 0 (M.singleton px 1))) cs >>=
             applyParamPauli q (DZ (ParamPhase 0 (M.singleton pz 1)))
        _ -> Left "DEPOLARIZE1 expects one probability"
    _ -> Left $ "Unsupported noise channel: " ++ show ty
convertNoise n _ = Left $ "Unsupported noise syntax: " ++ show n

applyParamPauli :: Int -> DoubledVertexType -> ConvState -> Either String ConvState
applyParamPauli q ty cs =
  case IM.lookup q (_csFrontiers cs) of
    Nothing -> Left $ "Qubit " ++ show q ++ " is not active"
    Just v ->
      let (spider, d1) = dAllocVertex ty (_csDiagram cs)
          d2 = dAddEdge v spider (DSimple Quantum) d1
      in Right $ cs { _csFrontiers = IM.insert q spider (_csFrontiers cs)
                    , _csDiagram = d2
                    }

-- ---------------------------------------------------------------------------
-- Annotation conversion
-- ---------------------------------------------------------------------------

convertAnn :: SE.Ann -> ConvState -> Either String ConvState
convertAnn (SE.Ann SE.TICK _ _ _) cs = Right cs
convertAnn (SE.Ann SE.DETECTOR _ _ qs) cs = applyDetector qs cs
convertAnn (SE.Ann SE.OBSERVABLE_INCLUDE _ (_:_) qs) cs = applyObservable qs cs
convertAnn (SE.Ann SE.OBSERVABLE_INCLUDE _ [] _qs) _cs = Left "OBSERVABLE_INCLUDE without index"
convertAnn _ cs = Right cs  -- skip QUBIT_COORDS, SHIFT_COORDS, etc.

-- | Build an XOR tree over the measurement outcomes referenced by 'qs' and
--   attach the result to a fresh classical output boundary labeled as a
--   detector.
applyDetector :: [SE.Q] -> ConvState -> Either String ConvState
applyDetector qs cs = do
  (idxs, ws) <- unzip <$> mapM (resolveRec (_csMeasureStack cs)) qs
  let (xorOut, d1) = buildXorTree ws (_csDiagram cs)
      (detV, d2) = dAllocVertex (DBoundary Smooth) d1
      d3 = dAddEdge xorOut detV (DSimple Classical) d2
  return $ cs { _csDiagram = d3 { _dClassicalOutputs = _dClassicalOutputs d3 ++ [detV] }
              , _csConsumed = S.union (_csConsumed cs) (S.fromList idxs)
              }

-- | Build an XOR tree over measurement outcomes for a logical observable.
applyObservable :: [SE.Q] -> ConvState -> Either String ConvState
applyObservable qs cs = do
  (idxs, ws) <- unzip <$> mapM (resolveRec (_csMeasureStack cs)) qs
  let (xorOut, d1) = buildXorTree ws (_csDiagram cs)
      (obsV, d2) = dAllocVertex (DBoundary Smooth) d1
      d3 = dAddEdge xorOut obsV (DSimple Classical) d2
  return $ cs { _csDiagram = d3 { _dObservableOutputs = _dObservableOutputs d3 ++ [obsV] }
              , _csConsumed = S.union (_csConsumed cs) (S.fromList idxs)
              }

-- | Look up a measurement outcome wire by rec[-k] reference, returning both
--   the stack index and the vertex.
resolveRec :: [Vertex] -> SE.Q -> Either String (Int, Vertex)
resolveRec stack (SE.QRec (SE.Rec k)) =
  let idx = negate k - 1
  in if idx >= 0 && idx < length stack
     then Right (idx, stack !! idx)
     else Left $ "rec[" ++ show k ++ "] out of range"
resolveRec _ (SE.Q i) = Right (i, i)
resolveRec _ q = Left $ "Unsupported detector reference: " ++ show q

-- | Build a binary XOR tree over a list of classical wires, returning the
--   output wire (the root of the tree).
buildXorTree :: [Vertex] -> DoubledDiagram -> (Vertex, DoubledDiagram)
buildXorTree [] _d = error "buildXorTree: empty input list"
buildXorTree [w] d = (w, d)
buildXorTree ws d =
  let (pairs, rest) = splitPairs ws
      (outs, d') = foldl buildPair ([], d) pairs
  in buildXorTree (outs ++ rest) d'
  where
    splitPairs [] = ([], [])
    splitPairs [x] = ([], [x])
    splitPairs (x:y:zs) = let (ps, r) = splitPairs zs in ((x,y):ps, r)

    buildPair (acc, d0) (x, y) =
      let (xor, d1) = dAllocVertex DXor d0
          d2 = dAddEdge x xor (DSimple Classical) d1
          d3 = dAddEdge y xor (DSimple Classical) d2
      in (xor : acc, d3)
