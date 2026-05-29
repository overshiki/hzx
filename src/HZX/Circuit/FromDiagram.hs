{-# LANGUAGE TupleSections #-}

module HZX.Circuit.FromDiagram
  ( diagramToCircuit
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List (foldl', nub)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes)
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase
import HZX.Circuit
import HZX.LinAlg.Z2

-- | Extract a quantum circuit from a ZX-diagram.
--   Stage 1 implementation: handles simple cases, returns empty for complex diagrams.
diagramToCircuit :: Diagram -> Circuit
diagramToCircuit d =
  -- First, convert to graph-like form
  let d' = toGraphLike d
  in extractSimple d'

-- | Convert a diagram to graph-like form:
--   All interior spiders are Z-spiders, all interior edges are Hadamard edges.
toGraphLike :: Diagram -> Diagram
toGraphLike d =
  let d1 = runStrategy hadamardEdgeSimp d
      d2 = runStrategy colorChangeAll d1
      d3 = runStrategy spiderFusionAll d2
  in d3
  where
    runStrategy f d0 =
      case f d0 of
        Nothing -> d0
        Just d' -> runStrategy f d'
    
    hadamardEdgeSimp d0 = listToMaybe $ mapMaybe tryVertex $ IM.keys (_vertices d0)
      where
        tryVertex v = case IM.lookup v (_vertices d0) of
          Just HBox -> do
            nb <- IM.lookup v (_neighborMap d0)
            let ns = M.toList nb
            case ns of
              [(a, _), (b, _)] | a /= b ->
                let d1 = removeVertex v d0
                    d2 = addEdge a b Hadamard d1
                in Just d2
              _ -> Nothing
          _ -> Nothing
    
    colorChangeAll d0 = listToMaybe $ mapMaybe tryVertex $ IM.keys (_vertices d0)
      where
        tryVertex v = case IM.lookup v (_vertices d0) of
          Just (X p) -> Just (flipColor v p d0)
          _ -> Nothing
        
        flipColor v p dOrig =
          let nb = neighborBundles v dOrig
              d1 = dOrig { _vertices = IM.adjust (const (Z p)) v (_vertices dOrig) }
              step acc (w, bndl) =
                let sCount = simpleCount bndl
                    hCount = hadamardCount bndl
                    acc1 = removeEdgeBundle v w acc
                    acc2 = foldl (\a _ -> addEdge v w Hadamard a) acc1 [1..sCount]
                    acc3 = foldl (\a _ -> addEdge v w Simple a) acc2 [1..hCount]
                in acc3
          in foldl step d1 (M.toList nb)
    
    spiderFusionAll d0 = listToMaybe $ mapMaybe tryPair pairs
      where
        vs = IM.keys (_vertices d0)
        pairs = [(v1, v2) | v1 <- vs, v2 <- vs, v1 < v2]
        
        tryPair (v1, v2) = do
          t1 <- IM.lookup v1 (_vertices d0)
          t2 <- IM.lookup v2 (_vertices d0)
          case (t1, t2) of
            (Z p1, Z p2) -> fuse v1 v2 p1 p2 d0
            _ -> Nothing
        
        fuse v1 v2 p1 p2 dOrig = do
          nb <- IM.lookup v1 (_neighborMap dOrig)
          if not (M.member v2 nb) then Nothing else do
            let v2Nbs = neighborBundles v2 dOrig
                dWithoutV2 = removeVertex v2 dOrig
                redirect acc (w, bndl) =
                  if w == v1 then acc else mergeEdges v1 w bndl acc
                dRedirected = foldl redirect dWithoutV2 (M.toList v2Nbs)
                dFinal = dRedirected { _vertices = IM.adjust (const (Z (addPhase p1 p2))) v1 (_vertices dRedirected) }
            return dFinal

-- | Extract a circuit from a graph-like diagram.
--   Stage 1: Simple extraction that handles basic cases.
extractSimple :: Diagram -> Circuit
extractSimple d =
  let -- Get all Z spiders and their phases
      zSpiders = [(v, p) | (v, Z p) <- IM.toList (_vertices d)]
      -- Get boundary information
      inBdrs = inputs d
      outBdrs = outputs d
      n = max (length inBdrs) (length outBdrs)
      
      -- Build a mapping from spider to which "qubit line" it belongs to
      -- by tracing from inputs
      spiderToQubit = IM.fromList $ catMaybes $ map (\(idx, bv) -> 
        case traceToSpider bv d of
          Just v -> Just (v, idx)
          Nothing -> Nothing) (zip [0..] inBdrs)
      
      -- Extract single-qubit gates from spiders
      singleQubitGates = concatMap (extractSingleQubit spiderToQubit d) zSpiders
      
      -- Extract CNOTs from connectivity
      cnotGates = extractCNOTs d spiderToQubit
      
      allGates = sortGates (singleQubitGates ++ cnotGates)
  in Circuit allGates
  where
    -- Trace from a boundary to find the connected spider
    traceToSpider :: Vertex -> Diagram -> Maybe Vertex
    traceToSpider v d0 =
      case neighbors v d0 of
        [] -> Nothing
        [w] -> case IM.lookup w (_vertices d0) of
                 Just (Boundary _) -> Nothing
                 Just (Z _) -> Just w
                 _ -> traceToSpider w d0
        _ -> Nothing  -- Branching
    
    -- Extract single-qubit gates from a Z spider
    extractSingleQubit :: IM.IntMap Int -> Diagram -> (Vertex, Rational) -> [Gate]
    extractSingleQubit qubitMap d0 (v, p) =
      case IM.lookup v qubitMap of
        Just q -> 
          let gates = []
              -- Add Z rotation if phase is non-zero
              gates' = if p /= 0 then ZPhase q p : gates else gates
              -- Check if connected via Hadamard (would indicate H gates)
              gates'' = checkHadamards d0 v q gates'
          in gates''
        Nothing -> []
    
    -- Check if spider has Hadamard connections that indicate H gates
    checkHadamards :: Diagram -> Vertex -> Int -> [Gate] -> [Gate]
    checkHadamards d0 v q acc =
      let nbs = neighborBundles v d0
          -- Count Hadamard edges
          hCount = sum [hadamardCount b | (_, b) <- M.toList nbs]
      in if hCount > 0
         then H q : acc  -- Simplified: add H if any Hadamard edges
         else acc
    
    -- Extract CNOT gates from spider connectivity
    extractCNOTs :: Diagram -> IM.IntMap Int -> [Gate]
    extractCNOTs d0 qubitMap =
      -- Look for pairs of connected Z-spiders that form CNOT patterns
      let zSpiders = [v | (v, Z _) <- IM.toList (_vertices d0)]
          pairs = [(v1, v2) | v1 <- zSpiders, v2 <- zSpiders, v1 < v2]
      in concatMap (tryExtractCNOT d0 qubitMap) pairs
    
    tryExtractCNOT :: Diagram -> IM.IntMap Int -> (Vertex, Vertex) -> [Gate]
    tryExtractCNOT d0 qubitMap (v1, v2) =
      case (IM.lookup v1 qubitMap, IM.lookup v2 qubitMap) of
        (Just q1, Just q2) | q1 /= q2 ->
          -- Check if there's a Hadamard edge between them (CZ pattern)
          -- or if they share a common X neighbor (CNOT pattern)
          if hasHadamardEdge d0 v1 v2
          then [CZ q1 q2]  -- Or could be CNOT depending on context
          else []
        _ -> []
    
    hasHadamardEdge :: Diagram -> Vertex -> Vertex -> Bool
    hasHadamardEdge d0 v1 v2 =
      let k = normalizeEdge v1 v2
      in case M.lookup k (_edges d0) of
           Just b -> hadamardCount b > 0
           Nothing -> False
    
    -- Simple topological sort of gates (placeholder for Stage 1)
    sortGates :: [Gate] -> [Gate]
    sortGates = id  -- In full implementation, would sort by dependency


