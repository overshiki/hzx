{-# LANGUAGE TupleSections #-}

-- | Graph-theoretic circuit extraction for ZX diagrams.
--
--   This module implements two extraction algorithms for graph-like diagrams:
--
--   1. 'FrontierMode' (default): a frontier-based extractor that uses GF(2)
--      Gaussian elimination to find CNOTs.  This is the algorithm used by PyZX
--      and is described in Backens et al., "There and back again: A circuit
--      extraction tale".
--
--   2. 'GFlowMode': an educational extractor that follows the focused
--      generalized flow directly, applying the precomputed correction sets as
--      CNOTs.
module HZX.Circuit.Extraction
  ( extractCircuit
  , extractCircuitWith
  , toGraphLike
  , ExtractionConfig(..)
  , ExtractionMode(..)
  , defaultExtractionConfig
  ) where

import Control.Monad (foldM, when)
import Data.Bits ((.&.), shiftR, testBit)
import Data.List (foldl', nub, sortBy)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

import HZX.Core.Diagram
import HZX.Circuit
import HZX.Rewrite.Rule (hadamardEdgeSimp, colorChange, spiderFusion)
import qualified HZX.LinAlg.Z2 as Z2

import HZX.Circuit.Extraction.Config
import HZX.Circuit.Extraction.Types
  ( GFlow(..)
  , QubitIndex
  , ExtractionState(..)
  , ExtractionResult(..)
  )
import qualified HZX.Circuit.Extraction.Flow as Flow

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract a quantum circuit from a ZX diagram using the default
--   frontier-based strategy.
extractCircuit :: Diagram -> ExtractionResult
extractCircuit = extractCircuitWith defaultExtractionConfig

-- | Extract a quantum circuit from a ZX diagram using the supplied extraction
--   configuration.
extractCircuitWith :: ExtractionConfig -> Diagram -> ExtractionResult
extractCircuitWith cfg d0
  | not (isWellFormed d0) =
      ExtractionError "diagram is not well-formed"
  | length (inputs d0) /= length (outputs d0) =
      ExtractionError "differing numbers of inputs and outputs"
  | otherwise =
      let d1 = toGraphLike d0
      in if hasNoInterior d1
         then Extracted (Circuit []) Nothing
         else case ecMode cfg of
                FrontierMode -> runFrontierExtraction cfg d1
                GFlowMode    -> runGFlowExtraction cfg d1

-- ---------------------------------------------------------------------------
-- Mode dispatch
-- ---------------------------------------------------------------------------

runFrontierExtraction :: ExtractionConfig -> Diagram -> ExtractionResult
runFrontierExtraction cfg d1 =
  case initExtractionState d1 Nothing of
    Left err  -> ExtractionError err
    Right st0 ->
      case runExtractM cfg (extractLoop st0) of
        Left err  -> NotExtractable err
        Right res -> res

runGFlowExtraction :: ExtractionConfig -> Diagram -> ExtractionResult
runGFlowExtraction cfg d1 =
  case Flow.focusedGFlow d1 of
    Nothing -> NotExtractable "diagram has no focused gflow"
    Just gflo ->
      case initExtractionState d1 (Just gflo) of
        Left err  -> ExtractionError err
        Right st0 ->
          case runExtractM cfg (extractByGFlow st0) of
            Left err  -> NotExtractable err
            Right res -> res

-- ---------------------------------------------------------------------------
-- Graph-like conversion
-- ---------------------------------------------------------------------------

-- | Convert a diagram to graph-like form: all interior spiders are Z-spiders
--   and all interior edges are Hadamard edges.  The conversion iterates the
--   constituent rules to a fixpoint.
toGraphLike :: Diagram -> Diagram
toGraphLike d =
  let d1 = runRule hadamardEdgeSimp d
      d2 = colorChangeAll d1
      d3 = spiderFusionAll d2
  in if d3 == d then d3 else toGraphLike d3
  where
    runRule r d0 = maybe d0 (runRule r) (r d0)

    colorChangeAll d0 = case colorChange d0 of
      Nothing  -> d0
      Just d1  -> colorChangeAll d1

    spiderFusionAll d0 = case spiderFusion d0 of
      Nothing  -> d0
      Just d1  -> spiderFusionAll d1

-- | Return True if the diagram has no interior spiders.
hasNoInterior :: Diagram -> Bool
hasNoInterior d = all (isBoundaryVertex d) (allVertices d)
  where
    isBoundaryVertex d0 v = case lookupVertex v d0 of
                              Just (Boundary _) -> True
                              _                 -> False

-- ---------------------------------------------------------------------------
-- Extraction state initialization
-- ---------------------------------------------------------------------------

initExtractionState :: Diagram -> Maybe GFlow -> Either String ExtractionState
initExtractionState d mbGFlow = do
  let outs = outputs d
  if null outs
    then Left "no outputs"
    else do
      let buildFrontier (q, o) =
            case [ v | v <- neighbors o d, not (isOutput v d) ] of
              [v] | not (isInput v d) -> Just (q, v)
              _                       -> Nothing
          frontierPairs = mapMaybe buildFrontier (zip [0..] outs)
          frontier0 = IM.fromList frontierPairs
          qubitOf0  = IM.fromList [ (v, q) | (q, v) <- frontierPairs ]
      return $ ExtractionState
        { esDiagram       = d
        , esFrontier      = frontier0
        , esQubitOf       = qubitOf0
        , esVertexToQubit = qubitOf0
        , esGates         = []
        , esGFlow         = mbGFlow
        }

-- ---------------------------------------------------------------------------
-- Frontier-based extraction loop
-- ---------------------------------------------------------------------------

extractLoop :: ExtractionState -> ExtractM ExtractionResult
extractLoop = go 0
  where
    go n st = do
      maxIter <- askMaxIterations
      case () of
        _ | IM.null (esFrontier st) ->
              return $ Extracted (Circuit $ reverse (esGates st)) (esGFlow st)
          | n > maxIter ->
              throwExtract "extraction loop exceeded iteration limit"
          | otherwise -> do
              st1 <- cleanFrontier st
              let st2 = removeFrontierInputs st1
              case neighborsOfFrontier st2 of
                Nothing ->
                  return $ Extracted (Circuit $ reverse (esGates st2)) (esGFlow st2)
                Just (nbrs, st3) -> do
                  (cnots, st4) <- extractCNOTs nbrs st3
                  st5 <- applyCNOTs cnots nbrs st4
                  go (n + 1) st5

-- ---------------------------------------------------------------------------
-- GFlow-driven extraction loop
-- ---------------------------------------------------------------------------

-- | Extract by following the focused generalized flow.  The gflow gives an
--   extraction order and, for each vertex, a correction set.  Applying CNOTs
--   from the qubits of the correction vertices to one chosen target qubit
--   makes the target frontier vertex connected only to the vertex being
--   extracted, so the frontier can be advanced.
extractByGFlow :: ExtractionState -> ExtractM ExtractionResult
extractByGFlow st0 = do
  let order = gOrder $ fromMaybe (error "extractByGFlow: missing gflow")
                                 (esGFlow st0)
      -- Process vertices from inputs back toward outputs (decreasing gflow
      -- order).  When we reach a vertex, all of its correction vertices
      -- (which have smaller order numbers) are still on the frontier.
      worklist = sortBy (flip (comparing snd)) $
                 filter (isInterior (esDiagram st0) . fst) (IM.toList order)
  go 0 worklist st0
  where
    isInterior d v = case lookupVertex v d of
                       Just (Boundary _) -> False
                       _                 -> True

    go _ [] st =
      return $ Extracted (Circuit $ reverse (esGates st)) (esGFlow st)
    go n ((v, _):vs) st = do
      maxIter <- askMaxIterations
      when (n > maxIter) $
        throwExtract "gflow extraction exceeded iteration limit"
      st1 <- cleanFrontier st
      -- Skip vertices that have already been removed (e.g. because they were
      -- an earlier frontier vertex) or that are already on the frontier.
      st2 <- case IM.lookup v (esQubitOf st1) of
               Just _  -> return st1
               Nothing -> extractGFlowVertex v corrections st1
      go (n + 1) vs st2
      where
        corrections = gCorrections $ fromMaybe (error "extractByGFlow: missing gflow")
                                               (esGFlow st0)

extractGFlowVertex :: Vertex -> IM.IntMap (M.Map Vertex ()) -> ExtractionState
                   -> ExtractM ExtractionState
extractGFlowVertex v corrections st = do
  let corrMap = fromMaybe M.empty (IM.lookup v corrections)
      corrVs  = M.keys corrMap
      d       = esDiagram st
      nonBoundary u = case lookupVertex u d of
                        Just (Boundary _) -> False
                        _                 -> True
      -- Correction vertices that carry a qubit line (exclude output/input
      -- boundaries and the vertex being extracted itself).
      corrInteriors = filter (\u -> nonBoundary u && u /= v) corrVs
      corrQubits = nub [ q | u <- corrInteriors
                           , Just q <- [IM.lookup u (esVertexToQubit st)] ]

  -- Find a target frontier vertex that is both in the correction set and
  -- adjacent to v.  The correction set guarantees that such a vertex exists
  -- whenever v is not directly extractable.
  let targetCandidates =
        [ (q, w) | q <- corrQubits
                 , Just w <- [IM.lookup q (esFrontier st)]
                 , areConnected v w d ]
      fallbackCandidates =
        [ (q, w) | w <- neighbors v d
                 , Just q <- [IM.lookup w (esQubitOf st)] ]

  (qw, w) <- case (targetCandidates, fallbackCandidates) of
               ((qw_, w_):_, _) -> return (qw_, w_)
               ([], (qw_, w_):_) -> return (qw_, w_)
               ([], []) -> throwExtract "gflow vertex has no extractable frontier neighbour"

  -- CNOTs from the remaining correction qubits to the target qubit.
  let cnots = [ (qw, qc) | qc <- corrQubits, qc /= qw ]
      st1   = foldl' addCNOTGate st cnots
  advanceFrontier st1 (qw, w, v)

-- ---------------------------------------------------------------------------
-- Frontier cleaning: extract single-qubit gates and CZs
-- ---------------------------------------------------------------------------

cleanFrontier :: ExtractionState -> ExtractM ExtractionState
cleanFrontier st = do
  let d = esDiagram st
      frontierList = IM.toList (esFrontier st)
  -- Extract single-qubit gates (H and ZPhase) for each frontier vertex.
  st1 <- foldM (cleanVertex d) st frontierList
  -- Extract CZ gates between frontier vertices.
  return $ extractCZs st1

cleanVertex :: Diagram -> ExtractionState -> (QubitIndex, Vertex) -> ExtractM ExtractionState
cleanVertex d st (q, v) = do
  outB <- case [ w | w <- neighbors v d, isOutput w d ] of
            [b] -> return b
            _   -> throwExtract "frontier vertex is not connected to exactly one output"
  let st1 = if hasHadamardEdge v outB d
            then addGate (H q) $ toggleEdge v outB st
            else st
      phase = case lookupVertex v (esDiagram st1) of
                Just (Z p) -> p
                _          -> 0
      st2 = if phase /= 0
            then addGate (ZPhase q phase) $ setPhaseZero v st1
            else st1
  return st2

extractCZs :: ExtractionState -> ExtractionState
extractCZs st =
  let d = esDiagram st
      frontierVerts = IM.elems (esFrontier st)
      pairs = [ (v, w) | v <- frontierVerts, w <- frontierVerts, v < w
                       , hasHadamardEdge v w d ]
      st1 = foldl' addCZ st pairs
  in st1
  where
    addCZ s (v, w) =
      let qv = esQubitOf s IM.! v
          qw = esQubitOf s IM.! w
          d' = removeEdge v w Hadamard (esDiagram s)
      in addGate (CZ qv qw) s { esDiagram = d' }

-- ---------------------------------------------------------------------------
-- Frontier advancement
-- ---------------------------------------------------------------------------

-- | Remove frontier vertices that are input boundaries themselves.
--   Such qubits are fully extracted.
removeFrontierInputs :: ExtractionState -> ExtractionState
removeFrontierInputs st =
  let d = esDiagram st
      toKeep = IM.filter (\v -> not (isInput v d)) (esFrontier st)
  in st { esFrontier = toKeep }

-- | Find interior vertices adjacent to the current frontier.  Returns Nothing
--   when no such vertices exist (all frontiers reached inputs).
neighborsOfFrontier :: ExtractionState -> Maybe ([Vertex], ExtractionState)
neighborsOfFrontier st
  | IM.null (esFrontier st) = Nothing
  | otherwise =
      let d = esDiagram st
          frontierVerts = IM.elems (esFrontier st)
          nbs = S.toList $ S.fromList $
                [ w | v <- frontierVerts, w <- neighbors v d
                    , not (isOutput w d)
                    , not (v == w)
                    , not (w `elem` frontierVerts) ]
      in if null nbs
         then Nothing
         else Just (nbs, st)

-- ---------------------------------------------------------------------------
-- CNOT extraction via GF(2) Gaussian elimination
-- ---------------------------------------------------------------------------

extractCNOTs :: [Vertex] -> ExtractionState -> ExtractM ([(QubitIndex, QubitIndex)], ExtractionState)
extractCNOTs nbrs st = do
  let d = esDiagram st
      frontierList = IM.toList (esFrontier st)  -- [(qubit, vertex)]
      rows = length frontierList
      cols = length nbrs
      frontierIdx = IM.fromList (zip (map snd frontierList) [0..])

      -- Build biadjacency matrix M[q][n] = 1 if frontier[q] connected to neighbor[n].
      mat = Z2.fromLists
        [ [ if areConnected fv n d then 1 else 0 | n <- nbrs ]
          | (_, fv) <- frontierList ]

      -- Find a sequence of row operations that creates at least one row with
      -- a single 1.  Each operation (target, control) means
      -- row_target <- row_target XOR row_control.
      ops = gaussForExtraction mat rows cols

      -- Apply operations to a copy of the matrix so we know which vertices
      -- become extractable.
      mat' = applyRowOps cols ops mat
      extractableRows = [ (q, nbrs !! j)
                        | (q, fv) <- frontierList
                        , let row = Z2.row mat' (frontierIdx IM.! fv)
                        , let js = [ j | j <- [0..cols-1], testBit row j ]
                        , length js == 1
                        , let [j] = js ]

      -- Re-derive the operations in terms of logical qubits.
      qOps = [ (targetQ, controlQ)
             | (targetRow, controlRow) <- ops
             , let targetQ  = fst (frontierList !! targetRow)
                   controlQ = fst (frontierList !! controlRow) ]

  if null extractableRows && not (null nbrs)
     then throwExtract "could not create an extractable vertex"
     else return (qOps, st)

-- | Apply a list of row operations to a matrix.
applyRowOps :: Int -> [(Int, Int)] -> Z2.Matrix -> Z2.Matrix
applyRowOps cols ops mat = foldl' applyRowOp mat ops
  where
    applyRowOp m (target, control) =
      let rT = Z2.row m target
          rC = Z2.row m control
      in Z2.fromLists [ [ if i == target
                          then if testBit rT j /= testBit rC j then 1 else 0
                          else if testBit (Z2.row m i) j then 1 else 0
                        | j <- [0..cols-1] ]
                      | i <- [0..Z2.nRows m - 1] ]

-- | Perform Gaussian elimination until at least one row has a single 1.
--   Returns the row operations (target, control) used.
gaussForExtraction :: Z2.Matrix -> Int -> Int -> [(Int, Int)]
gaussForExtraction mat0 rows cols = go mat0 0 0 []
  where
    go m r c ops
      | r >= rows || c >= cols = ops
      | otherwise =
          case findPivot m r c of
            Nothing -> go m r (c + 1) ops
            Just pRow ->
              let m1 = if pRow == r then m else swapRows m pRow r
                  ops1 = if pRow == r then ops else (r, pRow) : ops
                  (m2, ops2) = eliminateBelow m1 r c ops1
              in if any (\i -> popcount (Z2.row m2 i) == 1) [0..rows-1]
                 then ops2
                 else go m2 (r + 1) (c + 1) ops2

    findPivot m r c = listToMaybe [ i | i <- [r..rows-1], Z2.row m i /= 0, testBit (Z2.row m i) c ]

    swapRows m i j =
      Z2.fromLists [ [ rowK !? l | l <- [0..cols-1] ]
                   | k <- [0..rows-1]
                   , let rowK | k == i = Z2.row m j
                              | k == j = Z2.row m i
                              | otherwise = Z2.row m k ]

    eliminateBelow m r c ops =
      foldl' (\(mAcc, opsAcc) i ->
                if i == r then (mAcc, opsAcc)
                else if testBit (Z2.row mAcc i) c
                     then let m' = rowAdd mAcc i r
                          in (m', (i, r) : opsAcc)
                     else (mAcc, opsAcc))
             (m, ops) [0..rows-1]

    rowAdd m target source =
      let rT = Z2.row m target
          rS = Z2.row m source
      in Z2.fromLists [ [ if i == target
                          then if testBit rT j /= testBit rS j then 1 else 0
                          else if testBit (Z2.row m i) j then 1 else 0
                        | j <- [0..cols-1] ]
                      | i <- [0..rows-1] ]

    popcount :: Integer -> Int
    popcount 0 = 0
    popcount n = fromIntegral (n .&. 1) + popcount (n `shiftR` 1)

    x !? y = if testBit x y then 1 else 0

-- ---------------------------------------------------------------------------
-- Apply CNOTs and advance frontier
-- ---------------------------------------------------------------------------

applyCNOTs :: [(QubitIndex, QubitIndex)] -> [Vertex] -> ExtractionState -> ExtractM ExtractionState
applyCNOTs cnotOps nbrs st = do
  let d0 = esDiagram st
      frontierList = IM.toList (esFrontier st)
      frontierIdx  = IM.fromList (zip (map snd frontierList) [0..])
      qubitToRow   = IM.fromList (zip (map fst frontierList) [0..])
      rowOf q      = fromMaybe (error "invalid qubit") (IM.lookup q qubitToRow)

      -- Rebuild the biadjacency matrix after applying the recorded CNOTs.
      mat = applyRowOps (length nbrs) (map (\(t, c) -> (rowOf t, rowOf c)) cnotOps)
            $ Z2.fromLists
                [ [ if areConnected fv n d0 then 1 else 0 | n <- nbrs ]
                  | (_, fv) <- frontierList ]

      -- Determine which frontier vertices can now be advanced.
      advances = [ (q, fv, n)
                 | (q, fv) <- frontierList
                 , let row = Z2.row mat (frontierIdx IM.! fv)
                 , let js = [ j | j <- [0..length nbrs - 1], testBit row j ]
                 , length js == 1
                 , let [j] = js
                 , let n = nbrs !! j ]

  if null advances
    then throwExtract "no vertices could be advanced after CNOT extraction"
    else do
      let st1 = foldl' addCNOTGate st cnotOps
      foldM advanceFrontier st1 advances

addCNOTGate :: ExtractionState -> (QubitIndex, QubitIndex) -> ExtractionState
addCNOTGate st (target, control) = addGate (CNOT control target) st

advanceFrontier :: ExtractionState -> (QubitIndex, Vertex, Vertex) -> ExtractM ExtractionState
advanceFrontier st (q, oldFv, newFv) = do
  let d = esDiagram st
  outB <- case [ w | w <- neighbors oldFv d, isOutput w d ] of
            [b] -> return b
            _   -> throwExtract "frontier vertex lost its output boundary"
  let d1 = removeVertex oldFv d
      d2 = addEdge newFv outB Simple d1
      frontier' = IM.insert q newFv (esFrontier st)
      qubitOf'  = IM.insert newFv q (esQubitOf st)
      vertexToQubit' = IM.insert newFv q (esVertexToQubit st)
      -- Add a Hadamard when the new frontier vertex was connected to the old
      -- one by a Hadamard edge.
      st1 = if hasHadamardEdge oldFv newFv d
            then addGate (H q) st { esDiagram       = d2
                                  , esFrontier      = frontier'
                                  , esQubitOf       = qubitOf'
                                  , esVertexToQubit = vertexToQubit'
                                  }
            else st { esDiagram       = d2
                    , esFrontier      = frontier'
                    , esQubitOf       = qubitOf'
                    , esVertexToQubit = vertexToQubit'
                    }
  return st1

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

addGate :: Gate -> ExtractionState -> ExtractionState
addGate g st = st { esGates = esGates st ++ [g] }

isInput :: Vertex -> Diagram -> Bool
isInput v d = v `elem` inputs d

isOutput :: Vertex -> Diagram -> Bool
isOutput v d = v `elem` outputs d

toggleEdge :: Vertex -> Vertex -> ExtractionState -> ExtractionState
toggleEdge v w st =
  let d = esDiagram st
      et = if hasHadamardEdge v w d then Simple else Hadamard
      d' = removeEdge v w (if et == Simple then Hadamard else Simple) d
      d'' = addEdge v w et d'
  in st { esDiagram = d'' }

setPhaseZero :: Vertex -> ExtractionState -> ExtractionState
setPhaseZero v st =
  let d = esDiagram st
      d' = adjustVertex v (\ty -> case ty of Z _ -> Z 0; t -> t) d
  in st { esDiagram = d' }
