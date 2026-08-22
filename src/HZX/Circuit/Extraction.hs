{-# LANGUAGE TupleSections #-}

-- | Graph-theoretic circuit extraction for ZX diagrams.
--
--   This module implements an extractor based on the algorithm described in
--   Backens et al., "There and back again: A circuit extraction tale", which
--   is also the foundation of PyZX's @extract_circuit@.  The input diagram is
--   first converted to graph-like form, then spiders are extracted from outputs
--   back to inputs using a frontier, single-qubit clean-up, and GF(2)
--   Gaussian elimination for CNOTs.
module HZX.Circuit.Extraction
  ( extractCircuit
  , toGraphLike
  ) where

import Data.Bits ((.&.), setBit, shiftR, testBit)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

import HZX.Core.Diagram
import HZX.Core.Diagram.Types (EdgeType(..))
import HZX.Circuit
import HZX.Rewrite.Rule (hadamardEdgeSimp, colorChange, spiderFusion)
import qualified HZX.LinAlg.Z2 as Z2

import HZX.Circuit.Extraction.Types
  ( GFlow
  , QubitIndex
  , ExtractionState(..)
  , ExtractionResult(..)
  )
import qualified HZX.Circuit.Extraction.Flow as Flow

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract a quantum circuit from a ZX diagram.
--
--   Returns 'ExtractionResult' indicating success, a recoverable
--   "not extractable" failure, or an unexpected extraction error.
extractCircuit :: Diagram -> ExtractionResult
extractCircuit d0
  | not (isWellFormed d0) =
      ExtractionError "diagram is not well-formed"
  | length (inputs d0) /= length (outputs d0) =
      ExtractionError "differing numbers of inputs and outputs"
  | otherwise =
      let d1 = toGraphLike d0
          mbGFlow = Flow.focusedGFlow d1
      in if hasNoInterior d1
         then Extracted (Circuit []) mbGFlow
         else extractFromGraphLike d1 mbGFlow

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
-- Main extraction loop
-- ---------------------------------------------------------------------------

extractFromGraphLike :: Diagram -> Maybe GFlow -> ExtractionResult
extractFromGraphLike d0 mbGFlow =
  case initExtractionState d0 mbGFlow of
    Left err    -> ExtractionError err
    Right st0   -> extractLoop st0

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
        { esDiagram  = d
        , esFrontier = frontier0
        , esQubitOf  = qubitOf0
        , esGates    = []
        , esGFlow    = mbGFlow
        }

extractLoop :: ExtractionState -> ExtractionResult
extractLoop = go 0
  where
    go n st
      | IM.null (esFrontier st) =
          Extracted (Circuit $ reverse (esGates st)) (esGFlow st)
      | n > 100 =
          NotExtractable "extraction loop exceeded iteration limit"
      | otherwise =
          case cleanFrontier st of
            Left err -> ExtractionError err
            Right st1 ->
              let st2 = removeFrontierInputs st1
              in case neighborsOfFrontier st2 of
                   Nothing -> Extracted (Circuit $ reverse (esGates st2)) (esGFlow st2)
                   Just (neighbors, st3) ->
                     case extractCNOTs neighbors st3 of
                       Left err -> NotExtractable err
                       Right (cnots, st4) ->
                         case applyCNOTs cnots neighbors st4 of
                           Left err    -> NotExtractable err
                           Right st5   -> go (n + 1) st5

-- ---------------------------------------------------------------------------
-- Frontier cleaning: extract single-qubit gates and CZs
-- ---------------------------------------------------------------------------

cleanFrontier :: ExtractionState -> Either String ExtractionState
cleanFrontier st = do
  let d = esDiagram st
      frontierList = IM.toList (esFrontier st)
  -- Extract single-qubit gates (H and ZPhase) for each frontier vertex.
  st1 <- foldM (cleanVertex d) st frontierList
  -- Extract CZ gates between frontier vertices.
  return $ extractCZs st1

cleanVertex :: Diagram -> ExtractionState -> (QubitIndex, Vertex) -> Either String ExtractionState
cleanVertex d st (q, v) = do
  outB <- case [ w | w <- neighbors v d, isOutput w d ] of
            [b] -> Right b
            _   -> Left "frontier vertex is not connected to exactly one output"
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

extractCNOTs :: [Vertex] -> ExtractionState -> Either String ([(QubitIndex, QubitIndex)], ExtractionState)
extractCNOTs neighbors st =
  let d = esDiagram st
      frontierList = IM.toList (esFrontier st)  -- [(qubit, vertex)]
      rows = length frontierList
      cols = length neighbors
      neighborIdx = IM.fromList (zip neighbors [0..])
      frontierIdx = IM.fromList (zip (map snd frontierList) [0..])

      -- Build biadjacency matrix M[q][n] = 1 if frontier[q] connected to neighbor[n].
      mat = Z2.fromLists
        [ [ if areConnected fv n d then 1 else 0 | n <- neighbors ]
          | (_, fv) <- frontierList ]

      -- Find a sequence of row operations that creates at least one row with
      -- a single 1.  Each operation (target, control) means
      -- row_target <- row_target XOR row_control.
      ops = gaussForExtraction mat rows cols

      -- Apply operations to a copy of the matrix so we know which vertices
      -- become extractable.
      mat' = applyRowOps cols ops mat
      extractableRows = [ (q, neighbors !! j)
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

  in if null extractableRows && not (null neighbors)
     then Left "could not create an extractable vertex"
     else Right (qOps, st)

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
      Z2.fromLists [ [ Z2.row m k !? l | l <- [0..cols-1] ]
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

    popcount 0 = 0
    popcount n = fromIntegral (n .&. 1) + popcount (n `shiftR` 1)

    x !? y = if testBit x y then 1 else 0

-- ---------------------------------------------------------------------------
-- Apply CNOTs and advance frontier
-- ---------------------------------------------------------------------------

applyCNOTs :: [(QubitIndex, QubitIndex)] -> [Vertex] -> ExtractionState -> Either String ExtractionState
applyCNOTs cnotOps neighbors st = do
  let d0 = esDiagram st
      frontierList = IM.toList (esFrontier st)
      frontierIdx  = IM.fromList (zip (map snd frontierList) [0..])
      qubitToRow   = IM.fromList (zip (map fst frontierList) [0..])
      rowOf q      = fromMaybe (error "invalid qubit") (IM.lookup q qubitToRow)

      -- Rebuild the biadjacency matrix after applying the recorded CNOTs.
      mat = applyRowOps (length neighbors) (map (\(t, c) -> (rowOf t, rowOf c)) cnotOps)
            $ Z2.fromLists
                [ [ if areConnected fv n d0 then 1 else 0 | n <- neighbors ]
                  | (_, fv) <- frontierList ]

      -- Determine which frontier vertices can now be advanced.
      advances = [ (q, fv, n)
                 | (q, fv) <- frontierList
                 , let row = Z2.row mat (frontierIdx IM.! fv)
                 , let js = [ j | j <- [0..length neighbors - 1], testBit row j ]
                 , length js == 1
                 , let [j] = js
                 , let n = neighbors !! j ]

  if null advances
    then Left "no vertices could be advanced after CNOT extraction"
    else do
      let st1 = foldl' addCNOTGate st cnotOps
      foldM advanceFrontier st1 advances

addCNOTGate :: ExtractionState -> (QubitIndex, QubitIndex) -> ExtractionState
addCNOTGate st (target, control) = addGate (CNOT control target) st

advanceFrontier :: ExtractionState -> (QubitIndex, Vertex, Vertex) -> Either String ExtractionState
advanceFrontier st (q, oldFv, newFv) = do
  let d = esDiagram st
  outB <- case [ w | w <- neighbors oldFv d, isOutput w d ] of
            [b] -> Right b
            _   -> Left "frontier vertex lost its output boundary"
  let d1 = removeVertex oldFv d
      d2 = addEdge newFv outB Simple d1
      frontier' = IM.insert q newFv (esFrontier st)
      qubitOf'  = IM.insert newFv q (esQubitOf st)
      -- Add a Hadamard when the new frontier vertex was connected to the old
      -- one by a Hadamard edge.
      st1 = if hasHadamardEdge oldFv newFv d
            then addGate (H q) st { esDiagram = d2, esFrontier = frontier', esQubitOf = qubitOf' }
            else st { esDiagram = d2, esFrontier = frontier', esQubitOf = qubitOf' }
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

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x:xs) = f acc x >>= \acc' -> foldM f acc' xs
