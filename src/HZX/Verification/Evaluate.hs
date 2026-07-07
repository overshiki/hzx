-- | Reference evaluation of small concrete ZX diagrams as complex matrices.
module HZX.Verification.Evaluate
  ( evalDiagram
  , diagramsEquivalent
  ) where

import Data.Complex (Complex(..), magnitude)
import Data.List (find)
import HZX.Core.Scalar (scalarToComplex)
import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram
import HZX.Core.Diagram.Types (VertexType(..), EdgeType(..), EdgeBundle(..))
import HZX.Verification.Tensor

type Matrix = [[Complex Double]]

-- | Evaluate a concrete 'Diagram' as a matrix mapping input basis states to
--   output basis states.  Only square diagrams (same number of inputs and
--   outputs) are supported.  The evaluator is exponential in the number of
--   qubits and is intended for small test cases only.
evalDiagram :: Diagram -> Either String Matrix
evalDiagram d =
  let ins  = inputs d
      outs = outputs d
  in if length ins /= length outs
     then Left "evalDiagram: rectangular diagrams are not supported"
     else evalDiagramWith ins outs d

-- | Compare two diagrams up to a global scalar and a numerical tolerance.
diagramsEquivalent :: Double -> Diagram -> Diagram -> Bool
diagramsEquivalent tol d1 d2 =
  case (evalDiagram d1, evalDiagram d2) of
    (Right m1, Right m2) -> matricesEquivalent tol m1 m2
    _ -> False

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

evalDiagramWith :: [Vertex] -> [Vertex] -> Diagram -> Either String Matrix
evalDiagramWith ins outs d
  | null internalVertices = evaluateBoundaryOnlyDiagram ins outs d
  where
    internalVertices = filter (\v -> not (v `elem` ins || v `elem` outs)) (allVertices d)
evalDiagramWith ins outs d = do
  let internals = filter (\v -> not (v `elem` ins || v `elem` outs))
                        (allVertices d)
      (vLabels, edgeInfo) = buildEdgeInfo d
  intTensors <- buildInternalTensors d internals vLabels
  -- Apply Hadamards for boundary-internal edges.
  let afterBoundaryH = applyBoundaryHadamards edgeInfo ins outs intTensors
  -- Contract all internal edges.
  let contracted0 = contractInternal edgeInfo afterBoundaryH
  -- Add identity / Hadamard tensors for wires that have no internal vertex.
  boundaryTensors <- boundaryOnlyTensors d vLabels edgeInfo ins outs contracted0
  let contracted = contracted0 ++ boundaryTensors
      combined = combineTensors contracted
  inLabels  <- mapM (boundaryLabel vLabels) ins
  outLabels <- mapM (boundaryLabel vLabels) outs
  let matrix = reshapeToMatrix inLabels outLabels combined
  return $ scaleMatrix (scalarToComplex (scalar d)) matrix

-- | Tensors for wires that have no internal spider (e.g. an unused qubit
--   or a wire that was simplified away).
boundaryOnlyTensors :: Diagram -> IM.IntMap [EdgeLabel] -> M.Map EdgeLabel (Vertex, Vertex, EdgeType)
                    -> [Vertex] -> [Vertex] -> [LabeledTensor] -> Either String [LabeledTensor]
boundaryOnlyTensors _ vLabels edgeInfo ins outs contracted =
  let present = concatMap ltLabels contracted
      missing l = l `notElem` present
      go [] [] acc = Right (reverse acc)
      go (iv:ivs) (ov:ovs) acc = do
        li <- singleLabel iv
        lo <- singleLabel ov
        if missing li && missing lo
          then case M.lookup li edgeInfo of
                 Nothing -> Left "evalDiagram: missing edge info for boundary wire"
                 Just (_, _, et) -> go ivs ovs (boundaryWireTensor li lo et : acc)
          else go ivs ovs acc
      go _ _ _ = Left "evalDiagram: mismatched input/output counts"

      singleLabel v =
        case IM.lookup v vLabels of
          Just [l] -> Right l
          _        -> Left "evalDiagram: boundary must have exactly one incident edge"
  in go ins outs []

boundaryWireTensor :: EdgeLabel -> EdgeLabel -> EdgeType -> LabeledTensor
boundaryWireTensor lIn lOut Simple = LabeledTensor [2, 2] [lIn, lOut] [1, 0, 0, 1]
boundaryWireTensor lIn lOut Hadamard = hBox [lIn, lOut]

-- | Evaluate a diagram that contains only boundary vertices and edges
--   connecting them (e.g. an identity circuit with possible Hadamard wires).
evaluateBoundaryOnlyDiagram :: [Vertex] -> [Vertex] -> Diagram -> Either String Matrix
evaluateBoundaryOnlyDiagram ins outs d = do
  perWire <- mapM (wireMatrix d outs) ins
  let n = length ins
      bitstrings 0 = [[]]
      bitstrings m = [ b : bs | b <- [0, 1], bs <- bitstrings (m - 1) ]
      matrix =
        [ [ product [ (perWire !! q) !! (inputBit * 2 + outputBit)
                    | (q, inputBit, outputBit) <- zip3 [0 .. n - 1] inBits outBits ]
          | outBits <- bitstrings n ]
        | inBits <- bitstrings n ]
  return $ scaleMatrix (scalarToComplex (scalar d)) matrix
  where
    wireMatrix :: Diagram -> [Vertex] -> Vertex -> Either String [Complex Double]
    wireMatrix d0 outs0 inV =
      case neighbors inV d0 of
        [outV] | outV `elem` outs0 ->
          if hasHadamardEdge inV outV d0
          then Right [h, h, h, -h]   -- [in0out0, in0out1, in1out0, in1out1]
          else Right [1, 0, 0, 1]
        _ -> Left "evalDiagram: boundary-only diagram with non-trivial wiring"
    h = 1 / sqrt 2

-- | Build a map from vertex to incident edge-label lists, and a map from
--   each label to its endpoints and edge type.
buildEdgeInfo :: Diagram -> (IM.IntMap [EdgeLabel], M.Map EdgeLabel (Vertex, Vertex, EdgeType))
buildEdgeInfo d =
  let ins  = inputs d
      outs = outputs d
      (vLabels, info, _) = M.foldlWithKey (go ins outs) (IM.empty, M.empty, 0) (_edges d)
  in (vLabels, info)
  where
    go ins outs (vLabels, info, next) (a, b) bundle =
      let (v1, i1, n1) = addOccs ins outs a b Simple (simpleCount bundle) vLabels info next
          (v2, i2, n2) = addOccs ins outs a b Hadamard (hadamardCount bundle) v1 i1 n1
      in (v2, i2, n2)

    addOccs _ _ _ _ _ 0 vLabels info next = (vLabels, info, next)
    addOccs ins outs a b et n vLabels info next
      | a == b =
          let vLabels' = IM.insertWith (++) a [next, next] vLabels
              info'    = M.insert next (a, b, et) info
          in addOccs ins outs a b et (n - 1) vLabels' info' (next + 1)
      | isBdr ins a && isBdr outs b =
          let vLabels' = IM.insertWith (++) a [next] $ IM.insertWith (++) b [next + 1] vLabels
              info'    = M.insert next (a, b, et) $ M.insert (next + 1) (a, b, et) info
          in addOccs ins outs a b et (n - 1) vLabels' info' (next + 2)
      | isBdr ins b && isBdr outs a =
          let vLabels' = IM.insertWith (++) a [next + 1] $ IM.insertWith (++) b [next] vLabels
              info'    = M.insert next (a, b, et) $ M.insert (next + 1) (a, b, et) info
          in addOccs ins outs a b et (n - 1) vLabels' info' (next + 2)
      | otherwise =
          let vLabels' = IM.insertWith (++) a [next] $ IM.insertWith (++) b [next] vLabels
              info'    = M.insert next (a, b, et) info
          in addOccs ins outs a b et (n - 1) vLabels' info' (next + 1)

    isBdr xs v = v `elem` xs

buildInternalTensors :: Diagram -> [Vertex] -> IM.IntMap [EdgeLabel]
                     -> Either String (IM.IntMap LabeledTensor)
buildInternalTensors d internals vLabels =
  IM.fromList <$> mapM (\v -> fmap (\t -> (v, t)) (buildTensor v)) internals
  where
    buildTensor v =
      case vertexType d v of
        Nothing -> Left $ "buildInternalTensors: missing vertex " ++ show v
        Just (Z p) -> Right $ buildZ v p vLabels
        Just (X p) -> Right $ buildX v p vLabels
        Just HBox  ->
          let ls = IM.findWithDefault [] v vLabels
          in if length ls == 2
             then Right $ hBox ls
             else Left "evalDiagram: HBox must have degree 2"
        Just (Boundary _) -> Left "evalDiagram: boundary treated as internal"

buildZ :: Vertex -> Rational -> IM.IntMap [EdgeLabel] -> LabeledTensor
buildZ v p labels =
  let ls = IM.findWithDefault [] v labels
  in zSpider (length ls) p ls

buildX :: Vertex -> Rational -> IM.IntMap [EdgeLabel] -> LabeledTensor
buildX v p labels =
  let z = buildZ v p labels
      ls = ltLabels z
  in foldl applyH z ls

applyBoundaryHadamards :: M.Map EdgeLabel (Vertex, Vertex, EdgeType)
                       -> [Vertex] -> [Vertex]
                       -> IM.IntMap LabeledTensor -> IM.IntMap LabeledTensor
applyBoundaryHadamards edgeInfo ins outs tensors =
  M.foldlWithKey go tensors edgeInfo
  where
    go ts l (_, _, Simple) = ts
    go ts l (a, b, Hadamard)
      | a `elem` ins || a `elem` outs = applyToVertex ts b l
      | b `elem` ins || b `elem` outs = applyToVertex ts a l
      | otherwise = ts

    applyToVertex ts v l =
      case IM.lookup v ts of
        Nothing -> ts
        Just t  -> IM.insert v (applyH t l) ts

contractInternal :: M.Map EdgeLabel (Vertex, Vertex, EdgeType) -> IM.IntMap LabeledTensor -> [LabeledTensor]
contractInternal edgeInfo tensors0 = go (IM.elems tensors0)
  where
    go ts = case findRepeatedLabel ts of
        Nothing -> ts
        Just l ->
          let Just (_, _, et) = M.lookup l edgeInfo
              occs = occurrences l ts
          in case occs of
               [(t1, _), (t2, _)] | t1 == t2 ->
                 let t' = if et == Hadamard then traceAxesH t1 l else traceAxes t1 l
                 in go (t' : deleteOne t1 ts)
               [(t1, _), (t2, _)] ->
                 let t1' = if et == Hadamard then applyH t1 l else t1
                     t12 = contract t1' l t2 l
                 in go (t12 : deleteOne t2 (deleteOne t1 ts))
               _ -> error "contractInternal: unexpected label occurrence count"

    occurrences l ts =
      [ (t, i) | t <- ts, (i, l') <- zip [0..] (ltLabels t), l == l' ]

    deleteOne _ [] = []
    deleteOne x (y:ys) | x == y    = ys
                       | otherwise = y : deleteOne x ys

findRepeatedLabel :: [LabeledTensor] -> Maybe EdgeLabel
findRepeatedLabel ts =
  let counts = foldl (\m t -> foldl (\m' l -> M.insertWith (+) l (1 :: Int) m') m (ltLabels t)) M.empty ts
  in fmap fst $ find (\(_, c) -> c >= 2) (M.toList counts)

combineTensors :: [LabeledTensor] -> LabeledTensor
combineTensors []     = LabeledTensor [] [] [1]
combineTensors [t]    = t
combineTensors (t:ts) = kronecker t (combineTensors ts)

boundaryLabel :: IM.IntMap [EdgeLabel] -> Vertex -> Either String EdgeLabel
boundaryLabel vLabels v =
  case IM.lookup v vLabels of
    Just [l] -> Right l
    _        -> Left "evalDiagram: boundary must have exactly one incident edge"

matricesEquivalent :: Double -> Matrix -> Matrix -> Bool
matricesEquivalent tol m1 m2 =
  let rows1 = length m1
      cols1 = if null m1 then 0 else length (head m1)
      rows2 = length m2
      cols2 = if null m2 then 0 else length (head m2)
      ok = rows1 == rows2 && cols1 == cols2 &&
           case findNonZero m1 of
             Nothing -> all (all (\x -> magnitude x <= tol)) m2
             Just (r, c) ->
               let scale = (m2 !! r !! c) / (m1 !! r !! c)
               in all (\(row1, row2) ->
                         all (\(a, b) -> magnitude (b - scale * a) <= tol)
                             (zip row1 row2))
                      (zip m1 m2)
  in ok

findNonZero :: Matrix -> Maybe (Int, Int)
findNonZero m =
  let indexed = [ (r, c, x) | (r, row) <- zip [0..] m, (c, x) <- zip [0..] row ]
  in case filter (\(_, _, x) -> magnitude x > 1e-12) indexed of
       ((r, c, _):_) -> Just (r, c)
       _             -> Nothing

scaleMatrix :: Complex Double -> Matrix -> Matrix
scaleMatrix s = map (map (* s))
