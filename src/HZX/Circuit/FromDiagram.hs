{-# LANGUAGE TupleSections #-}

module HZX.Circuit.FromDiagram
  ( diagramToCircuit
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (fromMaybe)

import HZX.Core.Diagram
import HZX.Circuit

-- | Extract a quantum circuit from a ZX-diagram.
--
--   Stage 2: reverse converter.  It assumes the diagram has the circuit-like
--   form produced by 'circuitToDiagram' (or a simplification that keeps wires
--   intact): each input boundary traces through a linear wire, interrupted only
--   by single-qubit gates and by simple edges that encode CNOT / CZ.
--
--   Supported gates: H, S, T, ZPhase, XPhase, CNOT, CZ.  SWAP is decomposed
--   into three CNOTs by 'circuitToDiagram' and is not reconstructed as a SWAP
--   gate.  Measurement and reset operations are not recovered as gates.
diagramToCircuit :: Diagram -> Circuit
diagramToCircuit d = Circuit (go verts IS.empty)
  where
    qOf v = IM.lookup v (vertexToQubitMap d)
    verts = sort (filter (not . isBdr d) (allVertices d))
    go [] _ = []
    go (v:vs) seen
      | v `IS.member` seen = go vs seen
      | otherwise =
          case vertexType d v of
            Nothing -> go vs seen
            Just HBox ->
              case qOf v of
                Nothing -> go vs (IS.insert v seen)
                Just q  -> H q : go vs (IS.insert v seen)
            Just (Z p) ->
              case twoQubitPartner d v of
                Nothing ->
                  if degree v d <= 2
                  then case qOf v of
                         Nothing -> go vs (IS.insert v seen)
                         Just q  -> if p == 0
                                    then go vs (IS.insert v seen)
                                    else ZPhase q p : go vs (IS.insert v seen)
                  else go vs (IS.insert v seen)
                Just (w, CNOTControl) ->
                  case (qOf v, qOf w) of
                    (Just qv, Just qw) -> CNOT qv qw : go vs (IS.insert v (IS.insert w seen))
                    _                  -> go vs (IS.insert v seen)
                Just (w, CZPartner) ->
                  case (qOf v, qOf w) of
                    (Just qv, Just qw) -> CZ qv qw : go vs (IS.insert v (IS.insert w seen))
                    _                  -> go vs (IS.insert v seen)
                Just (_, CNOTTarget) ->
                  -- A Z spider is never the target of a CNOT.
                  go vs (IS.insert v seen)
            Just (X p) ->
              case twoQubitPartner d v of
                Nothing ->
                  if degree v d <= 2
                  then case qOf v of
                         Nothing -> go vs (IS.insert v seen)
                         Just q  -> if p == 0
                                    then go vs (IS.insert v seen)
                                    else XPhase q p : go vs (IS.insert v seen)
                  else go vs (IS.insert v seen)
                Just (w, CNOTTarget) ->
                  -- The control spider should have emitted the CNOT already.
                  go vs (IS.insert v (IS.insert w seen))
                Just (_, _) ->
                  go vs (IS.insert v seen)
            Just (Boundary _) -> go vs seen

data TwoQubitRole = CNOTControl | CNOTTarget | CZPartner deriving (Eq, Show)

-- | If @v@ is one endpoint of a two-qubit gate, return its partner and role.
twoQubitPartner :: Diagram -> Vertex -> Maybe (Vertex, TwoQubitRole)
twoQubitPartner d v =
  case vertexType d v of
    Just (Z _) ->
      -- Look for an X(0) neighbor: this is a CNOT target.
      let xNeighbors = [ w | w <- neighbors v d
                          , isX0 d w
                          , isJointEdge d v w ]
      in case xNeighbors of
           (w:_) -> Just (w, CNOTControl)
           _ ->
             -- Otherwise look for another Z(0) neighbor: this is a CZ.
             let zNeighbors = [ w | w <- neighbors v d
                                   , w /= v
                                   , isZ0 d w
                                   , isJointEdge d v w ]
             in case zNeighbors of
                  (w:_) -> Just (w, CZPartner)
                  _     -> Nothing
    Just (X _) ->
      let zNeighbors = [ w | w <- neighbors v d
                          , isZ0 d w
                          , isJointEdge d v w ]
      in case zNeighbors of
           (w:_) -> Just (w, CNOTTarget)
           _     -> Nothing
    _ -> Nothing

isBdr :: Diagram -> Vertex -> Bool
isBdr d v = case vertexType d v of
  Just (Boundary _) -> True
  _                 -> False

isZ0 :: Diagram -> Vertex -> Bool
isZ0 d v = case vertexType d v of
  Just (Z p) -> p == 0
  _          -> False

isX0 :: Diagram -> Vertex -> Bool
isX0 d v = case vertexType d v of
  Just (X p) -> p == 0
  _          -> False

-- | A "joint" simple edge connects two internal spiders that belong to
--   different qubit wires.  In circuit-like diagrams these are exactly the
--   edges that encode CNOT (Z--X) and CZ (Z--Z).
isJointEdge :: Diagram -> Vertex -> Vertex -> Bool
isJointEdge d a b
  | isBdr d a || isBdr d b = False
  | otherwise =
      case M.lookup (normalizeEdge a b) (_edges d) of
        Just bundle -> simpleCount bundle > 0 && degree a d >= 3 && degree b d >= 3
        Nothing     -> False

-- | Map every internal vertex to the qubit index of the wire it lives on.
--   Joint edges are temporarily removed so that each remaining connected
--   component is a single wire.
vertexToQubitMap :: Diagram -> IM.IntMap Int
vertexToQubitMap d = mapping
  where
    inputMap = IM.fromList (zip (inputs d) [0 :: Int ..])
    outputMap = IM.fromList (zip (outputs d) [0 :: Int ..])  -- fallback only

    -- Build adjacency of non-joint simple edges.
    addEdgeAdj a b m = IM.insertWith (++) a [b] $ IM.insertWith (++) b [a] m
    adj0 = M.foldrWithKey addBundle IM.empty (_edges d)
    addBundle (a, b) bundle acc =
      if simpleCount bundle > 0 && not (isJointEdge d a b)
      then addEdgeAdj a b acc
      else acc

    -- Connected components of the reduced graph.
    comps = components adj0

    qubitOfComponent comp =
      let inputsHere  = [ q | v <- comp, Just q <- [IM.lookup v inputMap] ]
          outputsHere = [ q | v <- comp, Just q <- [IM.lookup v outputMap] ]
      in case inputsHere of
           (q:_) -> q
           _     -> case outputsHere of
                      (q:_) -> q
                      _     -> 0

    mapping = IM.fromList
      [ (v, q)
      | comp <- comps
      , let q = qubitOfComponent comp
      , v <- comp
      ]

-- | Connected components of an undirected graph represented as an adjacency map.
components :: IM.IntMap [Vertex] -> [[Vertex]]
components adj = go (IM.keys adj) IS.empty
  where
    go [] _ = []
    go (v:vs) seen
      | v `IS.member` seen = go vs seen
      | otherwise =
          let comp = dfs [v] IS.empty
          in IS.toList comp : go vs (IS.union comp seen)

    dfs [] visited = visited
    dfs (x:xs) visited
      | x `IS.member` visited = dfs xs visited
      | otherwise =
          let nbrs = fromMaybe [] (IM.lookup x adj)
          in dfs (nbrs ++ xs) (IS.insert x visited)
