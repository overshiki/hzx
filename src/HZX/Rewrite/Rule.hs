module HZX.Rewrite.Rule
  ( Rule
  , spiderFusion
  , identityRemoval
  , hadamardEdgeSimp
  , colorChange
  , hopfRule
  , selfLoopRemoval
  , piCommutation
  , stateCopy
  , piCopy
  , bialgebraSimp
  , localComplementation
  , pivot
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase
import HZX.Core.Scalar
import HZX.Rewrite.Search

type Rule = Diagram -> Maybe Diagram

-- | Remove a degree-2 H-box and replace it with a Hadamard edge.
hadamardEdgeSimp :: Rule
hadamardEdgeSimp = vertexRule tryVertex
  where
    tryVertex v HBox d = do
      nb <- IM.lookup v (_neighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, _), (b, _)] | a /= b ->
          let d1 = removeVertex v d
              d2 = addEdge a b Hadamard d1
          in Just d2
        _ -> Nothing
    tryVertex _ _ _ = Nothing

-- | Fuse adjacent spiders of the same colour.
spiderFusion :: Rule
spiderFusion = pairRule tryPair
  where
    tryPair v1 v2 d = do
      t1 <- IM.lookup v1 (_vertices d)
      t2 <- IM.lookup v2 (_vertices d)
      case (t1, t2) of
        (Z p1, Z p2) -> fuse v1 v2 p1 p2 Z d
        (X p1, X p2) -> fuse v1 v2 p1 p2 X d
        _            -> Nothing

    fuse v1 v2 p1 p2 mk d = do
      nb <- IM.lookup v1 (_neighborMap d)
      bundle <- M.lookup v2 nb
      if simpleCount bundle == 0 || hadamardCount bundle > 0
        then Nothing
        else do
          let v2Nbs = neighborBundles v2 d
              dWithoutV2 = removeVertex v2 d
              redirect acc (w, bndl) =
                if w == v1 then acc else mergeEdges v1 w bndl acc
              dRedirected = foldl redirect dWithoutV2 (M.toList v2Nbs)
              dFinal = dRedirected
                { _vertices = IM.adjust (const (mk (addPhase p1 p2))) v1 (_vertices dRedirected) }
          return dFinal

-- | Remove a phaseless degree-2 spider with two simple edges.
identityRemoval :: Rule
identityRemoval = vertexRule tryVertex
  where
    tryVertex v (Z 0) d = tryRemove v d
    tryVertex v (X 0) d = tryRemove v d
    tryVertex _ _ _     = Nothing

    tryRemove v d = do
      nb <- IM.lookup v (_neighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, b1), (b, b2)]
          | a /= b
          , simpleCount b1 > 0 && hadamardCount b1 == 0
          , simpleCount b2 > 0 && hadamardCount b2 == 0 ->
              let d1 = removeVertex v d
                  d2 = addEdge a b Simple d1
              in Just d2
        _ -> Nothing

-- | Color change: convert an X spider into a Z spider, toggling edge types.
-- Only converts X -> Z, not Z -> X, to avoid infinite loops when
-- used with 'runStrategy'.
colorChange :: Rule
colorChange = vertexRule tryVertex
  where
    tryVertex v (X p) d = Just (flipColor v p Z d)
    tryVertex _ _ _     = Nothing

    flipColor v p newTy d =
      let nb = neighborBundles v d
          d1 = d { _vertices = IM.adjust (const (newTy p)) v (_vertices d) }
          step acc (w, bndl) =
            let sCount = simpleCount bndl
                hCount = hadamardCount bndl
                acc1 = removeEdgeBundle v w acc
                acc2 = foldl (\a _ -> addEdge v w Hadamard a) acc1 [1..sCount]
                acc3 = foldl (\a _ -> addEdge v w Simple a) acc2 [1..hCount]
            in acc3
          d2 = foldl step d1 (M.toList nb)
      in d2

-- | Cancel pairs of simple edges between opposite-colour spiders.
hopfRule :: Rule
hopfRule = edgeRule tryEdge
  where
    tryEdge (v1, v2) bundle d
      | v1 == v2               = Nothing
      | simpleCount bundle < 2 = Nothing
      | otherwise = do
          t1 <- IM.lookup v1 (_vertices d)
          t2 <- IM.lookup v2 (_vertices d)
          case (t1, t2) of
            (Z _, X _) -> applyHopf v1 v2 bundle d
            (X _, Z _) -> applyHopf v1 v2 bundle d
            _          -> Nothing

    applyHopf v1 v2 bundle d =
      let newBundle = bundle { simpleCount = simpleCount bundle - 2 }
          d' = if bundleHasEdges newBundle
               then updateEdge v1 v2 newBundle d
               else removeEdgeBundle v1 v2 d
      in Just d'

-- | Remove a simple self-loop from any non-boundary spider.
selfLoopRemoval :: Rule
selfLoopRemoval = vertexRule tryVertex
  where
    tryVertex _ (Boundary _) _ = Nothing
    tryVertex v _ d = do
      nb <- IM.lookup v (_neighborMap d)
      b <- M.lookup v nb
      if simpleCount b > 0
        then Just $ removeEdge v v Simple d
        else Nothing

-- | Push a π phase through an opposite-colour degree-2 spider.
piCommutation :: Rule
piCommutation = vertexRule tryVertex
  where
    isPi p = p == 1 % 1 || p == (-1) % 1

    tryVertex v (Z p) d | isPi p = tryPush v True d
    tryVertex v (X p) d | isPi p = tryPush v False d
    tryVertex _ _ _              = Nothing

    tryPush v isZ d = do
      nb <- IM.lookup v (_neighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, ba), (b, bb)]
          | a /= b
          , simpleCount ba > 0 && hadamardCount ba == 0
          , simpleCount bb > 0 && hadamardCount bb == 0 -> do
              let (opposite, other) = if isOppositeColor d a isZ then (a, b) else (b, a)
              tOpp <- IM.lookup opposite (_vertices d)
              nbOpp <- IM.lookup opposite (_neighborMap d)
              if M.size nbOpp /= 2 then Nothing else
                case (isZ, tOpp) of
                  (True, X alpha) ->
                    let d1 = removeVertex v d
                        d2 = d1 { _vertices = IM.adjust (const (X (addPhase alpha (1 % 1)))) opposite (_vertices d1) }
                        d3 = addEdge other opposite Simple d2
                    in Just d3
                  (False, Z alpha) ->
                    let d1 = removeVertex v d
                        d2 = d1 { _vertices = IM.adjust (const (Z (addPhase alpha (1 % 1)))) opposite (_vertices d1) }
                        d3 = addEdge other opposite Simple d2
                    in Just d3
                  _ -> Nothing
        _ -> Nothing

    isOppositeColor d w True  = case IM.lookup w (_vertices d) of Just (X _) -> True; _ -> False
    isOppositeColor d w False = case IM.lookup w (_vertices d) of Just (Z _) -> True; _ -> False

-- | Absorb degree-1 state spiders into their neighbours.
stateCopy :: Rule
stateCopy = vertexRule tryVertex
  where
    isPi p = p == 1 % 1 || p == (-1) % 1

    tryVertex v (Z 0) d | degree v d == 1 = tryRemoveZ v d
    tryVertex v (Z p) d | isPi p && degree v d == 1 = tryRemoveZPi v d p
    tryVertex v (X 0) d | degree v d == 1 = tryRemoveX v d
    tryVertex v (X p) d | isPi p && degree v d == 1 = tryRemoveXPi v d p
    tryVertex _ _ _ = Nothing

    tryRemoveZ v d = do
      [w] <- Just (neighbors v d)
      case IM.lookup w (_vertices d) of
        Just (X _) -> Just (removeVertex v d)
        _ -> Nothing

    tryRemoveZPi v d p = do
      [w] <- Just (neighbors v d)
      case IM.lookup w (_vertices d) of
        Just (X alpha) ->
          let d1 = removeVertex v d
              d2 = d1 { _vertices = IM.adjust (const (X (addPhase alpha p))) w (_vertices d1) }
          in Just d2
        _ -> Nothing

    tryRemoveX v d = do
      [w] <- Just (neighbors v d)
      case IM.lookup w (_vertices d) of
        Just (Z _) -> Just (removeVertex v d)
        _ -> Nothing

    tryRemoveXPi v d p = do
      [w] <- Just (neighbors v d)
      case IM.lookup w (_vertices d) of
        Just (Z alpha) ->
          let d1 = removeVertex v d
              d2 = d1 { _vertices = IM.adjust (const (Z (addPhase alpha p))) w (_vertices d1) }
          in Just d2
        _ -> Nothing

-- | Combined π-copy rule: tries both state copy and π-commutation.
--
-- The rules have non-overlapping preconditions (different degree requirements),
-- so they can be safely combined without conflict.
piCopy :: Rule
piCopy d = case stateCopy d of
  Just d' -> Just d'
  Nothing -> piCommutation d

-- | Apply the bialgebra law to a square of X/Z spiders.
bialgebraSimp :: Rule
bialgebraSimp = searchRule candidates tryQuadruple
  where
    candidates d =
      [ (x1, z1, x2, z2)
      | x1 <- IM.keys (_vertices d), isX0 d x1
      , z1 <- neighbors x1 d, isZ0 d z1
      , x2 <- neighbors z1 d, x2 > x1, isX0 d x2
      , z2 <- neighbors x2 d, z2 /= z1, isZ0 d z2
      , z2 `elem` neighbors x1 d
      ]

    isX0 d v = case IM.lookup v (_vertices d) of Just (X 0) -> True; _ -> False
    isZ0 d v = case IM.lookup v (_vertices d) of Just (Z 0) -> True; _ -> False

    tryQuadruple (x1, z1, x2, z2) d = do
      let x1Nbs = neighborBundles x1 d
          x2Nbs = neighborBundles x2 d
          z1Nbs = neighborBundles z1 d
          z2Nbs = neighborBundles z2 d
      if M.size x1Nbs /= 3 || M.size x2Nbs /= 3 || M.size z1Nbs /= 3 || M.size z2Nbs /= 3
        then Nothing
        else do
          let x1Ext = head $ filter (`notElem` [z1, z2]) (M.keys x1Nbs)
              x2Ext = head $ filter (`notElem` [z1, z2]) (M.keys x2Nbs)
              z1Ext = head $ filter (`notElem` [x1, x2]) (M.keys z1Nbs)
              z2Ext = head $ filter (`notElem` [x1, x2]) (M.keys z2Nbs)
              allSimple bs = all (\b -> simpleCount b > 0 && hadamardCount b == 0) bs
          if not (allSimple (map snd (M.toList x1Nbs)) &&
                  allSimple (map snd (M.toList x2Nbs)) &&
                  allSimple (map snd (M.toList z1Nbs)) &&
                  allSimple (map snd (M.toList z2Nbs)))
            then Nothing
            else do
              let d1 = removeVertex x1 d
                  d2 = removeVertex z1 d1
                  d3 = removeVertex x2 d2
                  d4 = removeVertex z2 d3
                  (newZ, d5) = allocVertex (Z 0) d4
                  (newX, d6) = allocVertex (X 0) d5
                  d7 = addEdge x1Ext newZ Simple d6
                  d8 = addEdge x2Ext newZ Simple d7
                  d9 = addEdge z1Ext newX Simple d8
                  d10 = addEdge z2Ext newX Simple d9
                  d11 = addEdge newZ newX Simple d10
              return d11

-- | Local complementation on an interior Pauli spider in graph-like form.
localComplementation :: Rule
localComplementation = searchRule candidates tryVertex
  where
    candidates d = filter (isInteriorPauli d) (IM.keys (_vertices d))

    isInteriorPauli d v = case IM.lookup v (_vertices d) of
      Just (Z p) | isPauli p ->
        let nbs = neighbors v d
            isHadamardEdge w = case M.lookup w (neighborBundles v d) of
              Just b -> simpleCount b == 0 && hadamardCount b > 0
              Nothing -> False
            allHadamard = all isHadamardEdge nbs
            noBoundary = all (\w -> case IM.lookup w (_vertices d) of Just (Boundary _) -> False; _ -> True) nbs
        in allHadamard && noBoundary
      _ -> False

    isPauli p = p == 0 || p == 1 % 1 || p == (-1) % 1

    tryVertex v d = do
      t <- IM.lookup v (_vertices d)
      let nbs = neighbors v d
          delta = case t of
            Z p | p == 0        -> 1 % 2
                | p == 1 % 1    -> (-1) % 2
                | p == (-1) % 1 -> (-1) % 2
            _                   -> 0
      let d1 = toggleNeighborEdges v nbs d
      let d2 = foldl (\acc w -> acc { _vertices = IM.adjust (addDeltaToZ delta) w (_vertices acc) }) d1 nbs
      let d3 = removeVertex v d2
          scalarFactor = sqrt2Pow 0
      return $ mapScalar (`mulScalar` scalarFactor) d3

    addDeltaToZ delta ty = case ty of
      Z p -> Z (addPhase p delta)
      _   -> ty

    toggleNeighborEdges v nbs acc =
      let pairs = [ (a, b) | a <- nbs, b <- nbs, a < b ]
      in foldl (togglePair v) acc pairs

    togglePair v acc (a, b) =
      let bundle = M.findWithDefault emptyBundle (normalizeEdge a b) (_edges acc)
      in if hadamardCount bundle > 0
         then let newB = bundle { hadamardCount = hadamardCount bundle - 1 }
              in if bundleHasEdges newB
                 then updateEdge a b newB acc
                 else removeEdgeBundle a b acc
         else let newB = addEdgeToBundle Hadamard bundle
              in updateEdge a b newB acc

-- | Pivot on a Hadamard edge between two interior Pauli Z spiders.
pivot :: Rule
pivot = searchRule candidates tryEdge
  where
    candidates d =
      [ ((v1, v2), b)
      | ((v1, v2), b) <- M.toList (_edges d)
      , v1 /= v2
      , hadamardCount b > 0
      , isZ d v1 && isZ d v2
      ]

    isZ d v = case IM.lookup v (_vertices d) of Just (Z _) -> True; _ -> False

    tryEdge ((u, v), _) d = do
      tU <- IM.lookup u (_vertices d)
      tV <- IM.lookup v (_vertices d)
      let (alpha, beta) = case (tU, tV) of
            (Z a, Z b) -> (a, b)
            _          -> (0, 0)
      if not (isPauli alpha || isPauli beta) then Nothing else do
        let nu = filter (/= v) (neighbors u d)
            nv = filter (/= u) (neighbors v d)
        if any (\w -> case IM.lookup w (_vertices d) of Just (Boundary _) -> True; _ -> False) (u : v : nu ++ nv)
          then Nothing
          else do
            let toggleRow acc a = foldl (\acc2 b -> toggleHadamard a b acc2) acc nv
                d1 = foldl toggleRow d nu
            let nBoth = filter (`elem` nv) nu
                nuOnly = filter (`notElem` nv) nu
                nvOnly = filter (`notElem` nu) nv
                d2 = foldl (\acc w -> addPhaseToZ beta w acc) d1 nuOnly
                d3 = foldl (\acc w -> addPhaseToZ alpha w acc) d2 nvOnly
                d4 = foldl (\acc w -> addPhaseToZ (addPhase alpha (addPhase beta (1 % 1))) w acc) d3 nBoth
            let d5 = removeVertex u d4
                d6 = removeVertex v d5
            return d6

    isPauli p = p == 0 || p == 1 % 1 || p == (-1) % 1

    toggleHadamard a b acc =
      let bundle = M.findWithDefault emptyBundle (normalizeEdge a b) (_edges acc)
      in if hadamardCount bundle > 0
         then let newB = bundle { hadamardCount = hadamardCount bundle - 1 }
              in if bundleHasEdges newB
                 then updateEdge a b newB acc
                 else removeEdgeBundle a b acc
         else let newB = addEdgeToBundle Hadamard bundle
              in updateEdge a b newB acc

    addPhaseToZ delta w acc =
      acc { _vertices = IM.adjust (\ty -> case ty of Z p -> Z (addPhase p delta); _ -> ty) w (_vertices acc) }
