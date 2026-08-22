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
  , simplifyEdgeBundles
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ratio ((%))

import HZX.Core.Diagram
import HZX.Core.Phase
import HZX.Core.Scalar

type Rule = Diagram -> Maybe Diagram

hadamardEdgeSimp :: Rule
hadamardEdgeSimp d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    tryVertex v HBox acc = case acc of
      Just _ -> acc
      Nothing -> do
        nb <- IM.lookup v (_neighborMap d)
        let ns = M.toList nb
        case ns of
          [(a, b1), (b, b2)]
            | a /= b
            , edgeCount b1 == 1
            , edgeCount b2 == 1 ->
                -- An HBox is a Hadamard gate.  The path a --e1-- HBox --e2-- b
                -- therefore contains (e1 + 1 + e2) Hadamards in series.
                -- Two Hadamards in series cancel, so the replacement edge is
                -- Hadamard iff this total is odd.
                let d1 = removeVertex v d
                    d2 = addEdge a b (replacementType b1 b2) d1
                in Just d2
          _ -> Nothing
    tryVertex _ _ acc = acc

    edgeCount b = simpleCount b + hadamardCount b

    replacementType b1 b2 =
      let totalHadamards = hadamardCount b1 + 1 + hadamardCount b2
      in if odd totalHadamards then Hadamard else Simple

spiderFusion :: Rule
spiderFusion d = listToMaybe $ mapMaybe tryPair pairs
  where
    vs = IM.keys (_vertices d)
    pairs = [(v1, v2) | v1 <- vs, v2 <- vs, v1 < v2]

    tryPair (v1, v2) = do
      t1 <- IM.lookup v1 (_vertices d)
      t2 <- IM.lookup v2 (_vertices d)
      -- Spiders fuse only when connected by simple edges (no Hadamard edges).
      nb <- IM.lookup v1 (_neighborMap d)
      bndl <- M.lookup v2 nb
      if hadamardCount bndl > 0 then Nothing else
        case (t1, t2) of
          (Z p1, Z p2) -> fuse v1 v2 p1 p2 Z bndl
          (X p1, X p2) -> fuse v1 v2 p1 p2 X bndl
          _            -> Nothing

    fuse v1 v2 p1 p2 mk bndl = do
      if simpleCount bndl == 0 then Nothing else do
        let v2Nbs = neighborBundles v2 d
            dWithoutV2 = removeVertex v2 d
            redirect acc (w, bndl) =
              if w == v1 then acc else mergeEdges v1 w bndl acc
            dRedirected = foldl redirect dWithoutV2 (M.toList v2Nbs)
            dFinal = dRedirected { _vertices = IM.adjust (const (mk (addPhase p1 p2))) v1 (_vertices dRedirected) }
        return dFinal

identityRemoval :: Rule
identityRemoval d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    tryVertex v (Z 0) acc = tryRemove acc v
    tryVertex v (X 0) acc = tryRemove acc v
    tryVertex _ _ acc     = acc

    tryRemove acc v = case acc of
      Just _ -> acc
      Nothing -> do
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

-- | Color change rule: convert X spiders to Z spiders.
-- Only converts X -> Z, not Z -> X, to avoid infinite loops when
-- used with 'runStrategy'.
colorChange :: Rule
colorChange d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    tryVertex v (X p) acc = case acc of
      Just _ -> acc
      Nothing -> Just (flipColor v p Z)
    tryVertex _ _ acc     = acc

    flipColor v p newTy =
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

hopfRule :: Rule
hopfRule d = listToMaybe $ mapMaybe tryEdge $ M.toList (_edges d)
  where
    tryEdge ((v1, v2), bundle)
      | v1 == v2 = Nothing
      | simpleCount bundle < 2 = Nothing
      | otherwise = do
          t1 <- IM.lookup v1 (_vertices d)
          t2 <- IM.lookup v2 (_vertices d)
          case (t1, t2) of
            (Z _, X _) -> applyHopf v1 v2 bundle
            (X _, Z _) -> applyHopf v1 v2 bundle
            _          -> Nothing

    applyHopf v1 v2 bundle =
      let newBundle = bundle { simpleCount = simpleCount bundle - 2 }
          d' = if bundleHasEdges newBundle
               then updateEdge v1 v2 newBundle d
               else removeEdgeBundle v1 v2 d
      in Just d'

selfLoopRemoval :: Rule
selfLoopRemoval d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    tryVertex v (Boundary _) acc = acc
    tryVertex v _ acc = case acc of
      Just _ -> acc
      Nothing -> do
        nb <- IM.lookup v (_neighborMap d)
        b <- M.lookup v nb
        if simpleCount b > 0
        then Just $ removeEdge v v Simple d
        else Nothing

piCommutation :: Rule
piCommutation d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    isPi p = p == 1 % 1 || p == (-1) % 1

    tryVertex v (Z p) acc | isPi p = tryPush acc v True
    tryVertex v (X p) acc | isPi p = tryPush acc v False
    tryVertex _ _ acc               = acc

    tryPush acc v isZ = case acc of
      Just _ -> acc
      Nothing -> do
        nb <- IM.lookup v (_neighborMap d)
        let ns = M.toList nb
        case ns of
          [(a, ba), (b, bb)]
            | a /= b
            , simpleCount ba > 0 && hadamardCount ba == 0
            , simpleCount bb > 0 && hadamardCount bb == 0 -> do
                let (opposite, other) = if isOppositeColor a isZ then (a, b) else (b, a)
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

    isOppositeColor w True  = case IM.lookup w (_vertices d) of Just (X _) -> True; _ -> False
    isOppositeColor w False = case IM.lookup w (_vertices d) of Just (Z _) -> True; _ -> False

stateCopy :: Rule
stateCopy d = IM.foldrWithKey tryVertex Nothing (_vertices d)
  where
    isPi p = p == 1 % 1 || p == (-1) % 1

    tryVertex v (Z 0) acc | degree v d == 1 = tryRemoveZ acc v
    tryVertex v (Z p) acc | isPi p && degree v d == 1 = tryRemoveZPi acc v p
    tryVertex v (X 0) acc | degree v d == 1 = tryRemoveX acc v
    tryVertex v (X p) acc | isPi p && degree v d == 1 = tryRemoveXPi acc v p
    tryVertex _ _ acc = acc

    tryRemoveZ acc v = case acc of
      Just _ -> acc
      Nothing -> do
        [w] <- Just (neighbors v d)
        case IM.lookup w (_vertices d) of
          Just (X _) -> Just (removeVertex v d)
          _ -> Nothing

    tryRemoveZPi acc v p = case acc of
      Just _ -> acc
      Nothing -> do
        [w] <- Just (neighbors v d)
        case IM.lookup w (_vertices d) of
          Just (X alpha) ->
            let d1 = removeVertex v d
                d2 = d1 { _vertices = IM.adjust (const (X (addPhase alpha p))) w (_vertices d1) }
            in Just d2
          _ -> Nothing

    tryRemoveX acc v = case acc of
      Just _ -> acc
      Nothing -> do
        [w] <- Just (neighbors v d)
        case IM.lookup w (_vertices d) of
          Just (Z _) -> Just (removeVertex v d)
          _ -> Nothing

    tryRemoveXPi acc v p = case acc of
      Just _ -> acc
      Nothing -> do
        [w] <- Just (neighbors v d)
        case IM.lookup w (_vertices d) of
          Just (Z alpha) ->
            let d1 = removeVertex v d
                d2 = d1 { _vertices = IM.adjust (const (Z (addPhase alpha p))) w (_vertices d1) }
            in Just d2
          _ -> Nothing

-- | Combined π-copy rule: tries both state copy and π-commutation.
--
-- This rule combines two related π-phase copying operations:
-- 1. 'stateCopy': |0⟩/|1⟩ or |+⟩/|-⟩ states copy through same-color spiders (degree-1)
-- 2. 'piCommutation': π-phase copies through opposite-color spiders (degree-2)
--
-- The rules have non-overlapping preconditions (different degree requirements),
-- so they can be safely combined without conflict.
--
-- Tries 'stateCopy' first (degree-1 check is cheaper), then 'piCommutation'.
piCopy :: Rule
piCopy d = case stateCopy d of
  Just d' -> Just d'
  Nothing -> piCommutation d

bialgebraSimp :: Rule
bialgebraSimp d = listToMaybe $ mapMaybe tryQuadruple quadruples
  where
    vs = IM.keys (_vertices d)
    quadruples = [ (x1, z1, x2, z2)
                 | x1 <- vs, isX0 x1
                 , z1 <- neighbors x1 d, isZ0 z1
                 , x2 <- neighbors z1 d, x2 > x1, isX0 x2
                 , z2 <- neighbors x2 d, z2 /= z1, isZ0 z2
                 , z2 `elem` neighbors x1 d
                 ]

    isX0 v = case IM.lookup v (_vertices d) of Just (X 0) -> True; _ -> False
    isZ0 v = case IM.lookup v (_vertices d) of Just (Z 0) -> True; _ -> False

    tryQuadruple (x1, z1, x2, z2) = do
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
        let allSimple bs = all (\b -> simpleCount b > 0 && hadamardCount b == 0) bs
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

localComplementation :: Rule
localComplementation d = listToMaybe $ mapMaybe tryVertex interiorZ
  where
    interiorZ = filter isInteriorPauli (IM.keys (_vertices d))

    isInteriorPauli v = case IM.lookup v (_vertices d) of
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

    tryVertex v = do
      t <- IM.lookup v (_vertices d)
      let nbs = neighbors v d
          -- Local complementation adds the removed spider's phase to each
          -- neighbour.  We restrict to Pauli spiders, so the phase is 0, π,
          -- or -π.
          delta = case t of
            Z p -> p
            _   -> 0
          n = length nbs
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

pivot :: Rule
pivot d = listToMaybe $ mapMaybe tryEdge hadamardEdges
  where
    hadamardEdges = [ ((v1, v2), b)
                    | ((v1, v2), b) <- M.toList (_edges d)
                    , v1 /= v2
                    , hadamardCount b > 0
                    , isZ v1 && isZ v2
                    ]

    isZ v = case IM.lookup v (_vertices d) of Just (Z _) -> True; _ -> False

    tryEdge ((u, v), _) = do
      tU <- IM.lookup u (_vertices d)
      tV <- IM.lookup v (_vertices d)
      let (alpha, beta) = case (tU, tV) of
            (Z a, Z b) -> (a, b)
            _          -> (0, 0)
      if not (isClifford alpha && isClifford beta) then Nothing else do
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

    isClifford :: Rational -> Bool
    isClifford p =
      let p' = phaseMod2 p
      in p' == 0 || p' == 1 % 2 || p' == 1 || p' == 3 % 2
      where
        phaseMod2 x =
          let q = fromInteger (floor (fromRational x / (2 :: Double))) * (2 :: Rational)
              r = x - q
          in if r < 0 then r + 2 else r

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

-- | Canonicalise parallel edge bundles.
--
--   Multiple simple edges collapse to one, Hadamard edges are taken modulo 2,
--   and a simple edge together with an odd number of Hadamard edges becomes a
--   single Hadamard edge.  This is required before pivot / local-complementation
--   rules, which assume a simple graph of Hadamard edges between interior spiders.
simplifyEdgeBundles :: Rule
simplifyEdgeBundles d =
  let d' = M.foldrWithKey canonicalize d (_edges d)
  in if d' == d then Nothing else Just d'
  where
    canonicalize (v1, v2) b acc =
      case (IM.lookup v1 (_vertices d), IM.lookup v2 (_vertices d)) of
        (Just (Boundary _), _) -> acc
        (_, Just (Boundary _)) -> acc
        _ ->
          let hParity = hadamardCount b `mod` 2
              sCount  = if simpleCount b > 0 then 1 else 0
              newB    = case (sCount, hParity) of
                          (_, 1) -> EdgeBundle 0 1
                          (1, 0) -> EdgeBundle 1 0
                          _      -> emptyBundle
          in updateEdge v1 v2 newB acc
