{-# LANGUAGE TupleSections #-}

-- | Parametric ZX rewrite rules.
--
--   These rules mirror the concrete rules in 'HZX.Rewrite.Rule' but operate
--   on symbolic phases.  They assume that all parameters are binary
--   variables, so phase predicates such as "is π" or "is Pauli" are
--   evaluated symbolically.
module HZX.Core.Diagram.Parametric.Rewrite
  ( ParamRule
  , pSpiderFusion
  , pIdentityRemoval
  , pHadamardEdgeSimp
  , pColorChange
  , pHopfRule
  , pSelfLoopRemoval
  , pPiCommutation
  , pStateCopy
  , pPiCopy
  , pBialgebraSimp
  , pLocalComplementation
  , pPivot
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Ratio ((%))

import HZX.Core.Diagram.Types
  ( EdgeType(..), EdgeBundle(..), emptyBundle
  , simpleCount, hadamardCount, bundleHasEdges
  , normalizeEdge, addEdgeToBundle )
import HZX.Core.Diagram.Parametric.Types
  ( ParamDiagram(..), ParamVertexType(..), ParamPhase(..) )
import HZX.Core.Diagram.Parametric.Instances
  ( pLookupVertex, pAllVertices, pNeighborBundles, pNeighbors, pDegree
  , pAdjustVertex, pAllocVertex, pRemoveVertex, pAddEdge, pRemoveEdge
  , pRemoveEdgeBundle, pMergeEdges, pUpdateEdge )
import HZX.Core.Diagram.Parametric.Phase
  ( addParamPhase, isParamZero, isParamPi )
import HZX.Core.Diagram.Parametric.Scalar
  ( mulParamScalar, sqrt2ParamPow )
import HZX.Core.Diagram.Parametric.Search


type ParamRule = ParamDiagram -> Maybe ParamDiagram

-- ---------------------------------------------------------------------------
-- Spider fusion
-- ---------------------------------------------------------------------------

pSpiderFusion :: ParamRule
pSpiderFusion = pPairRule tryPair
  where
    tryPair v1 v2 d = do
      t1 <- pLookupVertex v1 d
      t2 <- pLookupVertex v2 d
      case (t1, t2) of
        (ParamZ p1, ParamZ p2) -> fuse v1 v2 p1 p2 ParamZ d
        (ParamX p1, ParamX p2) -> fuse v1 v2 p1 p2 ParamX d
        _                      -> Nothing

    fuse v1 v2 p1 p2 mk d = do
      nb <- IM.lookup v1 (_pNeighborMap d)
      if not (M.member v2 nb) then Nothing else do
        let v2Nbs = pNeighborBundles v2 d
            dWithoutV2 = pRemoveVertex v2 d
            redirect acc (w, bndl) =
              if w == v1 then acc else pMergeEdges v1 w bndl acc
            dRedirected = foldl redirect dWithoutV2 (M.toList v2Nbs)
            dFinal = pAdjustVertex v1 (const (mk (addParamPhase p1 p2))) dRedirected
        return dFinal

-- ---------------------------------------------------------------------------
-- Identity removal
-- ---------------------------------------------------------------------------

pIdentityRemoval :: ParamRule
pIdentityRemoval = pVertexRule tryVertex
  where
    tryVertex v (ParamZ p) d = tryRemove v p d
    tryVertex v (ParamX p) d = tryRemove v p d
    tryVertex _ _ _          = Nothing

    tryRemove v p d = do
      nb <- IM.lookup v (_pNeighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, b1), (b, b2)]
          | a /= b
          , simpleCount b1 > 0 && hadamardCount b1 == 0
          , simpleCount b2 > 0 && hadamardCount b2 == 0
          , isParamZero p || isParamPi p ->  -- phaseless or π-phase spider
              let d1 = pRemoveVertex v d
                  d2 = pAddEdge a b Simple d1
              in Just d2
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Hadamard edge simplification
-- ---------------------------------------------------------------------------

pHadamardEdgeSimp :: ParamRule
pHadamardEdgeSimp = pVertexRule tryVertex
  where
    tryVertex v ParamHBox d = do
      nb <- IM.lookup v (_pNeighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, _), (b, _)] | a /= b ->
          let d1 = pRemoveVertex v d
              d2 = pAddEdge a b Hadamard d1
          in Just d2
        _ -> Nothing
    tryVertex _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Color change (X -> Z only, to avoid infinite loops)
-- ---------------------------------------------------------------------------

pColorChange :: ParamRule
pColorChange = pVertexRule tryVertex
  where
    tryVertex v (ParamX p) d = Just (pFlipColor v p ParamZ d)
    tryVertex _ _ _          = Nothing

    pFlipColor v p newTy d0 =
      let nb = pNeighborBundles v d0
          d1 = pAdjustVertex v (const (newTy p)) d0
          step acc (w, bndl) =
            let sCount = simpleCount bndl
                hCount = hadamardCount bndl
                acc1 = pRemoveEdgeBundle v w acc
                acc2 = foldl (\a _ -> pAddEdge v w Hadamard a) acc1 [1..sCount]
                acc3 = foldl (\a _ -> pAddEdge v w Simple a) acc2 [1..hCount]
            in acc3
      in foldl step d1 (M.toList nb)

-- ---------------------------------------------------------------------------
-- Hopf rule
-- ---------------------------------------------------------------------------

pHopfRule :: ParamRule
pHopfRule = pEdgeRule tryEdge
  where
    tryEdge (v1, v2) bundle d
      | v1 == v2               = Nothing
      | simpleCount bundle < 2 = Nothing
      | otherwise = do
          t1 <- pLookupVertex v1 d
          t2 <- pLookupVertex v2 d
          case (t1, t2) of
            (ParamZ _, ParamX _) -> applyHopf v1 v2 bundle d
            (ParamX _, ParamZ _) -> applyHopf v1 v2 bundle d
            _                    -> Nothing

    applyHopf v1 v2 bundle d =
      let newBundle = bundle { simpleCount = simpleCount bundle - 2 }
          d' = if bundleHasEdges newBundle
               then pUpdateEdge v1 v2 newBundle d
               else pRemoveEdgeBundle v1 v2 d
      in Just d'

-- ---------------------------------------------------------------------------
-- Self-loop removal
-- ---------------------------------------------------------------------------

pSelfLoopRemoval :: ParamRule
pSelfLoopRemoval = pVertexRule tryVertex
  where
    tryVertex _ (ParamBoundary _) _ = Nothing
    tryVertex v _ d = do
      nb <- IM.lookup v (_pNeighborMap d)
      b <- M.lookup v nb
      if simpleCount b > 0
        then Just $ pRemoveEdge v v Simple d
        else Nothing

-- ---------------------------------------------------------------------------
-- π-commutation
-- ---------------------------------------------------------------------------

pPiCommutation :: ParamRule
pPiCommutation = pVertexRule tryVertex
  where
    isPi p = isParamPi p

    tryVertex v (ParamZ p) d | isPi p = tryPush v True d
    tryVertex v (ParamX p) d | isPi p = tryPush v False d
    tryVertex _ _ _                  = Nothing

    tryPush v isZ d = do
      nb <- IM.lookup v (_pNeighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, ba), (b, bb)]
          | a /= b
          , simpleCount ba > 0 && hadamardCount ba == 0
          , simpleCount bb > 0 && hadamardCount bb == 0 -> do
              let (opposite, other) = if isOppositeColor d a isZ then (a, b) else (b, a)
              tOpp <- pLookupVertex opposite d
              nbOpp <- IM.lookup opposite (_pNeighborMap d)
              if M.size nbOpp /= 2 then Nothing else
                case (isZ, tOpp) of
                  (True, ParamX alpha) ->
                    let d1 = pRemoveVertex v d
                        d2 = pAdjustVertex opposite (const (ParamX (addParamPhase alpha (ParamPhase 1 M.empty)))) d1
                        d3 = pAddEdge other opposite Simple d2
                    in Just d3
                  (False, ParamZ alpha) ->
                    let d1 = pRemoveVertex v d
                        d2 = pAdjustVertex opposite (const (ParamZ (addParamPhase alpha (ParamPhase 1 M.empty)))) d1
                        d3 = pAddEdge other opposite Simple d2
                    in Just d3
                  _ -> Nothing
        _ -> Nothing

    isOppositeColor d w True  = case pLookupVertex w d of Just (ParamX _) -> True; _ -> False
    isOppositeColor d w False = case pLookupVertex w d of Just (ParamZ _) -> True; _ -> False

-- ---------------------------------------------------------------------------
-- State copy
-- ---------------------------------------------------------------------------

pStateCopy :: ParamRule
pStateCopy = pVertexRule tryVertex
  where
    isPi p = isParamPi p

    tryVertex v (ParamZ p) d | isParamZero p && pDegree v d == 1 = tryRemoveZ v d
    tryVertex v (ParamZ p) d | isPi p && pDegree v d == 1 = tryRemoveZPi v d p
    tryVertex v (ParamX p) d | isParamZero p && pDegree v d == 1 = tryRemoveX v d
    tryVertex v (ParamX p) d | isPi p && pDegree v d == 1 = tryRemoveXPi v d p
    tryVertex _ _ _ = Nothing

    tryRemoveZ v d = do
      [w] <- Just (pNeighbors v d)
      case pLookupVertex w d of
        Just (ParamX _) -> Just (pRemoveVertex v d)
        _               -> Nothing

    tryRemoveZPi v d p = do
      [w] <- Just (pNeighbors v d)
      case pLookupVertex w d of
        Just (ParamX alpha) ->
          let d1 = pRemoveVertex v d
              d2 = pAdjustVertex w (const (ParamX (addParamPhase alpha p))) d1
          in Just d2
        _ -> Nothing

    tryRemoveX v d = do
      [w] <- Just (pNeighbors v d)
      case pLookupVertex w d of
        Just (ParamZ _) -> Just (pRemoveVertex v d)
        _               -> Nothing

    tryRemoveXPi v d p = do
      [w] <- Just (pNeighbors v d)
      case pLookupVertex w d of
        Just (ParamZ alpha) ->
          let d1 = pRemoveVertex v d
              d2 = pAdjustVertex w (const (ParamZ (addParamPhase alpha p))) d1
          in Just d2
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Combined π-copy rule
-- ---------------------------------------------------------------------------

pPiCopy :: ParamRule
pPiCopy d = case pStateCopy d of
  Just d' -> Just d'
  Nothing -> pPiCommutation d

-- ---------------------------------------------------------------------------
-- Bialgebra simplification
-- ---------------------------------------------------------------------------

pBialgebraSimp :: ParamRule
pBialgebraSimp = pSearchRule candidates tryQuadruple
  where
    candidates d =
      [ (x1, z1, x2, z2)
      | x1 <- pAllVertices d, isX0 d x1
      , z1 <- pNeighbors x1 d, isZ0 d z1
      , x2 <- pNeighbors z1 d, x2 > x1, isX0 d x2
      , z2 <- pNeighbors x2 d, z2 /= z1, isZ0 d z2
      , z2 `elem` pNeighbors x1 d
      ]

    isX0 d v = case pLookupVertex v d of Just (ParamX p) -> isParamZero p; _ -> False
    isZ0 d v = case pLookupVertex v d of Just (ParamZ p) -> isParamZero p; _ -> False

    tryQuadruple (x1, z1, x2, z2) d = do
      let x1Nbs = pNeighborBundles x1 d
          x2Nbs = pNeighborBundles x2 d
          z1Nbs = pNeighborBundles z1 d
          z2Nbs = pNeighborBundles z2 d
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
              let d1 = pRemoveVertex x1 d
                  d2 = pRemoveVertex z1 d1
                  d3 = pRemoveVertex x2 d2
                  d4 = pRemoveVertex z2 d3
                  (newZ, d5) = pAllocVertex (ParamZ (ParamPhase 0 M.empty)) d4
                  (newX, d6) = pAllocVertex (ParamX (ParamPhase 0 M.empty)) d5
                  d7 = pAddEdge x1Ext newZ Simple d6
                  d8 = pAddEdge x2Ext newZ Simple d7
                  d9 = pAddEdge z1Ext newX Simple d8
                  d10 = pAddEdge z2Ext newX Simple d9
                  d11 = pAddEdge newZ newX Simple d10
              return d11

-- ---------------------------------------------------------------------------
-- Local complementation
-- ---------------------------------------------------------------------------

pLocalComplementation :: ParamRule
pLocalComplementation = pSearchRule candidates tryVertex
  where
    candidates d = filter (isInteriorPauli d) (pAllVertices d)

    isInteriorPauli d v = case pLookupVertex v d of
      Just (ParamZ p) | isParamZero p || isParamPi p ->
        let nbs = pNeighbors v d
            isHadamardEdge w = case M.lookup w (pNeighborBundles v d) of
              Just b -> simpleCount b == 0 && hadamardCount b > 0
              Nothing -> False
            allHadamard = all isHadamardEdge nbs
            noBoundary = all (\w -> case pLookupVertex w d of Just (ParamBoundary _) -> False; _ -> True) nbs
        in allHadamard && noBoundary
      _ -> False

    tryVertex v d = do
      t <- pLookupVertex v d
      let nbs = pNeighbors v d
          delta = case t of
            ParamZ p | isParamZero p -> ParamPhase (1 % 2) M.empty
                     | isParamPi p  -> ParamPhase ((-1) % 2) M.empty
            _ -> ParamPhase 0 M.empty
      let d1 = toggleNeighborEdges v nbs d
      let d2 = foldl (\acc w -> acc { _pVertices = IM.adjust (addDeltaToZ delta) w (_pVertices acc) }) d1 nbs
      let d3 = pRemoveVertex v d2
          scalarFactor = sqrt2ParamPow 0
      return $ d3 { _pScalar = mulParamScalar (_pScalar d3) scalarFactor }

    addDeltaToZ delta ty = case ty of
      ParamZ p -> ParamZ (addParamPhase p delta)
      _        -> ty

    toggleNeighborEdges v nbs acc =
      let pairs = [(a, b) | a <- nbs, b <- nbs, a < b]
      in foldl (togglePair v) acc pairs

    togglePair _ acc (a, b) =
      let bundle = M.findWithDefault emptyBundle (normalizeEdge a b) (_pEdges acc)
      in if hadamardCount bundle > 0
         then let newB = bundle { hadamardCount = hadamardCount bundle - 1 }
              in if bundleHasEdges newB
                 then pUpdateEdge a b newB acc
                 else pRemoveEdgeBundle a b acc
         else let newB = addEdgeToBundle Hadamard bundle
              in pUpdateEdge a b newB acc

-- ---------------------------------------------------------------------------
-- Pivot
-- ---------------------------------------------------------------------------

pPivot :: ParamRule
pPivot = pSearchRule candidates tryEdge
  where
    candidates d =
      [ ((v1, v2), b)
      | ((v1, v2), b) <- M.toList (_pEdges d)
      , v1 /= v2
      , hadamardCount b > 0
      , isZ d v1 && isZ d v2
      ]

    isZ d v = case pLookupVertex v d of Just (ParamZ _) -> True; _ -> False

    tryEdge ((u, v), _) d = do
      tU <- pLookupVertex u d
      tV <- pLookupVertex v d
      let (alpha, beta) = case (tU, tV) of
            (ParamZ a, ParamZ b) -> (a, b)
            _                    -> (ParamPhase 0 M.empty, ParamPhase 0 M.empty)
      if not ((isParamZero alpha || isParamPi alpha) || (isParamZero beta || isParamPi beta)) then Nothing else do
        let nu = filter (/= v) (pNeighbors u d)
            nv = filter (/= u) (pNeighbors v d)
        if any (\w -> case pLookupVertex w d of Just (ParamBoundary _) -> True; _ -> False) (u : v : nu ++ nv)
          then Nothing
          else do
            let toggleRow acc a = foldl (\acc2 b -> toggleHadamard a b acc2) acc nv
                d1 = foldl toggleRow d nu
            let nBoth = filter (`elem` nv) nu
                nuOnly = filter (`notElem` nv) nu
                nvOnly = filter (`notElem` nu) nv
                d2 = foldl (\acc w -> addPhaseToZ beta w acc) d1 nuOnly
                d3 = foldl (\acc w -> addPhaseToZ alpha w acc) d2 nvOnly
                d4 = foldl (\acc w -> addPhaseToZ (addParamPhase alpha (addParamPhase beta (ParamPhase 1 M.empty))) w acc) d3 nBoth
            let d5 = pRemoveVertex u d4
                d6 = pRemoveVertex v d5
            return d6

    toggleHadamard a b acc =
      let bundle = M.findWithDefault emptyBundle (normalizeEdge a b) (_pEdges acc)
      in if hadamardCount bundle > 0
         then let newB = bundle { hadamardCount = hadamardCount bundle - 1 }
              in if bundleHasEdges newB
                 then pUpdateEdge a b newB acc
                 else pRemoveEdgeBundle a b acc
         else let newB = addEdgeToBundle Hadamard bundle
              in pUpdateEdge a b newB acc

    addPhaseToZ delta w acc =
      acc { _pVertices = IM.adjust (\ty -> case ty of ParamZ p -> ParamZ (addParamPhase p delta); _ -> ty) w (_pVertices acc) }
