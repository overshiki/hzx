{-# LANGUAGE LambdaCase #-}

-- | Basic rewrite rules for doubled ZX diagrams.
--
--   This module provides a minimal rule set for doubled diagrams.  It
--   handles fusion of doubled spiders, Hadamard-edge simplification, and
--   conservative classical-structure rules.
module HZX.Core.Diagram.Doubled.Rewrite
  ( DoubledRule
  , dSpiderFusion
  , dHadamardEdgeSimp
  , dCopyIdentity
  , dXorIdentity
  , dCopyFusion
  , dXorFusion
  , dMeasureCopy
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..), DoubledEdgeType(..)
  , DoubledEdgeBundle(..), EdgeKind(..) )
import HZX.Core.Diagram.Doubled.Instances
  ( dLookupVertex, dAllVertices, dRemoveVertex, dMergeEdges, dAddEdge )
import HZX.Core.Diagram.Doubled.Search
import HZX.Core.Diagram.Parametric.Phase (addParamPhase)

type DoubledRule = DoubledDiagram -> Maybe DoubledDiagram

-- ---------------------------------------------------------------------------
-- Quantum spider fusion
-- ---------------------------------------------------------------------------

-- | Fuse adjacent doubled Z/Z or X/X spiders connected by a quantum edge.
dSpiderFusion :: DoubledRule
dSpiderFusion = dPairRule tryPair
  where
    tryPair v1 v2 d = do
      t1 <- dLookupVertex v1 d
      t2 <- dLookupVertex v2 d
      case (t1, t2) of
        (DZ p1, DZ p2) -> fuse v1 v2 p1 p2 DZ d
        (DX p1, DX p2) -> fuse v1 v2 p1 p2 DX d
        _              -> Nothing

    fuse v1 v2 p1 p2 mk d = do
      nb <- IM.lookup v1 (_qNeighborMap d)
      if not (M.member v2 nb) then Nothing else do
        let v2Nbs = IM.findWithDefault M.empty v2 (_qNeighborMap d)
            dWithoutV2 = dRemoveVertex v2 d
            redirect acc (w, bndl) =
              if w == v1 then acc else dMergeEdges v1 w (DSimple Quantum) bndl acc
            dRedirected = foldl redirect dWithoutV2 (M.toList v2Nbs)
            dFinal = dAdjustVertex v1 (const (mk (addParamPhase p1 p2))) dRedirected
        return dFinal

    dAdjustVertex v f d0 = d0 { _dVertices = IM.adjust f v (_dVertices d0) }

-- ---------------------------------------------------------------------------
-- Hadamard edge simplification
-- ---------------------------------------------------------------------------

-- | Remove a doubled H-box of quantum degree 2 and replace it with a
--   Hadamard edge.
dHadamardEdgeSimp :: DoubledRule
dHadamardEdgeSimp = dVertexRule tryVertex
  where
    tryVertex v DHBox d = do
      nb <- IM.lookup v (_qNeighborMap d)
      let ns = M.toList nb
      case ns of
        [(a, _), (b, _)] | a /= b ->
          let d1 = dRemoveVertex v d
              d2 = dAddEdge a b DHadamard d1
          in Just d2
        _ -> Nothing
    tryVertex _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Classical identity removal
-- ---------------------------------------------------------------------------

-- | A classical COPY or XOR spider with exactly one input and one output is
--   just a wire.  Remove it and connect the two neighbors directly.
dCopyIdentity :: DoubledRule
dCopyIdentity = classicalIdentity DCopy

dXorIdentity :: DoubledRule
dXorIdentity = classicalIdentity DXor

classicalIdentity :: DoubledVertexType -> DoubledRule
classicalIdentity ty = dSearchRule candidates tryVertex
  where
    candidates = dAllVertices

    tryVertex v d = do
      t <- dLookupVertex v d
      if t /= ty then Nothing else do
        nb <- IM.lookup v (_cNeighborMap d)
        let ns = M.toList nb
        case ns of
          [(a, b1), (b, b2)] | a /= b
                             , isSingleClassical b1
                             , isSingleClassical b2 ->
              let d1 = dRemoveVertex v d
              in Just (dAddEdge a b (DSimple Classical) d1)
          _ -> Nothing

    isSingleClassical b =
      cSimpleCount b == 1 && qSimpleCount b == 0 && qHadamardCount b == 0

-- ---------------------------------------------------------------------------
-- Classical spider fusion
-- ---------------------------------------------------------------------------

-- | Fuse two classical COPY spiders connected by a classical edge.
dCopyFusion :: DoubledRule
dCopyFusion = classicalFusion DCopy

-- | Fuse two classical XOR spiders connected by a classical edge.
dXorFusion :: DoubledRule
dXorFusion = classicalFusion DXor

classicalFusion :: DoubledVertexType -> DoubledRule
classicalFusion ty = dPairRule tryPair
  where
    tryPair v1 v2 d = do
      t1 <- dLookupVertex v1 d
      t2 <- dLookupVertex v2 d
      if t1 /= ty || t2 /= ty then Nothing else do
        nb <- IM.lookup v1 (_cNeighborMap d)
        if not (M.member v2 nb) then Nothing else do
          let v2Nbs = IM.findWithDefault M.empty v2 (_cNeighborMap d)
              dWithoutV2 = dRemoveVertex v2 d
              redirect acc (w, bndl) =
                if w == v1 then acc else dMergeEdges v1 w (DSimple Classical) bndl acc
          return $ foldl redirect dWithoutV2 (M.toList v2Nbs)

-- ---------------------------------------------------------------------------
-- Measurement / classical interaction
-- ---------------------------------------------------------------------------

-- | Eliminate a terminal COPY spider fed by a measurement.  If a
--   measurement spider's only classical neighbour is a COPY whose other
--   neighbours are all classical output boundaries, remove the COPY and wire
--   the measurement directly to those outputs.  Copying a classical outcome at
--   the diagram boundary is semantically free.
dMeasureCopy :: DoubledRule
dMeasureCopy = dSearchRule dAllVertices tryMeasure
  where
    tryMeasure m d = do
      mty <- dLookupVertex m d
      case mty of
        DMeasureZ -> Just ()
        DMeasureX -> Just ()
        _         -> Nothing
      mNb <- IM.lookup m (_cNeighborMap d)
      case M.toList mNb of
        [(c, b)] | isSingleClassical b -> do
          cty <- dLookupVertex c d
          if cty /= DCopy then Nothing else do
            cNb <- IM.lookup c (_cNeighborMap d)
            let outs = [ w | (w, bndl) <- M.toList cNb
                           , w /= m
                           , isOutputBoundary d w
                           , isSingleClassical bndl ]
            if length outs /= M.size cNb - 1 then Nothing else do
              let d1 = dRemoveVertex c d
              return $ foldl (\acc w -> dAddEdge m w (DSimple Classical) acc) d1 outs
        _ -> Nothing

    isSingleClassical b =
      cSimpleCount b == 1 && qSimpleCount b == 0 && qHadamardCount b == 0

    isOutputBoundary d v =
      v `elem` _dClassicalOutputs d &&
      case dLookupVertex v d of
        Just (DBoundary _) -> True
        _                  -> False
