{-# LANGUAGE TupleSections #-}

-- | Computation of focused generalized flow (gflow) on graph-like ZX diagrams.
--
--   A focused gflow determines an order in which the interior spiders of a
--   graph-like diagram can be measured / extracted.  Extraction is only
--   possible when such a flow exists.
module HZX.Circuit.Extraction.Flow
  ( focusedGFlow
  ) where

import Data.Bits (setBit, testBit)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

import HZX.Core.Diagram
import qualified HZX.LinAlg.Z2 as Z2

import HZX.Circuit.Extraction.Types (GFlow(..))

-- | Compute a focused gflow for a graph-like ZX diagram.
--
--   The algorithm works backwards from the output boundaries.  At each step
--   it looks for a non-frontier vertex @v@ whose neighbours (other than @v@
--   itself) are already on the frontier.  If such a vertex exists, it solves
--   a small Z2 linear system to find a correction set @U@ contained in the
--   frontier with @v \u2208 U@ and @Odd(U) \u2229 (V \\ frontier) = {v}@.
--
--   If no such vertex can be found, the diagram has no focused gflow and
--   extraction is not possible with this method.
focusedGFlow :: Diagram -> Maybe GFlow
focusedGFlow d =
  let initFrontier = S.fromList (outputs d)
  in go initFrontier IM.empty IM.empty
  where
    allVerts = S.fromList (allVertices d)

    go :: S.Set Vertex -> IM.IntMap Int -> IM.IntMap (M.Map Vertex ()) -> Maybe GFlow
    go frontier order corrections
      | frontier == allVerts = Just (GFlow order corrections)
      | otherwise =
          case findCandidate frontier of
            Nothing -> Nothing
            Just (v, corrSet) ->
              let frontier' = S.insert v frontier
                  order'    = IM.insert v (S.size frontier') order
                  corrMap   = M.fromList [(u, ()) | u <- corrSet]
                  corrections' = IM.insert v corrMap corrections
              in go frontier' order' corrections'

    -- Find a vertex not on the frontier whose neighbours (except itself) are
    -- all on the frontier, and for which a correction set exists.
    findCandidate :: S.Set Vertex -> Maybe (Vertex, [Vertex])
    findCandidate frontier = listToMaybe candidates
      where
        candidates = mapMaybe (tryVertex frontier) (S.toList (allVerts S.\\ frontier))

    tryVertex :: S.Set Vertex -> Vertex -> Maybe (Vertex, [Vertex])
    tryVertex frontier v = do
      let nbs = neighbors v d
      if all (\w -> w == v || w `S.member` frontier) nbs
        then do
          corrSet <- findCorrectionSet frontier v
          return (v, corrSet)
        else Nothing

    -- Solve for a correction set U \subseteq frontier such that:
    --   * v \in U
    --   * for every w \notin frontier, w /= v: |N(w) \cap U| is even.
    findCorrectionSet :: S.Set Vertex -> Vertex -> Maybe [Vertex]
    findCorrectionSet frontier v = do
      let frontierList = S.toList frontier
          frontierIdx  = IM.fromList (zip frontierList [0..])
          outside      = S.toList (allVerts S.\\ frontier)
          equations    = [ (w, equationRow frontierList w) | w <- outside, w /= v ]
          vIdx         = IM.lookup v frontierIdx

      vCol <- vIdx

      let n = S.size frontier
          -- A is rows x n matrix; b is the right-hand side vector.
          -- The equation for w is: (N(w) \cap U) even, i.e. sum = 0.
          -- Because v is forced into U, move v's contribution to RHS.
          aRows = [ rowBits | (_, rowBits) <- equations ]
          bVec  = [ if testBit rowBits vCol then 1 else 0
                  | (_, rowBits) <- equations ]
          aMat  = Z2.fromLists [ [ if testBit r j then 1 else 0 | j <- [0..n-1] ] | r <- aRows ]

      solution <- Z2.solveLinearSystem aMat bVec n

      let setBits = setBit (toBits solution) vCol
          corrSet = [ u | (u, idx) <- IM.toList frontierIdx, testBit setBits idx ]
      return corrSet
      where
        toBits :: [Int] -> Integer
        toBits xs = foldl (\acc (i, x) -> if x == 1 then setBit acc i else acc) 0 (zip [0..] xs)

    -- Build the row of the linear system for outside vertex w.
    -- Bit i is 1 iff frontier vertex i is adjacent to w.
    equationRow :: [Vertex] -> Vertex -> Integer
    equationRow frontierList w =
      foldl (\acc (u, idx) -> if areConnected u w d then setBit acc idx else acc) 0 (zip frontierList [0..])
