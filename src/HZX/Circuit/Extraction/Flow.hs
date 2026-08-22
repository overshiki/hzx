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
    findCandidate frontier =
      let outside    = allVerts S.\\ frontier
          interiors  = S.filter (not . isBoundaryVertex) outside
          boundaries = S.filter isBoundaryVertex outside
      in case listToMaybe (mapMaybe (tryInterior frontier) (S.toList interiors)) of
           Just c  -> Just c
           Nothing -> if S.null interiors && not (S.null boundaries)
                      then Just (S.findMin boundaries, [])
                      else Nothing

    isBoundaryVertex :: Vertex -> Bool
    isBoundaryVertex v = case IM.lookup v (_vertices d) of
                           Just (Boundary _) -> True
                           _                 -> False

    tryInterior :: S.Set Vertex -> Vertex -> Maybe (Vertex, [Vertex])
    tryInterior frontier v = do
      corrSet <- findCorrectionSet frontier v
      return (v, corrSet)

    -- Solve for a correction set U \subseteq frontier such that:
    --   Odd(U) \cap (V \\ frontier) = {v}.
    --   Equivalently, for every outside vertex w:
    --     sum_{u in U, u~w} 1 = 1 (mod 2) iff w = v.
    findCorrectionSet :: S.Set Vertex -> Vertex -> Maybe [Vertex]
    findCorrectionSet frontier v = do
      let frontierList = S.toList frontier
          frontierIdx  = IM.fromList (zip frontierList [0..])
          outside      = S.toList (allVerts S.\\ frontier)
          equations    = [ (w, equationRow frontierList w) | w <- outside ]

      let n = S.size frontier
          aRows = [ rowBits | (_, rowBits) <- equations ]
          bVec  = [ if w == v then 1 else 0 | (w, _) <- equations ]
          aMat  = Z2.fromLists [ [ if testBit r j then 1 else 0 | j <- [0..n-1] ] | r <- aRows ]

      solution <- Z2.solveLinearSystem aMat bVec n

      let setBits = toBits solution
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
