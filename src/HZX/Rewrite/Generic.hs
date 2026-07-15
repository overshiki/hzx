{-# LANGUAGE TypeFamilies #-}

-- | Generic, layer-independent rewrite-rule combinators.
--
--   This module abstracts the *plumbing* of rewrite rules (searching for a
--   match, composing rules, running to fixpoint) without changing the concrete
--   diagram types.  Each diagram layer provides a 'Rewritable' instance that
--   tells the generic combinators how to list vertices, look up vertex types,
--   and list edge bundles.
module HZX.Rewrite.Generic
  ( -- * Rule and strategy types
    Rule
  , Strategy
    -- * Rewritable diagrams
  , Rewritable(..)
    -- * Rule combinators
  , (>+>)
  , (<+>)
  , try
  , fixpoint
  , runStrategy
    -- * Generic search combinators
  , vertexRule
  , edgeRule
  , pairRule
  , searchRule
  ) where

import Data.Maybe (listToMaybe, mapMaybe)
import Control.Applicative ((<|>))

import HZX.Core.Diagram.Types (Vertex, EdgeKey)

-- | A rewrite rule on a diagram type @d@.  Rules are partial functions:
--   'Nothing' means the rule did not apply.
type Rule d = d -> Maybe d

-- | A simplification strategy on a diagram type @d@.  Strategies are total
--   functions that produce a (possibly unchanged) diagram.
type Strategy d = d -> d

-- | Interface that a diagram type must support in order to use the generic
--   search combinators.
class Rewritable d where
  -- | Layer-specific vertex type (e.g. 'VertexType', 'ParamVertexType',
  --   'DoubledVertexType').
  type RVertex d

  -- | Layer-specific edge bundle type (e.g. 'EdgeBundle',
  --   'DoubledEdgeBundle').
  type REdge d

  -- | All vertices in the diagram, in the order used for search.
  rAllVertices :: d -> [Vertex]

  -- | Look up the type of a vertex.
  rLookupVertex :: Vertex -> d -> Maybe (RVertex d)

  -- | All edge bundles in the diagram.
  rAllEdges :: d -> [(EdgeKey, REdge d)]

infixr 3 <+>
infixr 2 >+>

-- | Left-biased choice between two rules.
(<+>) :: Rule d -> Rule d -> Rule d
(f <+> g) d = f d <|> g d

-- | Sequential composition of rules: apply the first, then the second to the
--   result.
(>+>) :: Rule d -> Rule d -> Rule d
(f >+> g) d = f d >>= g

-- | Make a rule total: if it fails, return the input diagram unchanged.
try :: Rule d -> Rule d
try r = r <+> Just

-- | Repeatedly apply a rule until it no longer applies.
fixpoint :: Rule d -> Strategy d
fixpoint r d = maybe d (fixpoint r) (r d)

-- | Synonym for 'fixpoint'.
runStrategy :: Rule d -> Strategy d
runStrategy = fixpoint

-- | Search over vertices and return the first match.
vertexRule :: Rewritable d => (Vertex -> RVertex d -> d -> Maybe d) -> Rule d
vertexRule matcher d = go (rAllVertices d)
  where
    go [] = Nothing
    go (v : vs) =
      case rLookupVertex v d of
        Nothing -> go vs
        Just ty ->
          case matcher v ty d of
            Just d' -> Just d'
            Nothing -> go vs

-- | Search over edges and return the first match.
edgeRule :: Rewritable d => (EdgeKey -> REdge d -> d -> Maybe d) -> Rule d
edgeRule matcher d =
  listToMaybe $ mapMaybe (\(k, b) -> matcher k b d) (rAllEdges d)

-- | Search over unordered vertex pairs and return the first match.
pairRule :: Rewritable d => (Vertex -> Vertex -> d -> Maybe d) -> Rule d
pairRule matcher d =
  listToMaybe $ mapMaybe (\(v1, v2) -> matcher v1 v2 d) (allVertexPairs d)
  where
    allVertexPairs d' =
      let vs = rAllVertices d'
      in [(v1, v2) | v1 <- vs, v2 <- vs, v1 < v2]

-- | Generic first-match search over a user-supplied list of candidates.
searchRule :: Rewritable d => (d -> [a]) -> (a -> d -> Maybe d) -> Rule d
searchRule candidates matcher d =
  listToMaybe $ mapMaybe (\x -> matcher x d) (candidates d)
