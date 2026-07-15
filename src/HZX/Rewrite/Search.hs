-- | Common search patterns for concrete ZX rewrite rules.
--
--   This module is a thin specialization of 'HZX.Rewrite.Generic' to the
--   concrete 'Diagram' type.  The underlying combinators are shared with the
--   parametric and doubled layers.
module HZX.Rewrite.Search
  ( vertexRule
  , edgeRule
  , pairRule
  , searchRule
  ) where

import HZX.Core.Diagram (Diagram, VertexType, EdgeBundle, Vertex, EdgeKey)
import HZX.Rewrite.Generic (Rule)
import qualified HZX.Rewrite.Generic as G

-- | Search over vertices and return the first match.
vertexRule :: (Vertex -> VertexType -> Diagram -> Maybe Diagram) -> Rule Diagram
vertexRule = G.vertexRule

-- | Search over edges and return the first match.
edgeRule :: (EdgeKey -> EdgeBundle -> Diagram -> Maybe Diagram) -> Rule Diagram
edgeRule = G.edgeRule

-- | Search over unordered vertex pairs and return the first match.
pairRule :: (Vertex -> Vertex -> Diagram -> Maybe Diagram) -> Rule Diagram
pairRule = G.pairRule

-- | Generic first-match search over a user-supplied list of candidates.
searchRule :: (Diagram -> [a]) -> (a -> Diagram -> Maybe Diagram) -> Rule Diagram
searchRule = G.searchRule
