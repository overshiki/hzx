-- | Common search patterns for doubled ZX rewrite rules.
--
--   This module is a thin specialization of 'HZX.Rewrite.Generic' to the
--   doubled 'DoubledDiagram' type.
module HZX.Core.Diagram.Doubled.Search
  ( dVertexRule
  , dPairRule
  , dSearchRule
  ) where

import HZX.Core.Diagram.Doubled.Instances ()
import HZX.Core.Diagram.Doubled.Types (DoubledDiagram, DoubledVertexType)
import HZX.Core.Diagram.Types (Vertex)
import HZX.Rewrite.Generic (Rule)
import qualified HZX.Rewrite.Generic as G

-- | Search over vertices and return the first match.
dVertexRule :: (Vertex -> DoubledVertexType -> DoubledDiagram -> Maybe DoubledDiagram)
            -> Rule DoubledDiagram
dVertexRule = G.vertexRule

-- | Search over unordered vertex pairs and return the first match.
dPairRule :: (Vertex -> Vertex -> DoubledDiagram -> Maybe DoubledDiagram)
          -> Rule DoubledDiagram
dPairRule = G.pairRule

-- | Generic first-match search over a user-supplied list of candidates.
dSearchRule :: (DoubledDiagram -> [a]) -> (a -> DoubledDiagram -> Maybe DoubledDiagram)
            -> Rule DoubledDiagram
dSearchRule = G.searchRule
