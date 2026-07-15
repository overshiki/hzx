-- | Common search patterns for parametric ZX rewrite rules.
--
--   This module is a thin specialization of 'HZX.Rewrite.Generic' to the
--   parametric 'ParamDiagram' type.
module HZX.Core.Diagram.Parametric.Search
  ( pVertexRule
  , pEdgeRule
  , pPairRule
  , pSearchRule
  ) where

import HZX.Core.Diagram.Parametric.Instances ()
import HZX.Core.Diagram.Parametric.Types (ParamDiagram, ParamVertexType)
import HZX.Core.Diagram.Types (Vertex, EdgeKey, EdgeBundle)
import HZX.Rewrite.Generic (Rule)
import qualified HZX.Rewrite.Generic as G

-- | Search over vertices and return the first match.
pVertexRule :: (Vertex -> ParamVertexType -> ParamDiagram -> Maybe ParamDiagram)
            -> Rule ParamDiagram
pVertexRule = G.vertexRule

-- | Search over edges and return the first match.
pEdgeRule :: (EdgeKey -> EdgeBundle -> ParamDiagram -> Maybe ParamDiagram)
          -> Rule ParamDiagram
pEdgeRule = G.edgeRule

-- | Search over unordered vertex pairs and return the first match.
pPairRule :: (Vertex -> Vertex -> ParamDiagram -> Maybe ParamDiagram)
          -> Rule ParamDiagram
pPairRule = G.pairRule

-- | Generic first-match search over a user-supplied list of candidates.
pSearchRule :: (ParamDiagram -> [a]) -> (a -> ParamDiagram -> Maybe ParamDiagram)
            -> Rule ParamDiagram
pSearchRule = G.searchRule
