module HZX.Rewrite.Matcher where

import HZX.Core.Diagram

-- | Abstract matcher interface.
-- Future Stage 3 may swap this for an e-hypergraph matcher.
type Match = Diagram -> [(Vertex, Vertex)]  -- placeholder
