module HZX.Core.Phase
  ( Phase
  , addPhase
  , negatePhase
  , phaseToString
  ) where

import Data.Ratio (Rational, denominator, numerator)

-- | A phase is an exact rational multiple of π.
--   For example, Phase (1 % 2) represents π/2.
type Phase = Rational

addPhase :: Phase -> Phase -> Phase
addPhase = (+)

negatePhase :: Phase -> Phase
negatePhase = negate

phaseToString :: Phase -> String
phaseToString p
  | n == 0    = "0"
  | d == 1    = show n ++ "π"
  | otherwise = show n ++ "/" ++ show d ++ "π"
  where
    n = numerator p
    d = denominator p
