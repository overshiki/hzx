-- | Symbolic phase arithmetic for parametric ZX diagrams.
module HZX.Core.Diagram.Parametric.Phase
  ( addParamPhase
  , negateParamPhase
  , scaleParamPhase
  , isParamZero
  , isParamPi
  , isParamPauli
  , evalParamPhase
  , paramPhaseToString
  ) where

import Data.Ratio (numerator, denominator)
import qualified Data.Map as M

import HZX.Core.Diagram.Parametric.Types (ParamPhase(..))

-- | Add two parametric phases.
addParamPhase :: ParamPhase -> ParamPhase -> ParamPhase
addParamPhase (ParamPhase c1 ps1) (ParamPhase c2 ps2) =
  ParamPhase (c1 + c2) (M.filter (/= 0) (M.unionWith (+) ps1 ps2))

-- | Negate a parametric phase.
negateParamPhase :: ParamPhase -> ParamPhase
negateParamPhase (ParamPhase c ps) = ParamPhase (-c) (M.map negate ps)

-- | Scale a parametric phase by a rational factor.
scaleParamPhase :: Rational -> ParamPhase -> ParamPhase
scaleParamPhase s (ParamPhase c ps) = ParamPhase (s * c) (M.map (s *) ps)

-- | Check whether the phase is symbolically zero.
isParamZero :: ParamPhase -> Bool
isParamZero (ParamPhase c ps) = c == 0 && M.null ps

-- | Check whether the phase is symbolically π or -π.
isParamPi :: ParamPhase -> Bool
isParamPi (ParamPhase c ps)
  | M.null ps = c == 1 || c == -1
  | otherwise = False

-- | Check whether the phase is an integer multiple of π.
--
--   For a phase to be Pauli, every parameter coefficient must be an integer
--   and the constant term must be an integer.
isParamPauli :: ParamPhase -> Bool
isParamPauli (ParamPhase c ps) =
  isInteger c && all (isInteger . snd) (M.toList ps)
  where
    isInteger r = denominator r == 1

-- | Evaluate a parametric phase under a concrete assignment of binary
--   parameters.
evalParamPhase :: M.Map String Bool -> ParamPhase -> Rational
evalParamPhase env (ParamPhase c ps) =
  c + sum [ coeff * boolToRat (M.lookup k env)
          | (k, coeff) <- M.toList ps
          ]
  where
    boolToRat (Just True)  = 1
    boolToRat (Just False) = 0
    boolToRat Nothing      = 0  -- absent parameters default to 0

-- | Pretty-print a parametric phase.
paramPhaseToString :: ParamPhase -> String
paramPhaseToString (ParamPhase c ps)
  | isParamZero (ParamPhase c ps) = "0"
  | M.null ps = ratToString c
  | c == 0    = paramTerms
  | otherwise = ratToString c ++ " + " ++ paramTerms
  where
    paramTerms = intercalate " + " (map term (M.toList ps))
    term (k, coeff)
      | coeff == 1  = k
      | coeff == -1 = "-" ++ k
      | otherwise   = "(" ++ ratToString coeff ++ ")*" ++ k

    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

    ratToString r
      | denominator r == 1 = show (numerator r) ++ "π"
      | otherwise          = show (numerator r) ++ "/" ++ show (denominator r) ++ "π"
