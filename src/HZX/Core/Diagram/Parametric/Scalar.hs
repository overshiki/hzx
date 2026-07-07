-- | Scalar arithmetic for parametric ZX diagrams.
module HZX.Core.Diagram.Parametric.Scalar
  ( paramScalarOne
  , paramScalarZero
  , mulParamScalar
  , divParamScalar
  , sqrt2ParamPow
  , paramPhaseFactor
  , evalParamScalar
  ) where


import qualified Data.Map as M

import qualified HZX.Core.Scalar as S

import HZX.Core.Diagram.Parametric.Types (ParamScalar(..), ParamPhase(..))
import HZX.Core.Diagram.Parametric.Phase
  ( addParamPhase, negateParamPhase, evalParamPhase )

-- | The parametric scalar 1.
paramScalarOne :: ParamScalar
paramScalarOne = ParamScalar 0 (ParamPhase 0 M.empty)

-- | The parametric scalar 0 (marker for invalid/undefined diagrams).
paramScalarZero :: ParamScalar
paramScalarZero = ParamScalar 0 (ParamPhase 0 M.empty)

-- | Multiply two parametric scalars.
mulParamScalar :: ParamScalar -> ParamScalar -> ParamScalar
mulParamScalar (ParamScalar k1 p1) (ParamScalar k2 p2) =
  ParamScalar (k1 + k2) (addParamPhase p1 p2)

-- | Divide two parametric scalars.
divParamScalar :: ParamScalar -> ParamScalar -> ParamScalar
divParamScalar (ParamScalar k1 p1) (ParamScalar k2 p2) =
  ParamScalar (k1 - k2) (addParamPhase p1 (negateParamPhase p2))

-- | Create a power of √2.
sqrt2ParamPow :: Int -> ParamScalar
sqrt2ParamPow k = ParamScalar k (ParamPhase 0 M.empty)

-- | Create a phase factor e^(iπ * phase).
paramPhaseFactor :: Rational -> ParamScalar
paramPhaseFactor r = ParamScalar 0 (ParamPhase r M.empty)

-- | Evaluate a parametric scalar under a concrete parameter assignment.
evalParamScalar :: M.Map String Bool -> ParamScalar -> S.Scalar
evalParamScalar env (ParamScalar k p) =
  S.Scalar k (evalParamPhase env p)
