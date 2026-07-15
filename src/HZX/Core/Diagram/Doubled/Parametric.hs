-- | Doubled diagrams with parametric phases.
--
--   A "doubled parametric diagram" is just a 'DoubledDiagram': the DZ and DX
--   spiders already carry 'ParamPhase', so the doubled representation already
--   supports symbolic parameters.  This module exposes that fact and provides
--   the small amount of plumbing needed to instantiate parameters and to
--   evaluate the diagram scalar under a concrete assignment.
module HZX.Core.Diagram.Doubled.Parametric
  ( DoubledParametricDiagram
  , parametricCircuitToDoubledDiagram
  , instantiateDoubledDiagram
  , evalDoubledScalar
  ) where

import qualified Data.IntMap as IM
import qualified Data.Map as M

import qualified HZX.Core.Scalar as S
import HZX.Circuit.Parametric (ParametricCircuit)
import HZX.Circuit.ToDoubledDiagram (circuitToDoubledDiagram)
import HZX.Core.Diagram.Parametric.Phase (evalParamPhase)
import HZX.Core.Diagram.Parametric.Scalar (evalParamScalar)
import HZX.Core.Diagram.Parametric.Types (ParamPhase(..), ParamScalar(..))
import HZX.Core.Diagram.Doubled.Types
  ( DoubledDiagram(..), DoubledVertexType(..) )

-- | A type synonym that makes explicit the fact that 'DoubledDiagram' already
--   supports parametric phases.  There is no need for a separate combined
--   type: quantum spiders carry 'ParamPhase' and classical spiders are
--   parameter-free.
type DoubledParametricDiagram = DoubledDiagram

-- | Convert a parametric circuit into a doubled parametric diagram.
--
--   This is simply an alias for 'circuitToDoubledDiagram', but with a type
--   that advertises the parametric nature of the result.
parametricCircuitToDoubledDiagram :: ParametricCircuit -> DoubledParametricDiagram
parametricCircuitToDoubledDiagram = circuitToDoubledDiagram

-- | Instantiate all symbolic parameters in a doubled diagram, producing a
--   doubled diagram whose quantum phases are concrete (their parameter maps
--   are empty).
--
--   Missing parameters are treated as @False@ (value 0), matching the
--   behaviour of 'evalParamPhase'.
instantiateDoubledDiagram :: M.Map String Bool -> DoubledParametricDiagram -> DoubledDiagram
instantiateDoubledDiagram env d =
  d { _dVertices = IM.map instantiateVertex (_dVertices d)
    , _dScalar   = instantiateScalar (_dScalar d)
    }
  where
    instantiateVertex :: DoubledVertexType -> DoubledVertexType
    instantiateVertex (DZ p) = DZ (concretizePhase p)
    instantiateVertex (DX p) = DX (concretizePhase p)
    instantiateVertex v      = v

    concretizePhase :: ParamPhase -> ParamPhase
    concretizePhase p = ParamPhase (evalParamPhase env p) M.empty

    instantiateScalar :: ParamScalar -> ParamScalar
    instantiateScalar (ParamScalar k p) = ParamScalar k (concretizePhase p)

-- | Evaluate the diagram's scalar factor under a concrete parameter
--   assignment.
evalDoubledScalar :: M.Map String Bool -> DoubledDiagram -> S.Scalar
evalDoubledScalar env = evalParamScalar env . _dScalar
