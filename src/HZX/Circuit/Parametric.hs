-- | Circuits augmented with parametric Pauli noise channels.
--
--   This module extends HZX's gate-based 'Circuit' with classical noise
--   channels.  Operations (gates and noise) are stored in execution order so
--   that noise channels can be inserted at the correct frontier positions
--   when building a parametric ZX diagram.
module HZX.Circuit.Parametric
  ( NoiseChannel(..)
  , ParametricOp(..)
  , ParametricCircuit(..)
  , pcNumQubits
  ) where

import HZX.Circuit (Gate(..))

-- | A Pauli noise channel represented as a set of binary parameters.
--
--   For each parameter, the associated probability is the probability that
--   the parameter takes the value 1 (i.e., that the corresponding Pauli
--   error is applied).
data NoiseChannel
  = XError String Int Double        -- ^ param name, qubit, probability
  | ZError String Int Double        -- ^ param name, qubit, probability
  | Depolarize1 String String Int Double Double Double
      -- ^ param names for X and Z errors, qubit, probabilities px, py, pz
  deriving (Eq, Show)

-- | A single operation in a parametric circuit.
data ParametricOp
  = POGate Gate
  | PONoise NoiseChannel
  deriving (Eq, Show)

-- | A circuit together with parametric noise channels, in execution order.
newtype ParametricCircuit = ParametricCircuit
  { pcOps :: [ParametricOp]
  } deriving (Eq, Show)

-- | Number of qubits referenced by the circuit or its noise channels.
pcNumQubits :: ParametricCircuit -> Int
pcNumQubits (ParametricCircuit ops) = foldl maxOpQubit 0 ops
  where
    maxOpQubit n (POGate g)   = maxGateQubit n g
    maxOpQubit n (PONoise nc) = maxNoiseQubit n nc

    maxNoiseQubit n (XError _ q _) = max n (q + 1)
    maxNoiseQubit n (ZError _ q _) = max n (q + 1)
    maxNoiseQubit n (Depolarize1 _ _ q _ _ _) = max n (q + 1)

    maxGateQubit n (H q)        = max n (q + 1)
    maxGateQubit n (S q)        = max n (q + 1)
    maxGateQubit n (T q)        = max n (q + 1)
    maxGateQubit n (ZPhase q _) = max n (q + 1)
    maxGateQubit n (XPhase q _) = max n (q + 1)
    maxGateQubit n (CNOT c t)   = max n (max c t + 1)
    maxGateQubit n (CZ q1 q2)   = max n (max q1 q2 + 1)
    maxGateQubit n (SWAP q1 q2) = max n (max q1 q2 + 1)
    maxGateQubit n (Measure _ q) = max n (q + 1)
    maxGateQubit n (Reset _ q)  = max n (q + 1)
