module HZX.Circuit
  ( MeasurementBasis(..)
  , Gate(..)
  , Circuit(..)
  , numQubits
  , finallyMeasuredQubits
  ) where

import qualified Data.Set as S

-- | Basis for measurement and reset operations.
data MeasurementBasis
  = XBasis   -- ^ X basis (|+⟩, |-⟩)
  | ZBasis   -- ^ Z basis (|0⟩, |1⟩)
  deriving (Eq, Show, Ord)

-- | Primitive quantum gates supported by HZX.
--
--   * 'Measure' terminates a qubit wire at a boundary.
--   * 'Reset' discards the current state of a qubit and prepares a fresh
--     basis state (modelling a STIM reset / collapsing gate).
data Gate
  = H Int
  | S Int
  | T Int
  | ZPhase Int Rational
  | XPhase Int Rational
  | CNOT Int Int
  | CZ Int Int
  | SWAP Int Int
  | Measure MeasurementBasis Int
  | Reset MeasurementBasis Int
  deriving (Eq, Show)

newtype Circuit = Circuit { gates :: [Gate] }
  deriving (Eq, Show)

-- | Number of qubits referenced by the circuit.
--
--   This is one past the largest qubit index appearing in any gate, so
--   measured or reset qubits still contribute to the overall qubit count.
numQubits :: Circuit -> Int
numQubits (Circuit gs) = foldl maxQubit 0 gs
  where
    maxQubit n (H q)        = max n (q + 1)
    maxQubit n (S q)        = max n (q + 1)
    maxQubit n (T q)        = max n (q + 1)
    maxQubit n (ZPhase q _) = max n (q + 1)
    maxQubit n (XPhase q _) = max n (q + 1)
    maxQubit n (CNOT c t)   = max n (max c t + 1)
    maxQubit n (CZ q1 q2)   = max n (max q1 q2 + 1)
    maxQubit n (SWAP q1 q2) = max n (max q1 q2 + 1)
    maxQubit n (Measure _ q) = max n (q + 1)
    maxQubit n (Reset _ q)  = max n (q + 1)

-- | Set of qubit indices whose final operation in the circuit is a
-- measurement.
--
--   Such qubits do not receive a final output boundary in the corresponding
--   ZX diagram.  If a qubit is measured and later reset, it is removed from
--   this set because the reset re-introduces an active wire that can again
--   produce an output boundary.
finallyMeasuredQubits :: Circuit -> S.Set Int
finallyMeasuredQubits (Circuit gs) = go S.empty gs
  where
    go s [] = s
    go s (Measure _ q : rest) = go (S.insert q s) rest
    go s (Reset _ q : rest)   = go (S.delete q s) rest
    go s (_ : rest)           = go s rest
