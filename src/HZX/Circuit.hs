module HZX.Circuit
  ( Gate(..)
  , Circuit(..)
  , numQubits
  ) where

import Data.Ratio (Rational)

data Gate
  = H Int
  | S Int
  | T Int
  | ZPhase Int Rational
  | XPhase Int Rational
  | CNOT Int Int
  | CZ Int Int
  | SWAP Int Int
  deriving (Eq, Show)

newtype Circuit = Circuit { gates :: [Gate] }
  deriving (Eq, Show)

numQubits :: Circuit -> Int
numQubits (Circuit gs) = foldl maxQubit 0 gs
  where
    maxQubit n (H q)       = max n (q + 1)
    maxQubit n (S q)       = max n (q + 1)
    maxQubit n (T q)       = max n (q + 1)
    maxQubit n (ZPhase q _) = max n (q + 1)
    maxQubit n (XPhase q _) = max n (q + 1)
    maxQubit n (CNOT c t)  = max n (max c t + 1)
    maxQubit n (CZ q1 q2)  = max n (max q1 q2 + 1)
    maxQubit n (SWAP q1 q2) = max n (max q1 q2 + 1)
