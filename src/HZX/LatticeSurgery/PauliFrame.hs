{-# LANGUAGE TupleSections #-}

module HZX.LatticeSurgery.PauliFrame
  ( PauliFrame
  , emptyFrame
  , addX
  , addZ
  , applyPauli
  , combineFrames
  , toCorrection
  ) where

import Data.Bits (xor)

-- | A Pauli frame tracks accumulated Pauli operators on logical qubits.
-- In lattice surgery, measurement outcomes may require Pauli corrections.
-- Rather than applying these physically, we track them classically.
data PauliFrame = PauliFrame
  { xBits :: !Int     -- Bit i = 1 means X correction needed on qubit i
  , zBits :: !Int     -- Bit i = 1 means Z correction needed on qubit i
  } deriving (Eq, Show)

-- | Empty Pauli frame (no corrections needed).
emptyFrame :: PauliFrame
emptyFrame = PauliFrame 0 0

-- | Add an X correction for a given qubit.
addX :: Int -> PauliFrame -> PauliFrame
addX q f = f { xBits = xBits f `xor` (1 `shiftL` q) }
  where
    shiftL = unsafeShiftL
    unsafeShiftL x n = x * (2^n)

-- | Add a Z correction for a given qubit.
addZ :: Int -> PauliFrame -> PauliFrame
addZ q f = f { zBits = zBits f `xor` (1 `shiftL` q) }
  where
    shiftL = unsafeShiftL
    unsafeShiftL x n = x * (2^n)

-- | Check if X correction is needed for a qubit.
needsX :: Int -> PauliFrame -> Bool
needsX q f = (xBits f `div` (2^q)) `mod` 2 == 1

-- | Check if Z correction is needed for a qubit.
needsZ :: Int -> PauliFrame -> Bool
needsZ q f = (zBits f `div` (2^q)) `mod` 2 == 1

-- | Apply a Pauli operator to the frame.
-- For lattice surgery outcomes, we track corrections needed.
applyPauli :: Int -> Bool -> Bool -> PauliFrame -> PauliFrame
applyPauli q needsXCorr needsZCorr f =
  let f1 = if needsXCorr then addX q f else f
      f2 = if needsZCorr then addZ q f1 else f1
  in f2

-- | Combine two Pauli frames (XOR of their corrections).
combineFrames :: PauliFrame -> PauliFrame -> PauliFrame
combineFrames f1 f2 = PauliFrame
  { xBits = xBits f1 `xor` xBits f2
  , zBits = zBits f1 `xor` zBits f2
  }

-- | Convert a Pauli frame to a list of (qubit, needsX, needsZ).
toCorrection :: PauliFrame -> [(Int, Bool, Bool)]
toCorrection f = 
  let maxQ = max (bitLength (xBits f)) (bitLength (zBits f)) - 1
  in [(q, needsX q f, needsZ q f) | q <- [0..maxQ], needsX q f || needsZ q f]
  where
    bitLength 0 = 0
    bitLength n = 1 + bitLength (n `div` 2)
