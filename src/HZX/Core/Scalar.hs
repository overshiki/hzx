{-# LANGUAGE DeriveGeneric #-}

module HZX.Core.Scalar
  ( Scalar(..)
  , scalarOne
  , scalarZero
  , mulScalar
  , divScalar
  , sqrt2Pow
  , phaseFactor
  , scalarToComplex
  , scalarToString
  ) where

import Data.Complex (Complex(..))
import Data.Ratio (numerator, denominator)

-- | A scalar factor in ZX-calculus.
--   Represented as (2^(k/2)) * e^(iπφ) where k is an integer and φ is rational.
--   We store this as separate components to enable exact arithmetic.
data Scalar = Scalar
  { sqrt2Power :: !Int       -- ^ Power of √2: 2^(k/2) means k here
  , phase :: !Rational       -- ^ Phase factor: e^(iπ * phase)
  , scalarIsZero :: !Bool    -- ^ Distinguishes the true zero scalar from 1
  } deriving (Show)

instance Eq Scalar where
  s1 == s2
    | scalarIsZero s1 && scalarIsZero s2 = True
    | scalarIsZero s1 || scalarIsZero s2 = False
    | otherwise = sqrt2Power s1 == sqrt2Power s2 && phase s1 == phase s2

-- | The scalar 1.
scalarOne :: Scalar
scalarOne = Scalar 0 0 False

-- | The scalar 0 (for invalid/undefined diagrams).
scalarZero :: Scalar
scalarZero = Scalar 0 0 True

-- | Multiply two scalars.
mulScalar :: Scalar -> Scalar -> Scalar
mulScalar s1 s2
  | scalarIsZero s1 || scalarIsZero s2 = scalarZero
  | otherwise =
      Scalar (sqrt2Power s1 + sqrt2Power s2)
             (normalizePhase (phase s1 + phase s2))
             False

-- | Divide two scalars.
divScalar :: Scalar -> Scalar -> Scalar
divScalar s1 s2
  | scalarIsZero s1 = scalarZero
  | scalarIsZero s2 = scalarZero  -- 0/0 is also represented as zero here
  | otherwise =
      Scalar (sqrt2Power s1 - sqrt2Power s2)
             (normalizePhase (phase s1 - phase s2))
             False

-- | Normalize phase to [0, 2) range.
normalizePhase :: Rational -> Rational
normalizePhase p = let p' = p `mod'` 2 in if p' < 0 then p' + 2 else p'
  where
    mod' a b = a - fromInteger (floor (fromRational a / fromRational b :: Double)) * b

-- | Create a scalar that is a power of √2.
--   sqrt2Pow n represents (√2)^n = 2^(n/2)
sqrt2Pow :: Int -> Scalar
sqrt2Pow n = Scalar n 0 False

-- | Create a scalar that is a phase factor e^(iπ * r).
phaseFactor :: Rational -> Scalar
phaseFactor r = Scalar 0 (normalizePhase r) False

-- | Convert a scalar to a complex number.
scalarToComplex :: Scalar -> Complex Double
scalarToComplex s
  | scalarIsZero s = 0.0 :+ 0.0
  | otherwise =
      let sqrt2 = sqrt 2.0
          sqrt2Factor = sqrt2 ^^ sqrt2Power s  -- (√2)^k
          phaseAngle = pi * fromRational (phase s)
          phaseReal = cos phaseAngle
          phaseImag = sin phaseAngle
      in (sqrt2Factor * phaseReal) :+ (sqrt2Factor * phaseImag)
  where
    x ^^ n = if n >= 0 then x ^ n else 1.0 / (x ^ (-n))

-- | Convert a scalar to a readable string.
scalarToString :: Scalar -> String
scalarToString s
  | scalarIsZero s = "0"
  | isOne = "1"
  | isSqrt2 && isPhaseOne = "√2" ++ powStr k
  | isSqrt2 = "√2" ++ powStr k ++ " · " ++ phaseStr p
  | isPhaseOne && k `mod` 2 == 0 = "2" ++ powStr (k `div` 2)
  | isPhaseOne = "2" ++ powStr (k `div` 2) ++ "·√2"
  | k `mod` 2 == 0 = phaseStr p ++ " · 2" ++ powStr (k `div` 2)
  | otherwise = phaseStr p ++ " · √2" ++ powStr k ++ " · 2" ++ powStr (k `div` 2)
  where
    k = sqrt2Power s
    p = phase s
    isOne = k == 0 && p == 0 && not (scalarIsZero s)
    isSqrt2 = k /= 0
    isPhaseOne = p == 0
    
    powStr n
      | n == 0 = ""
      | n == 1 = ""
      | n > 0 = "^" ++ show n
      | otherwise = "^(" ++ show n ++ ")"
    
    phaseStr r
      | r == 0 = "1"
      | r == 1 = "-1"
      | r == 1/2 = "i"
      | r == (-1/2) = "-i"
      | r == 1/4 = "(1+i)/√2"
      | r == (-1/4) = "(1-i)/√2"
      | r == 3/4 = "(-1+i)/√2"
      | r == (-3/4) = "(-1-i)/√2"
      | numerator r == 1 = "e^(iπ/" ++ show (denominator r) ++ ")"
      | numerator r == -1 = "e^(-iπ/" ++ show (denominator r) ++ ")"
      | otherwise = "e^(iπ·" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"
