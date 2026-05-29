{-# LANGUAGE TupleSections #-}

module HZX.LatticeSurgery.Heralded
  ( Heralded(..)
  , Outcome(..)
  , runHeralded
  , mapHeralded
  , sequenceHeralded
  ) where

import Data.Ratio (Rational, (%))

-- | Measurement outcome for lattice surgery merges.
data Outcome = Positive | Negative
  deriving (Eq, Show)

-- | A heralded operation produces different results based on measurement outcome.
-- This captures the non-deterministic nature of lattice surgery merges.
data Heralded a = Heralded
  { positiveBranch :: a           -- ^ Result when measurement outcome is +1
  , negativeBranch :: a           -- ^ Result when measurement outcome is -1
  , outcomeProb    :: Rational    -- ^ Probability of +1 outcome (typically 1/2)
  } deriving (Eq, Show)

-- | Functor instance for Heralded.
instance Functor Heralded where
  fmap f h = Heralded
    { positiveBranch = f (positiveBranch h)
    , negativeBranch = f (negativeBranch h)
    , outcomeProb = outcomeProb h
    }

-- | Applicative instance for Heralded.
instance Applicative Heralded where
  pure x = Heralded x x 1
  hf <*> hx = Heralded
    { positiveBranch = positiveBranch hf (positiveBranch hx)
    , negativeBranch = negativeBranch hf (negativeBranch hx)
    , outcomeProb = outcomeProb hf * outcomeProb hx
    }

-- | Monad instance for Heralded (sequential composition).
instance Monad Heralded where
  return = pure
  h >>= f = Heralded
    { positiveBranch = positiveBranch (f (positiveBranch h))
    , negativeBranch = negativeBranch (f (negativeBranch h))
    , outcomeProb = outcomeProb h * outcomeProb (f (positiveBranch h))
    }

-- | Run a heralded operation, selecting branch based on outcome.
runHeralded :: Heralded a -> Outcome -> a
runHeralded h Positive = positiveBranch h
runHeralded h Negative = negativeBranch h

-- | Map over both branches.
mapHeralded :: (a -> b) -> (a -> b) -> Heralded a -> Heralded b
mapHeralded fPos fNeg h = Heralded
  { positiveBranch = fPos (positiveBranch h)
  , negativeBranch = fNeg (negativeBranch h)
  , outcomeProb = outcomeProb h
  }

-- | Sequence multiple heralded operations.
-- This creates all possible outcome combinations.
sequenceHeralded :: [Heralded a] -> Heralded [a]
sequenceHeralded [] = pure []
sequenceHeralded (h:hs) = do
  x <- h
  xs <- sequenceHeralded hs
  return (x:xs)


