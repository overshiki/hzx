{-# LANGUAGE TupleSections #-}

-- | Configuration and Reader monad for the circuit extractor.
module HZX.Circuit.Extraction.Config
  ( ExtractionMode(..)
  , ExtractionConfig(..)
  , defaultExtractionConfig
  , ExtractM
  , runExtractM
  , throwExtract
  , askConfig
  , askMode
  , askMaxIterations
  ) where

import Control.Monad.Reader

-- | Strategy used to extract a circuit from a graph-like ZX diagram.
--
--   * 'FrontierMode' is the fast, default algorithm based on a frontier and
--     GF(2) Gaussian elimination (the algorithm used by PyZX).
--   * 'GFlowMode' follows the focused generalized flow directly, applying the
--     precomputed correction sets as CNOTs.  It is slower but conceptually
--     simpler and useful for education / validation.
data ExtractionMode
  = FrontierMode
  | GFlowMode
  deriving (Eq, Show)

-- | Hyper-parameters for the extractor.
data ExtractionConfig = ExtractionConfig
  { ecMode          :: !ExtractionMode
  , ecMaxIterations :: !Int
  } deriving (Eq, Show)

-- | Default configuration: frontier-based extraction with a 100-iteration
--   safety limit.
defaultExtractionConfig :: ExtractionConfig
defaultExtractionConfig = ExtractionConfig
  { ecMode          = FrontierMode
  , ecMaxIterations = 100
  }

-- | Extraction monad: read-only access to the config with 'Either String'
--   failure.
type ExtractM a = ReaderT ExtractionConfig (Either String) a

-- | Run an extraction computation with the supplied configuration.
runExtractM :: ExtractionConfig -> ExtractM a -> Either String a
runExtractM cfg m = runReaderT m cfg

-- | Fail the extraction with an error message.
throwExtract :: String -> ExtractM a
throwExtract = lift . Left

-- | Read the full extraction configuration.
askConfig :: ExtractM ExtractionConfig
askConfig = ask

-- | Read the extraction mode.
askMode :: ExtractM ExtractionMode
askMode = asks ecMode

-- | Read the maximum iteration limit.
askMaxIterations :: ExtractM Int
askMaxIterations = asks ecMaxIterations
