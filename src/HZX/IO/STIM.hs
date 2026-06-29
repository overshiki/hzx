{-# LANGUAGE LambdaCase #-}

-- | Parser and converter for STIM quantum circuit files.
--
--   This module uses the @stim-parser@ package to parse STIM source text into
--   an AST and then converts the AST into HZX's native 'Circuit' type.  The
--   conversion is intentionally conservative: common Clifford gates,
--   measurements, and resets are supported, while noise channels and classical
--   annotations are silently skipped.  Any unsupported gate causes a hard
--   failure so that the user is aware of the unsupported construct.
module HZX.IO.STIM
  ( parseSTIM
  , stimToCircuit
  ) where

import Control.Monad (replicateM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Text.Megaparsec (runParser, errorBundlePretty)

import qualified StimParser.Expr as SE
import qualified StimParser.Parse as SP

import HZX.Circuit

-- | Parse a STIM file string into an HZX 'Circuit'.
--
--   The underlying @stim-parser@ parser requires a leading @"!!!Start"@
--   marker.  This wrapper adds it automatically when it is missing, so callers
--   can pass ordinary STIM source text.
parseSTIM :: String -> Either String Circuit
parseSTIM s =
  let input = if "!!!Start" `isPrefixOf` s then s else "!!!Start " ++ s
  in case runParser SP.parseStim "" input of
       Left err  -> Left (errorBundlePretty err)
       Right ast -> stimToCircuit ast

-- | Convert a parsed STIM AST into an HZX 'Circuit'.
stimToCircuit :: SE.Stim -> Either String Circuit
stimToCircuit ast = Circuit <$> convertStim ast

-- ---------------------------------------------------------------------------
-- STIM AST conversion
-- ---------------------------------------------------------------------------

convertStim :: SE.Stim -> Either String [Gate]
convertStim = \case
  SE.StimG g       -> convertGate g
  SE.StimM m       -> convertMeasure m
  SE.StimGpp (SE.Gpp gty _ _ _) ->
    Left $ "Unsupported generalized Pauli-product gate: " ++ show gty
  SE.StimNoise _   -> Right []  -- noise channels are skipped
  SE.StimAnn a     -> convertAnn a
  SE.StimList ss   -> concat <$> mapM convertStim ss
  SE.StimRepeat n s -> concat <$> replicateM n (convertStim s)

-- | Extract the integer index from a qubit argument.
--
--   Classical references such as @rec[-1]@, sweep bits, and inverted targets
--   are not supported by HZX in this version.
qIndex :: SE.Q -> Either String Int
qIndex (SE.Q i) = Right i
qIndex q        = Left $ "Unsupported qubit reference (classical control / feed-forward not supported): " ++ show q

qIndices :: [SE.Q] -> Either String [Int]
qIndices = mapM qIndex

-- ---------------------------------------------------------------------------
-- Gate conversion
-- ---------------------------------------------------------------------------

convertGate :: SE.Gate -> Either String [Gate]
convertGate (SE.Gate ty _ qs) = do
  is <- qIndices qs
  case (ty, is) of
    (SE.H,   [q])    -> Right [H q]
    (SE.S,   [q])    -> Right [S q]
    (SE.X,   [q])    -> Right [XPhase q (1 % 1)]
    (SE.Y,   [q])    -> Right [XPhase q (1 % 2), ZPhase q (1 % 1)]
    (SE.Z,   [q])    -> Right [ZPhase q (1 % 1)]
    (SE.CNOT, [c,t]) -> Right [CNOT c t]
    (SE.CZ,  [q1,q2]) -> Right [CZ q1 q2]
    (SE.SWAP,[q1,q2]) -> Right [SWAP q1 q2]
    (SE.R,   [q])    -> Right [Reset ZBasis q]
    (SE.RZ,  [q])    -> Right [Reset ZBasis q]
    (SE.RX,  [q])    -> Right [Reset XBasis q]
    _ -> Left $ "Unsupported gate or wrong arity: " ++ show ty ++ " on qubits " ++ show is

-- ---------------------------------------------------------------------------
-- Measurement conversion
-- ---------------------------------------------------------------------------

convertMeasure :: SE.Measure -> Either String [Gate]
convertMeasure (SE.Measure ty _ _ qs) = do
  is <- qIndices qs
  case ty of
    SE.M   -> Right $ map (Measure ZBasis) is
    SE.MZ  -> Right $ map (Measure ZBasis) is
    SE.MX  -> Right $ map (Measure XBasis) is
    SE.MR  -> Right $ concatMap (\q -> [Measure ZBasis q, Reset ZBasis q]) is
    SE.MRX -> Right $ concatMap (\q -> [Measure XBasis q, Reset XBasis q]) is
    SE.MRZ -> Right $ concatMap (\q -> [Measure ZBasis q, Reset ZBasis q]) is
    _      -> Left $ "Unsupported measurement type: " ++ show ty

-- ---------------------------------------------------------------------------
-- Annotation conversion
-- ---------------------------------------------------------------------------

convertAnn :: SE.Ann -> Either String [Gate]
convertAnn (SE.Ann SE.TICK _ _ _) = Right []  -- TICK is a timing marker only
convertAnn _                      = Right []  -- skip DETECTOR, OBSERVABLE_INCLUDE, etc.
