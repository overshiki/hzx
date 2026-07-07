{-# LANGUAGE LambdaCase #-}

-- | STIM to parametric-circuit converter.
--
--   This module builds on 'HZX.IO.STIM' but additionally preserves Pauli
--   noise channels as parametric operations instead of skipping them.
module HZX.IO.STIM.Parametric
  ( parseSTIMParametric
  , stimToParametricCircuit
  ) where

import Control.Monad (replicateM)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Text.Megaparsec (runParser, errorBundlePretty)

import qualified StimParser.Expr as SE
import qualified StimParser.Parse as SP

import HZX.Circuit (Gate(..), MeasurementBasis(..))
import HZX.Circuit.Parametric
  ( ParametricCircuit(..), ParametricOp(..), NoiseChannel(..) )

-- | Parse a STIM string into a parametric circuit.
parseSTIMParametric :: String -> Either String ParametricCircuit
parseSTIMParametric s =
  let input = if "!!!Start" `isPrefixOf` s then s else "!!!Start " ++ s
  in case runParser SP.parseStim "" input of
       Left err  -> Left (errorBundlePretty err)
       Right ast -> stimToParametricCircuit ast

-- | Convert a parsed STIM AST into a parametric circuit.
stimToParametricCircuit :: SE.Stim -> Either String ParametricCircuit
stimToParametricCircuit ast = ParametricCircuit <$> convertStim ast

convertStim :: SE.Stim -> Either String [ParametricOp]
convertStim = \case
  SE.StimG g       -> map POGate <$> convertGate g
  SE.StimM m       -> map POGate <$> convertMeasure m
  SE.StimGpp (SE.Gpp gty _ _ _) ->
    Left $ "Unsupported generalized Pauli-product gate: " ++ show gty
  SE.StimNoise n   -> (:[]) . PONoise <$> convertNoise n
  SE.StimAnn a     -> convertAnn a
  SE.StimList ss   -> concat <$> mapM convertStim ss
  SE.StimRepeat n s -> concat <$> replicateM n (convertStim s)

qIndex :: SE.Q -> Either String Int
qIndex (SE.Q i) = Right i
qIndex q        = Left $ "Unsupported qubit reference: " ++ show q

qIndices :: [SE.Q] -> Either String [Int]
qIndices = mapM qIndex

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

convertNoise :: SE.Noise -> Either String NoiseChannel
convertNoise (SE.NoiseNormal ty _ _ phs qs) = do
  is <- qIndices qs
  case (ty, is) of
    (SE.X_ERROR, [q]) ->
      case phs of
        [p] -> Right $ XError ("xerr_" ++ show q) q p
        _   -> Left "X_ERROR expects one probability"
    (SE.Z_ERROR, [q]) ->
      case phs of
        [p] -> Right $ ZError ("zerr_" ++ show q) q p
        _   -> Left "Z_ERROR expects one probability"
    (SE.DEPOLARIZE1, [q]) ->
      case phs of
        [p] -> let p' = p / 3 in Right (Depolarize1 ("dx_" ++ show q) ("dz_" ++ show q) q p' p' p')
        _   -> Left "DEPOLARIZE1 expects one probability"
    _ -> Left $ "Unsupported noise channel: " ++ show ty
convertNoise n = Left $ "Unsupported noise syntax: " ++ show n

convertAnn :: SE.Ann -> Either String [ParametricOp]
convertAnn (SE.Ann SE.TICK _ _ _) = Right []
convertAnn _                      = Right []
