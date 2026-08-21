module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Ratio (numerator, denominator)

import HZX.Core.Diagram
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Circuit.FromDiagram
import HZX.IO.QASM
import HZX.Rewrite.Strategy

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runDemos
    [inputFile, outputFile] -> runBenchmark inputFile outputFile "basic"
    [inputFile, outputFile, strategy] -> runBenchmark inputFile outputFile strategy
    _ -> do
      hPutStrLn stderr "Usage: hzx [input.qasm output.json [strategy]]"
      hPutStrLn stderr "  strategy: basic | clifford | spider | phasefree (default: basic)"
      exitFailure

-- | Run HZX in benchmark mode: read QASM, simplify, and write a JSON report
--   containing metrics, extracted QASM, and the simplified graph structure.
runBenchmark :: FilePath -> FilePath -> String -> IO ()
runBenchmark inputFile outputFile strategyName = do
  qasm <- readFile inputFile
  case parseQASM qasm of
    Left err -> do
      writeFile outputFile $ "{\"error\": \"parse: " ++ escapeJson err ++ "\"}"
      exitFailure
    Right circ -> do
      let d0 = circuitToDiagram circ
          d1 = applyStrategy strategyName d0
          extracted = diagramToCircuit d1
      writeFile outputFile $ benchmarkJson circ d0 d1 extracted strategyName

applyStrategy :: String -> Diagram -> Diagram
applyStrategy name d = case name of
  "basic"     -> basicSimp d
  "clifford"  -> cliffordSimp d
  "spider"    -> spiderSimp d
  "phasefree" -> phaseFreeSimp d
  _           -> error $ "Unknown strategy: " ++ name ++ ". Use basic|clifford|spider|phasefree"

benchmarkJson :: Circuit -> Diagram -> Diagram -> Circuit -> String -> String
benchmarkJson inputCirc inputDiag outputDiag outputCirc strategy =
  "{\n"
  ++ "  \"strategy\": \"" ++ strategy ++ "\",\n"
  ++ "  \"input_gates\": " ++ show (length (gates inputCirc)) ++ ",\n"
  ++ "  \"input_vertices\": " ++ show (numVertices inputDiag) ++ ",\n"
  ++ "  \"input_edges\": " ++ show (numEdges inputDiag) ++ ",\n"
  ++ "  \"output_vertices\": " ++ show (numVertices outputDiag) ++ ",\n"
  ++ "  \"output_edges\": " ++ show (numEdges outputDiag) ++ ",\n"
  ++ "  \"well_formed\": " ++ map toLower (show (isWellFormed outputDiag)) ++ ",\n"
  ++ "  \"extracted_gates\": " ++ show (length (gates outputCirc)) ++ ",\n"
  ++ "  \"t_count\": " ++ show (countTGates outputCirc) ++ ",\n"
  ++ "  \"output_qasm\": " ++ show (serializeQASM outputCirc) ++ ",\n"
  ++ "  \"graph\": " ++ graphJson outputDiag ++ "\n"
  ++ "}"
  where
    toLower c
      | c == 'T'  = 't'
      | c == 'F'  = 'f'
      | otherwise = c

graphJson :: Diagram -> String
graphJson d =
  "{\n"
  ++ "    \"inputs\": " ++ show (inputs d) ++ ",\n"
  ++ "    \"outputs\": " ++ show (outputs d) ++ ",\n"
  ++ "    \"vertices\": [\n" ++ verticesJson ++ "\n    ],\n"
  ++ "    \"edges\": [\n" ++ edgesJson ++ "\n    ]\n"
  ++ "  }"
  where
    verticesJson = commaSep (map vertexJson (IM.toList (_vertices d)))
    vertexJson (v, ty) = "      {\"id\": " ++ show v ++ ", \"type\": " ++ show (vertexTypeString ty) ++ ", \"phase\": " ++ phaseJson ty ++ "}"

    edgesJson = commaSep (map edgeJson (M.toList (_edges d)))
    edgeJson ((v1, v2), bundle) =
      "      {\"source\": " ++ show v1 ++ ", \"target\": " ++ show v2 ++
      ", \"simple\": " ++ show (simpleCount bundle) ++
      ", \"hadamard\": " ++ show (hadamardCount bundle) ++ "}"

vertexTypeString :: VertexType -> String
vertexTypeString (Boundary Rough)  = "boundary_rough"
vertexTypeString (Boundary Smooth) = "boundary_smooth"
vertexTypeString (Z _)             = "z"
vertexTypeString (X _)             = "x"
vertexTypeString HBox              = "hbox"

phaseJson :: VertexType -> String
phaseJson (Z p) = rationalJson p
phaseJson (X p) = rationalJson p
phaseJson _     = "0"

rationalJson :: Rational -> String
rationalJson r =
  "{\"num\": " ++ show (numerator r) ++ ", \"den\": " ++ show (denominator r) ++ "}"

commaSep :: [String] -> String
commaSep = foldr (\x y -> if null y then x else x ++ ",\n" ++ y) ""

countTGates :: Circuit -> Int
countTGates (Circuit gs) = length [() | T _ <- gs]

escapeJson :: String -> String
escapeJson = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape c   = [c]

-- | Original demonstration mode. Runs when no command-line arguments are given.
runDemos :: IO ()
runDemos = do
  -- Test 1: circuit translation
  let c = Circuit [H 0, CNOT 0 1, S 1]
      d = circuitToDiagram c
  putStrLn "=== Circuit to Diagram ==="
  putStrLn $ "Vertices: " ++ show (numVertices d) ++ ", Edges: " ++ show (numEdges d)
  putStrLn $ "Well-formed: " ++ show (isWellFormed d)

  -- Test 2: spider fusion on two adjacent S gates
  let c2 = Circuit [S 0, S 0]
      d2 = circuitToDiagram c2
  putStrLn "\n=== Two S gates ==="
  putStrLn $ "Before: " ++ show (numVertices d2) ++ " vertices, " ++ show (numEdges d2) ++ " edges"
  let d2' = basicSimp d2
  putStrLn $ "After basicSimp: " ++ show (numVertices d2') ++ " vertices, " ++ show (numEdges d2') ++ " edges"
  putStrLn $ "Well-formed: " ++ show (isWellFormed d2')
  print (_vertices d2')

  -- Test 3: identity removal
  let c3 = Circuit [H 0, H 0]
      d3 = circuitToDiagram c3
  putStrLn "\n=== H then H ==="
  putStrLn $ "Before: " ++ show (numVertices d3) ++ " vertices, " ++ show (numEdges d3) ++ " edges"
  let d3' = basicSimp d3
  putStrLn $ "After basicSimp: " ++ show (numVertices d3') ++ " vertices, " ++ show (numEdges d3') ++ " edges"
  putStrLn $ "Well-formed: " ++ show (isWellFormed d3')
  print (_vertices d3')

  -- Test 4: clifford simplification on a small Clifford circuit
  let c4 = Circuit [H 0, CNOT 0 1, S 1, H 1, CNOT 1 0]
      d4 = circuitToDiagram c4
  putStrLn "\n=== Clifford Circuit ==="
  putStrLn $ "Before: " ++ show (numVertices d4) ++ " vertices, " ++ show (numEdges d4) ++ " edges"
  let d4' = cliffordSimp d4
  putStrLn $ "After cliffordSimp: " ++ show (numVertices d4') ++ " vertices, " ++ show (numEdges d4') ++ " edges"
  putStrLn $ "Well-formed: " ++ show (isWellFormed d4')
  print (_vertices d4')

  -- Test 5: QASM parse and round-trip simplification
  let qasm = unlines
        [ "OPENQASM 2.0;"
        , "qreg q[2];"
        , "h q[0];"
        , "cx q[0], q[1];"
        , "s q[1];"
        , "s q[1];"
        ]
  putStrLn "\n=== QASM Parse & Simplify ==="
  case parseQASM qasm of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right c5 -> do
      putStrLn $ "Parsed gates: " ++ show (gates c5)
      let d5 = circuitToDiagram c5
      putStrLn $ "Diagram: " ++ show (numVertices d5) ++ " vertices, " ++ show (numEdges d5) ++ " edges"
      let d5' = basicSimp d5
      putStrLn $ "Simplified: " ++ show (numVertices d5') ++ " vertices, " ++ show (numEdges d5') ++ " edges"
      putStrLn $ "Well-formed: " ++ show (isWellFormed d5')
      print (_vertices d5')

  -- Test 6: QASM serialization
  let c6 = Circuit [H 0, CNOT 0 1, T 1]
  putStrLn "\n=== QASM Serialization ==="
  putStrLn (serializeQASM c6)
