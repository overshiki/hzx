module Main (main) where

import HZX.Core.Diagram
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.IO.QASM
import HZX.Rewrite.Strategy

main :: IO ()
main = do
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
