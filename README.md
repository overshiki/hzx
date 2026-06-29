# HZX - Haskell ZX Calculus Library

[![Haskell](https://img.shields.io/badge/Language-Haskell-purple.svg)](https://www.haskell.org/)

**ZX calculus** is a diagrammatic language for quantum computing. HZX provides an implementation for circuit optimization, equivalence checking, and fault-tolerant protocol design through graph rewriting.

**Key Insight**: Quantum circuits become graphs; optimization becomes pattern matching.

```
Before:  Z(α) ─── Z(β)    After:  Z(α+β)      -- Spider fusion
Before:  ── Z(0) ──       After:  ─────       -- Identity removal
Before:  11 vertices      After:  5 vertices  -- Clifford simplification
```

---

## Prerequisites

- GHC 9.6.7+
- Cabal 3.10+

---

## Command Line Usage

```bash
$ cabal run
```

Sample output:

```
=== Circuit to Diagram ===
Vertices: 8, Edges: 7
Well-formed: True

=== Two S gates ===
Before: 4 vertices, 3 edges
After basicSimp: 3 vertices, 2 edges
Well-formed: True
fromList [(0,Boundary Rough),(1,Boundary Rough),(2,Z (1 % 1))]

=== H then H ===
Before: 4 vertices, 3 edges
After basicSimp: 2 vertices, 1 edges
Well-formed: True
fromList [(0,Boundary Rough),(1,Boundary Rough)]

=== Clifford Circuit ===
Before: 11 vertices, 11 edges
After cliffordSimp: 5 vertices, 4 edges
Well-formed: True
fromList [(0,Boundary Rough),(1,Boundary Rough),(2,Boundary Rough),(3,Boundary Rough),(5,Z (1 % 2))]

=== QASM Parse & Simplify ===
Parsed gates: [H 0,CNOT 0 1,S 1,S 1]
Diagram: 9 vertices, 8 edges
Simplified: 7 vertices, 6 edges
Well-formed: True
fromList [(0,Boundary Rough),(1,Boundary Rough),(2,Boundary Rough),(3,Boundary Rough),(5,Z (0 % 1)),(6,X (0 % 1)),(7,Z (1 % 1))]

=== QASM Serialization ===
OPENQASM 2.0;
qreg q[2];
h q[0];
cx q[0], q[1];
t q[1];
```

---

## Library Usage

### 1. Circuit Optimization

```haskell
import HZX.Core.Diagram
import HZX.Circuit
import HZX.Circuit.ToDiagram
import HZX.Rewrite.Strategy

-- Optimize a quantum circuit
let circuit = Circuit [H 0, CNOT 0 1, S 1, H 1, CNOT 1 0]
let diagram = circuitToDiagram circuit
let optimized = cliffordSimp diagram

-- Check reduction
putStrLn $ "Before: " ++ show (numVertices diagram) ++ " vertices"
putStrLn $ "After:  " ++ show (numVertices optimized) ++ " vertices"
-- Output: Before: 11, After: 5
```

### 2. QASM Integration

```haskell
import HZX.IO.QASM

case parseQASM "OPENQASM 2.0;\nqreg q[2];\nh q[0];\ncx q[0],q[1];" of
  Left err -> putStrLn $ "Error: " ++ err
  Right circuit -> do
    let simplified = basicSimp (circuitToDiagram circuit)
    putStrLn $ serializeQASM simplified
```

Note: `serializeQASM` currently skips gates that have no OpenQASM 2.0 equivalent (e.g., mid-circuit measurements and resets).

### 3. STIM Integration

```haskell
import HZX.IO.STIM

-- Parse a STIM circuit (used widely in QEC research)
let stim = unlines
      [ "R 0 1"
      , "H 0"
      , "CNOT 0 1"
      , "M 0 1"
      ]

case parseSTIM stim of
  Left err -> putStrLn $ "Parse error: " ++ err
  Right circuit -> do
    let diagram = circuitToDiagram circuit
    putStrLn $ "Before: " ++ show (numVertices diagram) ++ " vertices"
    let optimized = basicSimp diagram
    putStrLn $ "After:  " ++ show (numVertices optimized) ++ " vertices"
```

Supported STIM constructs (v1): `H`, `S`, `X`, `Y`, `Z`, `CNOT`, `CZ`, `SWAP`, `M`, `MX`, `MZ`, `R`, `RX`, `RZ`, `MR`, `MRX`, `MRZ`, `TICK`, and noise channels / annotations (skipped).  Unsupported gates such as `MPP`, `MXX`, or classical `rec[-1]` references fail hard.

### 4. Lattice Surgery (Fault-Tolerant Computing)

```haskell
import HZX.LatticeSurgery.Core
import HZX.LatticeSurgery.Protocols.CNOT

-- Create logical qubits
let (d1, mem1) = createMemory Rough empty
let (d2, mem2) = createMemory Rough d1

-- Perform CNOT via lattice surgery  
let heralded = cnotLS d2 mem1 mem2

-- Handle both measurement outcomes
let (posDiagram, _) = positiveBranch heralded
let (negDiagram, _) = negativeBranch heralded
```

### 5. Equivalence Checking

```haskell
-- Two circuits are equivalent if they simplify to the same normal form
let c1 = Circuit [H 0, H 0]           -- H then H = I
let c2 = Circuit []                   -- Empty (identity)

let n1 = basicSimp (circuitToDiagram c1)
let n2 = basicSimp (circuitToDiagram c2)

print (n1 == n2)  -- True: both reduce to 2 vertices (just boundaries)
```

---

## Core Functionality

### Rewrite Rules

| Rule | Effect | Use Case |
|------|--------|----------|
| `spiderFusion` | Merge adjacent same-color spiders | Reduce gate count |
| `identityRemoval` | Remove phaseless degree-2 spiders | Wire cleanup |
| `hadamardEdgeSimp` | Remove H-boxes between two spiders | Convert to graph-like form |
| `colorChange` | Convert X spiders to Z spiders | Graph-like form |
| `hopfRule` | Cancel pairs of simple edges between opposite colors | Simplify connectivity |
| `selfLoopRemoval` | Remove simple self-loops | Clean up artifacts |
| `piCommutation` | Push π phases through opposite-color spiders | Phase propagation |
| `stateCopy` | Absorb degree-1 state spiders | State simplification |
| `piCopy` | Combined state-copy / π-commutation | Phase copying |
| `bialgebraSimp` | Apply bialgebra law | Clifford simplification |
| `localComplementation` | Eliminate Pauli spiders | Clifford optimization |
| `pivot` | Eliminate spider pairs | Clifford optimization |

### Strategies

```haskell
phaseFreeSimp  :: Strategy  -- Phase-free rules (H-box, bialgebra, fusion, identity, Hopf, self-loop)
basicSimp      :: Strategy  -- Iterate phase-free simplification to fixpoint
spiderSimp     :: Strategy  -- Spider fusion + self-loop removal
toGraphLike    :: Strategy  -- Convert to graph-like form
cliffordSimp   :: Strategy  -- Clifford normal form
runStrategy    :: Rule -> Strategy  -- Iterate a single rule to fixpoint
```

### Type Class Design

The `Diagrammatic` type class abstracts over ZX diagram structures:

```haskell
class Diagrammatic d where
  lookupVertex  :: Vertex -> d -> Maybe VertexType
  allVertices   :: d -> [Vertex]
  adjustVertex  :: Vertex -> (VertexType -> VertexType) -> d -> d
  removeVertex  :: Vertex -> d -> d
  neighborBundles :: Vertex -> d -> M.Map Vertex EdgeBundle
  addEdge       :: Vertex -> Vertex -> EdgeType -> d -> d
  removeEdge    :: Vertex -> Vertex -> EdgeType -> d -> d
  inputs        :: d -> [Vertex]
  outputs       :: d -> [Vertex]
  scalar        :: d -> Scalar
  mapScalar     :: (Scalar -> Scalar) -> d -> d
  -- plus derived predicates: isZSpider, isXSpider, neighbors, degree, ...
```

Rules are defined on the concrete `Diagram` type (`Rule = Diagram -> Maybe Diagram`) and can be iterated via `runStrategy`.

---

## Potential Impact

### 1. Circuit Optimization
- **Reduce T-count** (expensive for fault-tolerant computing)
- **Minimize CNOT depth** (critical for NISQ devices)
- **Automated simplification** (no manual rewrite sequences)

### 2. Equivalence Checking
- Verify compiler optimizations preserve semantics
- Check circuit identities without matrix multiplication
- Polynomial-time simplification vs. exponential simulation

### 3. Fault-Tolerant Protocols
- Design lattice surgery operations diagrammatically
- Verify CNOT, T-gate implementations
- Automate magic state distillation circuits

### 4. Educational Tool
- Visual understanding of quantum computation
- Bridge between theory and implementation

---

## Architecture

```
HZX/
├── Core/
│   ├── Diagram/
│   │   ├── Types.hs      -- Vertex, Edge, Scalar types
│   │   ├── Class.hs      -- Diagrammatic type class
│   │   └── Instances.hs  -- Diagram record + allocation helpers
│   ├── Diagram.hs        -- Re-export module
│   ├── Phase.hs          -- Phase arithmetic
│   └── Scalar.hs         -- Scalar factors
├── Rewrite/
│   ├── Rule.hs           -- 12 rewrite rules
│   ├── Strategy.hs       -- Simplification strategies
│   └── Matcher.hs        -- Pattern matching utilities
├── Circuit/
│   ├── Circuit.hs        -- Gate / Circuit types
│   ├── ToDiagram.hs      -- Circuit → Diagram
│   └── FromDiagram.hs    -- Diagram → Circuit (stage 1)
├── LatticeSurgery/       -- Fault-tolerant protocols
│   ├── Core.hs
│   ├── Heralded.hs
│   ├── PauliFrame.hs
│   └── Protocols/
│       ├── CNOT.hs
│       └── TGate.hs
├── LinAlg/
│   └── Z2.hs             -- Z2 linear algebra helpers
└── IO/
    ├── QASM.hs           -- OpenQASM parser/serializer
    └── STIM.hs           -- STIM parser/converter
```

**Design Highlights**:
- Pure functional (no mutable state)
- Persistent data structures (IntMap, Map)
- Compositional strategies (rules + iteration)
- 77 tests passing

---

## License

MIT License
