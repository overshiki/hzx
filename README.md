# HZX - Haskell ZX Calculus Library

[![Haskell](https://img.shields.io/badge/Language-Haskell-purple.svg)](https://www.haskell.org/)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

**ZX calculus** is a diagrammatic language for quantum computing. HZX provides a complete implementation for circuit optimization, equivalence checking, and fault-tolerant protocol design through graph rewriting.

**Key Insight**: Quantum circuits become graphs; optimization becomes pattern matching.

```
Before:  Z(α) ─── Z(β)    After:  Z(α+β)      -- Spider fusion
Before:  ── Z(0) ──       After:  ─────       -- Identity removal
Before:  11 vertices      After:  5 vertices  -- Clifford simplification
```

---

## Installation

```bash
# Prerequisites: GHC 9.6.7+, Cabal 3.10+
cabal build
cabal test
cabal install   # Optional: install executable
```

---

## Command Line Usage

```bash
$ cabal run

=== Circuit to Diagram ===
Vertices: 8, Edges: 7, Well-formed: True

=== Two S gates (Spider Fusion) ===
Before: 4 vertices, 3 edges
After:  3 vertices, 2 edges

=== H then H (Identity Removal) ===
Before: 4 vertices, 3 edges
After:  2 vertices, 1 edges

=== Clifford Circuit ===
Before: 11 vertices, 11 edges
After:  5 vertices, 4 edges
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

-- Parse QASM
case parseQASM "OPENQASM 2.0;\nqreg q[2];\nh q[0];\ncx q[0],q[1];" of
  Left err -> putStrLn $ "Error: " ++ err
  Right circuit -> do
    let simplified = basicSimp (circuitToDiagram circuit)
    putStrLn $ serializeQASM (diagramToCircuit simplified)
```

### 3. Lattice Surgery (Fault-Tolerant Computing)

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

### 4. Equivalence Checking

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
| `spiderFusion` | Merge adjacent spiders | Reduce gate count |
| `identityRemoval` | Remove phaseless degree-2 spiders | Wire cleanup |
| `colorChange` | Z ↔ X conversion | Graph-like form |
| `localComplementation` | Eliminate Pauli spiders | Clifford optimization |
| `pivot` | Eliminate spider pairs | Clifford optimization |

### Strategies

```haskell
basicSimp      :: Strategy  -- Phase-free simplification
cliffordSimp   :: Strategy  -- Clifford normal form
toGraphLike    :: Strategy  -- Convert to graph-like form
spiderSimp     :: Strategy  -- Fusion + self-loop removal
runStrategy    :: Rule -> Strategy  -- Iterate rule to fixpoint
```

### Type Class Design

The `Diagrammatic` type class enables polymorphic rules:

```haskell
class Diagrammatic d where
  lookupVertex  :: Vertex -> d -> Maybe VertexType
  addEdge       :: Vertex -> Vertex -> EdgeType -> d -> d
  removeVertex  :: Vertex -> d -> d
  neighbors     :: Vertex -> d -> [Vertex]
  -- ... 20+ operations

-- Rules work with any instance
spiderFusion :: Diagrammatic d => d -> Maybe d
```

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
- Step-by-step rewrite tracing
- Bridge between theory and implementation

---

## Architecture

```
HZX/
├── Core/
│   ├── Diagram/          -- Diagram type + Diagrammatic class
│   ├── Phase.hs          -- Phase arithmetic
│   └── Scalar.hs         -- Scalar factors
├── Rewrite/
│   ├── Rule.hs           -- 12+ rewrite rules
│   └── Strategy.hs       -- Simplification strategies
├── Circuit/              -- Circuit ↔ Diagram conversion
├── LatticeSurgery/       -- Fault-tolerant protocols
└── IO/QASM.hs           -- OpenQASM parser
```

**Design Highlights**:
- Pure functional (no mutable state)
- Persistent data structures (IntMap, Map)
- Compositional strategies (rules + iteration)
- 52 tests passing

---

## Documentation

- `doc/zx-rewriting-theory-and-implementation.md` - Theory & design
- `doc/zx-calculus-rules.md` - Complete rule reference
- `devlog/2026-04-05-diagrammatic-type-class-design.md` - Architecture design

---

## References

- Coecke & Duncan, *Interacting Quantum Observables*, NJP 2011
- Backens, *ZX-calculus is complete for stabilizer QM*, NJP 2014
- Kissinger & van de Wetering, *Reducing T-count with ZX*, PRA 2019
- Coecke & Kissinger, *Picturing Quantum Processes*, CUP 2017

---

## License

MIT License
