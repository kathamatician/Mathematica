# Symbolic Calculation in Mathematica

**Course:** Symbolic Calculation in Mathematica  
**Semester:** Winter Semester 2025/26  
**Institution:** Ruhr-Universität Bochum
**Supervisor:** Dr. Arseniy Filin  

This repository contains two Mathematica / Wolfram Language projects developed as part of the course *Symbolic Calculation in Mathematica*.  
Both projects focus on symbolic and combinatorial methods relevant for theoretical and mathematical physics.

---

## Project 1: Symbolic Simplification of Pauli Matrix Products

### Overview

The first task implements a **symbolic simplification framework for products of Pauli matrices** using the Wolfram Language.  
The goal is to automate algebraic manipulations commonly appearing in quantum mechanics and quantum field theory, such as:

- Products of Pauli matrices
- Contractions using Kronecker deltas
- Levi-Civita (ε) tensor identities
- Einstein summation over repeated indices

The implementation avoids built-in simplification shortcuts and instead relies on **explicit rewrite rules**, making the algebraic structure transparent and controllable.

### Main features

- Custom noncommutative product (`myProduct`)
- Explicit Pauli matrix multiplication rule  
  \[
  \sigma_a \sigma_b = \delta_{ab} \mathbf{1} + i \epsilon_{abc} \sigma_c
  \]
- Automatic handling of dummy indices
- Conversion to a canonical form for simplification
- Pretty-printed output resembling textbook notation

### File

- `pauli_matrices.wls`  
  A standalone WolframScript file that defines the symbolic rules, simplification pipeline, and example evaluations.

---

## Project 2: Symmetry Factors of Graphs / Feynman Diagrams

### Overview

The second task computes **symmetry factors of Feynman diagrams** represented as graphs.  
Given a graph and the number of internal vertices, the program:

1. Constructs the adjacency matrix
2. Identifies graph automorphisms via permutation invariance
3. Computes the symmetry factor using combinatorial rules

This approach mirrors standard symmetry-factor calculations in perturbative quantum field theory.

### Main features

- Canonical reordering of adjacency matrices
- Detection of automorphisms via permutation invariance
- Separation of internal and external vertices
- General function interface:
  ```wolfram
  symFactor[g, numberOfInternalVertices]
