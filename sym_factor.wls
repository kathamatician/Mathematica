#!/usr/bin/env wolframscript

(* ======================================================= *)
(*  WolframScript for calculating symmetry factors of     *)
(*  Feynman diagrams / graphs given a graph and number   *)
(*  of internal vertices.                                 *)
(* ======================================================= *)

ClearAll[symFactor, correctAdjacencyMatrix, isinv, automorphismsM];

(* ------------------------------------------------------- *)
(* Correct adjacency matrix so vertices are ordered 1..n  *)
(* This ensures canonical ordering for permutation checks *)
(* ------------------------------------------------------- *)
correctAdjacencyMatrix[g_Graph] := Module[{naiveAM, vertexlist, correctingPermutation},
  naiveAM = Normal[AdjacencyMatrix[g]];
  vertexlist = VertexList[g];
  
  (* Check that all vertices are integers 1..n *)
  If[Not[TrueQ[Sort[vertexlist] == Range[Length[vertexlist]]]],
    Message[correctAdjacencyMatrix::badgraph, Sort[vertexlist], Range[Length[vertexlist]]];
    Return[$Failed]
  ];
  
  (* Permute adjacency matrix into canonical order *)
  correctingPermutation = InversePermutation[vertexlist];
  naiveAM[[correctingPermutation, correctingPermutation]]
];

correctAdjacencyMatrix::badgraph = 
  "Input graph has vertices `1` but expected `2`. Check that all vertices are integers from 1 to n.";

(* Check if a matrix is invariant under a permutation    *)
isinv[matr_, perm_] := matr[[perm, perm]] == matr

(* Select all permutations that leave a matrix invariant *)
automorphismsM[matrix_, listofperm_] := Select[listofperm, (isinv[matrix, #]) &]

(* ------------------------------------------------------- *)
(* Main function to calculate the symmetry factor          *)
(* g: input graph                                         *)
(* n: number of internal vertices                         *)
(* ------------------------------------------------------- *)

symFactor[g_Graph, n_Integer] := Module[
  {matrixFD, bign, matrixWD, permutationsWD, amWD, permutationsFD, amFD, sFD, symmetryFactor},

  (* Full adjacency matrix *)
  matrixFD = correctAdjacencyMatrix[g];
  bign = Length[matrixFD];

  (* Adjacency matrix of internal vertices only *)
  matrixWD = Take[matrixFD, n, n];

  (* All permutations of internal vertices *)
  permutationsWD = Permutations[Range[n]];

  (* Automorphisms of the internal adjacency matrix *)
  amWD = automorphismsM[matrixWD, permutationsWD];

  (* Extend internal automorphisms to full graph size *)
  permutationsFD = ArrayPad[amWD, {{0, 0}, {0, bign - n}}, Range[n + 1, bign]];

  (* Automorphisms of the full adjacency matrix *)
  amFD = automorphismsM[matrixFD, permutationsFD];
  sFD = Length[amFD];

  (* Symmetry factor formula: inverse of automorphisms, factorials, and 2^trace *)
  symmetryFactor = (sFD *
     Product[Product[Factorial[matrixFD[[i, j]]], {i, j, bign}], {j, 1, bign}] *
     2^Tr[matrixFD])^(-1);

  symmetryFactor
];

(* ------------------------------------------------------- *)
(*                       Example                           *)
(* ------------------------------------------------------- *)

myGraph = Graph[
  {1 <-> 2, 2 <-> 3, 3 <-> 4, 4 <-> 5, 5 <-> 6, 6 <-> 1, 
   2 <-> 5, 6 <-> 3, 7 <-> 1, 8 <-> 4},
  VertexLabels -> "Name"
];
gv1 = Graph[
  {1 <-> 2, 1 <-> 2, 1 <-> 2, 3 <-> 1, 2 <-> 4},
  VertexLabels -> "Name"
];

Print["Symmetry factor: ", symFactor[myGraph, 6]];
Print["Symmetry factor: ", symFactor[gv1, 2]];