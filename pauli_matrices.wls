#!/usr/bin/env wolframscript

(* ========================================= *)
(*   Simplification rules for Pauli matrices *)
(* ========================================= *)

(* 
   Symbolic objects:
   - delta      : Kronecker delta δ_ab
   - epsilon    : Levi–Civita symbol ε_abc
   - sigma      : Pauli matrices σ_a
   - myProduct  : noncommutative product container
*)

ClearAll[delta, epsilon, myProduct, sigma];

(* Kronecker delta is symmetric in its indices *)
SetAttributes[delta, Orderless];

(* ========================================= *)
(*   Core algebraic simplification rules     *)
(* ========================================= *)

rules = {
   (* Distribute scalar multiplication *)
   (a_ + b_) c_ :> a c + b c,

   (* Using Einstein summation: δ_aa = 3 for SU(2) *)
   delta[x_Symbol, x_] :> 3,

   (* δ_ab replaces index a → b inside tensors *)
   delta[a_Symbol, b_] sometensor_[indicesbefore___, a_, indicesafter___] :> 
     sometensor[indicesbefore, b, indicesafter],

   (* δ_ab replaces free index a everywhere *)
   delta[a_Symbol, b_] anything_ :> 
     (anything /. a -> b) /; Not[FreeQ[anything, a]],

   (* Squared delta *)
   delta[a_Symbol, b_]^2 :> delta[b, b],

   (* Canonical ordering and antisymmetry of ε_abc *)
   epsilon[a_, b_, c_] :> Signature[{a, b, c}] Sort[epsilon[a, b, c]],

   epsilon[a_, b_, c_] epsilon[d_, e_, f_] :> 
     Det[Outer[delta, {a, b, c}, {d, e, f}]],

   epsilon[a_, b_, c_]^2 :> 
     Det[Outer[delta, {a, b, c}, {a, b, c}]],

   (* Identity element cleanup *)
   myProduct[a__, one, c___] :> myProduct[a, c],
   myProduct[a___, one, c__] :> myProduct[a, c],

   (* Zero annihilates the whole product *)
   myProduct[___, 0, ___] :> 0,

   (* Distribute sums inside noncommutative products *)
   myProduct[a___, x_ + y_, c___] :> 
     myProduct[a, x, c] + myProduct[a, y, c],

   (* Pull out scalar factors *)
   myProduct[a___, x_ y_, b___] :> 
     x myProduct[a, y, b] /; FreeQ[x, sigma] && FreeQ[x, one],

   (* Pauli matrix multiplication rule: σ_a σ_b = δ_ab 1 + i ε_abc σ_c *)
   myProduct[something___, sigma[a_], sigma[b_], somethingelse___] :> 
     delta[a, b] myProduct[something, one, somethingelse] + 
     Module[{c}, 
       I epsilon[a, b, c] myProduct[something, sigma[c], somethingelse]
     ]
};


(* =========================================== *)
(*  Schoonschip notation dummy-index handling  *)
(* =========================================== *)

(* Temporarily absorb contracted indices into epsilon to avoid dummy-index clashes during simplification *)
convertToSchoonRules = {
   epsilon[a___, x_Symbol, b___] myProduct[sigma[x_]] :> 
     epsilon[a, sigma[], b],

   (* Re-canonicalize epsilon after replacements *)
   epsilon[x__] :> Signature[{x}] Sort[epsilon[x]]
};

(* Reintroduce explicit dummy indices after simplification *)
convertFromSchoonRules = {
   epsilon[a___, sigma[], b___] :> 
     Module[{x}, epsilon[a, x, b] myProduct[sigma[x]]]
};


(* ========================================= *)
(*   Full simplification pipeline            *)
(* ========================================= *)

ClearAll[mySimplify];
mySimplify[x_] := 
  x //. rules //. convertToSchoonRules //. convertFromSchoonRules;

(* ========================================= *)
(*   Pretty-printing rules                   *)
(* ========================================= *)

niceOutputRules = {
   myProduct[one] -> \[DoubleStruckOne],
   myProduct[sigma[x_]] :> Subscript[\[Sigma], x],
   delta[x_, y_] :> Subscript[\[Delta], x, y],
   epsilon[x_, y_, z_] :> Subscript[\[Epsilon], x, y, z]
};

ClearAll[niceForm];
niceForm[x_] := x //. niceOutputRules;

(* ===================== *)
(*   Example evaluations *)
(* ===================== *)

examples = {
  myProduct[sigma[a], sigma[b], sigma[c], sigma[a], sigma[b], sigma[c]],
  myProduct[sigma[a], sigma[b], sigma[c], sigma[a], sigma[b], sigma[d]],
  myProduct[sigma[a], sigma[b], sigma[c], sigma[c], sigma[b]],
  epsilon[a, b, c] myProduct[sigma[a], sigma[b], sigma[d], sigma[c], sigma[e]],
  myProduct[sigma[a], sigma[b], sigma[c], sigma[d], sigma[b]],
  myProduct[sigma[a], sigma[b], sigma[c], sigma[b]],
  epsilon[a, b, c] myProduct[sigma[a], sigma[b], sigma[c]],
  epsilon[a, b, c] myProduct[sigma[a], sigma[e], sigma[b], sigma[c], sigma[d]],
  epsilon[a, b, c] myProduct[sigma[a], sigma[b], sigma[c], sigma[d], sigma[d]],
  epsilon[a, b, c] myProduct[sigma[a], sigma[e], sigma[d], sigma[b], sigma[c]],
  epsilon[a, b, c] myProduct[sigma[d], sigma[a], sigma[b], sigma[c], sigma[d]],
  epsilon[a, b, c] myProduct[sigma[e], sigma[a], sigma[d], sigma[b], sigma[c]]
};

Do[
  simplified = mySimplify[ex];
  Print @ niceForm[simplified],
  {ex, examples}
];