#!/usr/bin/env wolframscript

(* ========================================= *)
(*   Simplification rules for Pauli matrices *)
(* ========================================= *)

ClearAll[delta, epsilon, myProduct, sigma];

SetAttributes[delta, Orderless];

rules = {
   (a_ + b_) c_ :> a c + b c,

   delta[x_Symbol, x_] :> 3,

   delta[a_Symbol, b_] sometensor_[indicesbefore___, a_, indicesafter___] :> 
     sometensor[indicesbefore, b, indicesafter],

   delta[a_Symbol, b_] anything_ :> 
     (anything /. a -> b) /; Not[FreeQ[anything, a]],

   delta[a_Symbol, b_]^2 :> delta[b, b],

   epsilon[a_, b_, c_] :> Signature[{a, b, c}] Sort[epsilon[a, b, c]],

   epsilon[a_, b_, c_] epsilon[d_, e_, f_] :> 
     Det[Outer[delta, {a, b, c}, {d, e, f}]],

   epsilon[a_, b_, c_]^2 :> 
     Det[Outer[delta, {a, b, c}, {a, b, c}]],

   myProduct[a__, one, c___] :> myProduct[a, c],
   myProduct[a___, one, c__] :> myProduct[a, c],
   myProduct[___, 0, ___] :> 0,

   myProduct[a___, x_ + y_, c___] :> 
     myProduct[a, x, c] + myProduct[a, y, c],

   myProduct[a___, x_ y_, b___] :> 
     x myProduct[a, y, b] /; FreeQ[x, sigma] && FreeQ[x, one],

   myProduct[something___, sigma[a_], sigma[b_], somethingelse___] :> 
     delta[a, b] myProduct[something, one, somethingelse] + 
     Module[{c}, 
       I epsilon[a, b, c] myProduct[something, sigma[c], somethingelse]
     ]
};

convertToSchoonRules = {
   epsilon[a___, x_Symbol, b___] myProduct[sigma[x_]] :> 
     epsilon[a, sigma[], b],

   epsilon[x__] :> Signature[{x}] Sort[epsilon[x]]
};

convertFromSchoonRules = {
   epsilon[a___, sigma[], b___] :> 
     Module[{x}, epsilon[a, x, b] myProduct[sigma[x]]]
};

ClearAll[mySimplify];
mySimplify[x_] := 
  x //. rules //. convertToSchoonRules //. convertFromSchoonRules;

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
  myProduct[epsilon[a, b, c], sigma[a], sigma[b], sigma[d], sigma[c], sigma[e]]
};

Do[
  simplified = mySimplify[ex];
  Print @ niceForm[simplified],
  {ex, examples}
];