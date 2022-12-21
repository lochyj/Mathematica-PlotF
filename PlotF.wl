(* ::Package:: *)

 (*
 * Created by Lachlan Jowett, 2022
 * Licensed under the MIT License which can be found in the root directory of this repository.
 * Hopefully this helps you out!
 *)

FindDerivative[expr_] := 
   Return[
      D[expr, x]
   ]

FindInverse[expr_] := 
   Module[
      {inverseFunction},
      
      inverseFunction = Simplify[
         InverseFunction[expr]
      ];
      If[Head[inverseFunction] === InverseFunction,
         Return[expr],
         Return[inverseFunction];
      ]
   ]

PlotF[
      expr_,
      options:OptionsPattern[]
   ] := 
   DynamicModule[
      {derivative, inverse, simplified},

      derivative = FindDerivative[expr];
      inverse = FindInverse[expr];
      simplified = Simplify[expr];

      Print["Derivative", derivative, "--", "Inverse", inverse, "--", "Simplified", simplified];
   
      Print[ Plot[expr, {x, -10, 10}, options], Plot[derivative, {x, -10, 10}, options], Plot[inverse, {x, -10, 10}, options] ];

   ]

(* End::Package:: *)