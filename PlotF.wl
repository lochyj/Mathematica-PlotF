(* ::Package:: *)

 (*
 * Created by Lachlan Jowett, 2022
 * Licensed under the MIT License which can be found in the root directory of this repository.
 * Hopefully this helps you out!
 *)

CustomPlot[f_, xMin_, xMax_] := 
   Module[
      {xValues, yValues},

      xValues = Range[xMin, xMax, 0.1];
      yValues = f /@ xValues;
      ListLinePlot[
         Transpose[{xValues, yValues}]
      ]
   ]

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
      {derivative, inverse, simplified, showDerivative = False, showInverse = False},

      derivative = FindDerivative[expr];
      inverse = FindInverse[expr];
      simplified = Simplify[expr];

      Print["Derivative: |", derivative, "|\r\n"];
      Print["Inverse:    |", inverse, "|\r\n"];
      Print["Simplified: |", simplified, "|\r\n"];

      Grid[
         {
            {
               Row[{"Show derivative ", Checkbox[
                  Dynamic[showDerivative]
               ],
                  showDerivative
               }],
               Row[{"Show inverse", Checkbox[
                  Dynamic[showInverse]
               ],
                  showInverse
               }]
            },
            {
               Plot[{
                     expr,
                     If[showDerivative, Return[derivative], {}],
                     If[showInverse, Return[inverse], {}]
                  }, {x, 10, -10}]
            }
         }
      ]
   
   ]

(* End::Package:: *)