(* ::Package:: *)

 (* Created by Lachlan Jowett, 2022
 * Licensed under the MIT License which can be found in the root directory of this repository.
 * Hopefully this helps you out!
 *)


AddMouseovers[g_Graphics] :=
    Module[{lines, pts},
        lines = Cases[Normal[g][[1]], _Line, Infinity];
        If[lines === {},
            Return[g]
        ];
        pts = Catenate @ lines[[All, 1]];
        With[{nf = Nearest[pts]},
            Show[
                g,
                Epilog ->
                    Dynamic @
                        DynamicModule[
                            {pt = nf[MousePosition[{"Graphics", Graphics
                                }, {0, 0}]], scaled = MousePosition[{"GraphicsScaled", Graphics}, None
                                ]}
                            ,
                            If[scaled === None,
                                {}
                                ,
                                {Text[Framed[Row[Round[pt[[1]], 0.1],
                                     ","], Background -> LightBlue, RoundingRadius -> 7], Round[pt[[1]], 
                                    0.1], {1.2 Sign[scaled[[1]] - .5], 0}], AbsolutePointSize[7], Point[pt
                                    ], LightBlue, AbsolutePointSize[5], Point[pt]}
                            ]
                        ]
            ]
        ]
    ]


LabelIntercepts[expr_] :=
    Module[ {intercepts, x, y},
        (* ensuring it runs *)
        x = 0;
        y = 0;
        (* finding the intercepts *)
        intercepts = Solve[expr == 0, x];
        (* printing the intercepts *)
        Print["Intercepts: "];
        Print[intercepts];
        (* plotting the intercepts *)
        Plot[expr, {x, -10, 10}, PlotStyle -> {Red, Thickness[0.005]}, 
            Epilog -> {AbsolutePointSize[0.01], PointSize[0.01], Point[intercepts[[All, 1, 2]]]}]
    ]


PlotFn[expr_] :=
        DynamicModule[ {max = 10, min = -10, graphic, Axisx = True, Axisy = True},
            (* The actual GUI *)
    
            (* The actual GUI *)
            graphic = Grid[
                {
                    (* Main Options *)
                    {
                        Row[{
                            Text["Hello"]
                        }],
                        Row[{
                            Text["Hello"]
                        }],
                        Row[{
                            Text["Hello"]
                        }]
                    },

                    (* Graph and other stuff *)
                    {
                        Row[{
                            Text["Hello"]
                        }],
                        Plot[expr, {x, 10, -10}, Background -> LightBlue, PlotStyle -> {Black, Thickness -> 0.5}]
                    },

                    (* Axis options and credits *)
                    {
                        Row[{
                                
                        }],
                        Row[{
                            Text["Enable / Disable Axis"],
                        }]
                    },
                    {
                        Row[{
                                
                        }],
                        Row[{
                            Checkbox[Dynamic[Axisx]], Text[" X: "], Dynamic[Axisx]
                        }],
                        Row[{
                            Checkbox[Dynamic[Axisy]], Text[" Y: "], Dynamic[Axisy]
                        }]
                    },
                    {
                        Row[{

                        }],
                        Row[{
                            Text["Made by Lachlan Jowett 2022 - WOOD7"]
                        }]
                    }
                }
            ];

            Print[graphic];
        ]

