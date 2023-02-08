(* ::Package:: *)

(* ::Text:: *)
(*This patch change the default date from `OptionValue@DateSetterRange` to `DateList[]`.*)


Begin["NotebookTools`ControlsDump`"]


NotebookTools`ControlsDump`dateSetterGrid[cellobj_, var_, range_] := DynamicModule[{date = var},
	If[!NotebookTools`ControlsDump`validDateQ@date || NotebookTools`ControlsDump`expiredDateQ[range, date],
		date = Take[DateList[], 3]
	];
	Dynamic[
		Grid[
			{
				NotebookTools`ControlsDump`browseDateButton[date, range],
				Riffle[
					Grid[
						Join[
							{{"S", "M", "T", "W", "T", "F", "S"}},
							Partition[
								Function[
									If[NotebookTools`ControlsDump`expiredDateQ[range, #],
										Setter[
											Dynamic @ date,
											None, 
											Style[Part[#, 3], LightGray]
										, Appearance -> None
										, Enabled -> False
										],
											MouseAppearance[
												Setter[
													Dynamic[
														date,
														Function[
															var = #;
															NotebookDelete @ cellobj
														]
													],
													Unevaluated @ #,
													Mouseover[
														Style[
															Part[#, 3],
															"Hyperlink",
															If[var === #, Underlined, Plain]
														],
														Style[#[[3]], "HyperlinkActive"]
													]
												, Appearance -> None],
												"LinkHand"
											]
										]
									] /@ {##1},
									7,
									7,
									{
										DateString[#, "DayName"] /. {
											"Sunday" -> 1,
											"Monday" -> 2,
											"Tuesday" -> 3,
											"Wednesday" -> 4,
											"Thursday" -> 5,
											"Friday" -> 6,
											"Saturday" -> 7
										},
										1
									},
								Null
							]
						]
					, Background -> {Automatic, {Automatic, 1 -> LightGray}}
					, Dividers -> {False, Gray}
					] & @@@ Take[
						SplitBy[
							DatePlus[ReplacePart[date, 3 -> 0], #] & /@ Range @ 62,
							#[[2]] &
						],
						2
					],
					{SpanFromLeft, SpanFromLeft}
				]
			},
				Alignment -> Top,
				ItemStyle -> {Automatic, {Automatic, 1 -> Bold}}
			]
	],
	InheritScope -> True
]


End[]
