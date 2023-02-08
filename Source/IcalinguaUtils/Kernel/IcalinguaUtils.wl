(* ::Package:: *)

(* ::Title:: *)
(*IcalinguaUtils*)


BeginPackage["IcalinguaUtils`"]


`$DefaultDatabasePath = "~/.config/icalingua/databases/";


`$DatabaseNamePattern = "eqq"~~DigitCharacter..~~".db";


`ListDatabaseFiles[] := FileNames[
	`$DatabaseNamePattern,
	`$DefaultDatabasePath
]


`MessageSearchApp[] := IcalinguaUtils`MessageSearch`GUI`SelectDatabase[]


Begin["`Private`"]


End[]


(* ::Chapter:: *)
(*MessageSearch*)


BeginPackage["`MessageSearch`", {"`"}]


`RegisterRDBEntity[] := (
	`$DatabaseReference = DatabaseReference@`$DataBaseFile;
	`$RelationalDatabase = RelationalDatabase@`$DatabaseReference;
	`$EntityStore = EntityStore@`$RelationalDatabase;
	EntityUnregister@Entity["messages"]["EntityStore"];
	EntityRegister@`$EntityStore
)


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsection:: *)
(*QueryMessages*)


(* ::Text:: *)
(*No need to save it here, will achieve this by `Tasks`.*)


(*IcalinguaUtils`MessageSearch`QueryMessages[opts:OptionsPattern[]] := <|
	"Query" -> {opts},
	"Result" -> EntityValue[
		FilteredEntityClass[
			EntityClass["messages", OptionValue@"ClassProperty"],
			OptionValue@"EntityFilter"
		]
	, OptionValue@"QueryField"]
|>;*)


IcalinguaUtils`MessageSearch`QueryMessages[opts:OptionsPattern[]] := EntityValue[
	FilteredEntityClass[
		EntityClass["messages", OptionValue@"ClassProperty"],
		OptionValue@"EntityFilter"
	]
, OptionValue@"QueryField"]


Options[IcalinguaUtils`MessageSearch`QueryMessages] = {
	"ClassProperty" -> {},
	"EntityFilter" -> EntityFunction[IcalinguaUtils`MessageSearch`Private`$entity, True],
	"QueryField" -> {"time", "roomId", "senderId", "username", "content", "replyMessage"}
};


(* ::Subsection:: *)
(*Form*)


IcalinguaUtils`MessageSearch`FieldInterpreter["DateObject"][input_] := If[DateObjectQ@#,
	#,
	Failure["InvalidDateList", <||>]
]&@DateObject@input;


(* ::Text:: *)
(*I've struggled with *)
(*	Forms`PackageScope`bindForm*)
(*and*)
(*	Forms`PackageScope`makeController,*)
(*and still don't understand how to make a custom controller which works in FormObject.*)
(**)
(*But this workaround works.*)


IcalinguaUtils`MessageSearch`Private`ResetDateRange[] := (
	IcalinguaUtils`MessageSearch`Private`$dateLower = -Infinity;
	IcalinguaUtils`MessageSearch`Private`$dateUpper = Infinity;
)


IcalinguaUtils`MessageSearch`$FormObject := (
	IcalinguaUtils`MessageSearch`Private`ResetDateRange[];
	FormObject@{
		"roomId" -> <|
			"Interpreter" -> "Number",
			"Control" -> InputField,
			"Label" -> "\:4f1a\:8bddID",
			"Required"->False,
			"Hint" -> "Optional"
		|>,
		"senderId" -> <|
			"Interpreter" -> Restricted["String", DigitCharacter.., {5, Infinity}],
			"Control" -> InputField,
			"Label" -> "\:53d1\:9001\:8005ID",
			"Required"->False,
			"Hint" -> "Optional"
		|>,
		"contentKeyword" -> <|
			"Interpreter" -> "String",
			"Control" -> InputField,
			"Label" -> "\:5185\:5bb9\:ff08\:5173\:952e\:8bcd\:ff09",
			"Required"->False,
			"Hint" -> "Optional"
		|>,
		"contentPattern" -> <|
			"Interpreter" -> "Expression",
			"Control" -> InputField,
			"Label" -> "\:5185\:5bb9\:ff08\:6a21\:5f0f\:ff09",
			"Required" -> False,
			"Hint" -> "Optional"
		|>,
		"contentFilter" -> <|
			"Interpreter" -> "Expression",
			"Control" -> InputField,
			"Label" -> "\:5185\:5bb9\:ff08\:7b5b\:9009\:5668\:ff09",
			"Required" -> False,
			"Hint" -> "Optional"
		|>,
		"dateLower" -> <|
			"Interpreter" -> Function[
				IcalinguaUtils`MessageSearch`FieldInterpreter["DateObject"]@IcalinguaUtils`MessageSearch`Private`$dateLower
			],
			"Control" -> Function@Row@{
				Developer`DateSetter[Dynamic@IcalinguaUtils`MessageSearch`Private`$dateLower, NotebookTools`DateSetterRange -> {1970,1,1}],
				InputField[Dynamic@IcalinguaUtils`MessageSearch`Private`$dateLower, Expression]
			},
			"Input" -> "This is a placeholder",
			"Label" -> "\:8d77\:59cb\:65f6\:95f4",
			(*"Required" -> False,*)
			"Default" -> Function@-Infinity
		|>,
		"dateUpper" -> <|
			"Interpreter" -> Function[
				IcalinguaUtils`MessageSearch`FieldInterpreter["DateObject"]@IcalinguaUtils`MessageSearch`Private`$dateUpper
			],
			"Control" -> Function@Row@{
				Developer`DateSetter[Dynamic@IcalinguaUtils`MessageSearch`Private`$dateUpper, NotebookTools`DateSetterRange -> {1970,1,1}],
				InputField[Dynamic@IcalinguaUtils`MessageSearch`Private`$dateUpper, Expression]
			},
			"Input" -> "This is a placeholder",
			"Label" -> "\:7ec8\:6b62\:65f6\:95f4",
			(*"Required" -> False,*)
			"Default" -> Function@Infinity
		|>,
		"timestampFilter" -> <|
			"Interpreter" -> "Expression",
			"Control" -> InputField,
			"Label" -> "\:65f6\:95f4\:6233\:ff08\:8fc7\:6ee4\:5668\:ff09",
			"Required" -> False,
			"Hint" -> "Optional"
		|>,
		"queryField" -> <|
			"Interpreter" -> AnySubset@{"_id", "time", "roomId", "senderId", "username", "content", "at", "replyMessage", "files"},
			"Control" -> TogglerBar,
			"Label" -> "\:8fd4\:56de\:5b57\:6bb5",
			"Input" -> {"time", "roomId", "senderId", "username", "content", "replyMessage"}
		|>,
		"timeConstraint" -> <|
			"Interpreter" -> "Number",
			"Control" -> InputField,
			"Label" -> "\:67e5\:8be2\:7528\:65f6\:9650\:5236",
			"Default" -> 10,
			"Hint" -> "Default: 10"
		|>
	}
)


(*IcalinguaUtils`MessageSearch`DivideFilter[opts:{__Rule}] := *)


(* ::Subsection:: *)
(*Form-Query Interface*)


IcalinguaUtils`MessageSearch`GenerateFormFilter[formAssoc_Association] := With[{
	contentKeywordFilter = If[MissingQ@#, Nothing&, #]&@#contentKeyword,
	contentPatternFilter = If[MissingQ@#, Nothing&, #]&@#contentPattern,
	contentFilter = If[MissingQ@#, Nothing&, #]&@#contentFilter,
	timeFilter = Which[
		!MissingQ@#timestampFilter,
			#timestampFilter,
		AllTrue[Not@*MissingQ]@{#dateLower, #dateUpper},
			Function[{t1, t2},
				Switch[{t1, t2},(* Infinity not compilable by SQL*)
					{-Infinity, Infinity},
						Nothing&,
					{_, Infinity},
						t1 < #/1000. &,
					{-Infinity, _},
						#/1000. < t2 &,
					_,
						t1 < #/1000. < t2 &
				]
			][UnixTime@#dateLower, UnixTime@NextDate[#dateUpper, "Second"]],
		True,
			Nothing&
	]
},
	EntityFunction[IcalinguaUtils`MessageSearch`Private`$entity,
		And@##
	]&@@{
		contentKeywordFilter@IcalinguaUtils`MessageSearch`Private`$entity["content"],
		contentPatternFilter@IcalinguaUtils`MessageSearch`Private`$entity["content"],
		contentFilter@IcalinguaUtils`MessageSearch`Private`$entity["content"],
		timeFilter@IcalinguaUtils`MessageSearch`Private`$entity["time"]
	}
]&@formAssoc

IcalinguaUtils`MessageSearch`QueryByForm[formAssoc_Association] := TimeConstrained[
	IcalinguaUtils`MessageSearch`QueryMessages[
		"ClassProperty" -> Normal@DeleteMissing@Query[{"roomId", "senderId"}]@formAssoc,
		"EntityFilter" -> IcalinguaUtils`MessageSearch`GenerateFormFilter@formAssoc,
		"QueryField" -> Query["queryField"]@formAssoc
	]
, Query["timeConstraint"]@formAssoc];

IcalinguaUtils`MessageSearch`QueryByFormAsync[formAssoc_Association] := SessionSubmit[
	IcalinguaUtils`MessageSearch`QueryByForm@formAssoc
	, HandlerFunctions -> <|"TaskFinished" -> IcalinguaUtils`MessageSearch`$AsyncQueryFinishHandler|>
];


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*GUI*)


Begin["`GUI`"]


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection:: *)
(*Select database*)


IcalinguaUtils`MessageSearch`GUI`SelectDatabase[] := CreateWindow@With[{
	paths = (*ReverseSortBy[FileSize]@*)ListDatabaseFiles[]
},
	If[path =!= {},
		DialogNotebook[{
			ActionMenu[
				"Select database:",
				# :> (
					IcalinguaUtils`MessageSearch`$DataBaseFile = File@#;
					IcalinguaUtils`MessageSearch`RegisterRDBEntity[];
					DialogReturn@IcalinguaUtils`MessageSearch`GUI`QueryFormPage[]
			)&/@paths
			]
		}, WindowTitle -> "\:9009\:62e9\:6570\:636e\:5e93"],
		DialogNotebook[{TextCell["Empty database folder!"], DefaultButton[]}]
	]
]


(* ::Subsubsection:: *)
(*FormPage*)


(* ::Text:: *)
(*Maybe customize a `FormLayoutFunction`.*)


IcalinguaUtils`MessageSearch`GUI`QueryFormPage[] := CreateDialog[
	FormPage[
		IcalinguaUtils`MessageSearch`$FormObject,
		IcalinguaUtils`MessageSearch`QueryByFormAsync,
		{"Form", "Button"}
		, AppearanceRules -> <|"SubmitLabel" -> "\:67e5\:8be2"|>
	]
, Saveable -> False
, WindowElements -> {}
, WindowSize -> All
, WindowTitle -> "\:67e5\:8be2\:8868\:5355"
]


(* ::Subsubsection:: *)
(*Result*)


(*IcalinguaUtils`MessageSearch`$AsyncQueryFinishHandler = MessageDialog*)


IcalinguaUtils`MessageSearch`$AsyncQueryFinishHandler = Function[
	IcalinguaUtils`MessageSearch`$LastQueryResult = #;
	NotebookPut@IcalinguaUtils`MessageSearch`GUI`LayoutResult@IcalinguaUtils`MessageSearch`GUI`FormatResult@#
];


IcalinguaUtils`MessageSearch`$FieldFormatter = {
	"time" -> Function@DateString[FromUnixTime[#/1000.], "ISODateTime"],
	"roomId" -> Identity,
	"senderId" -> Identity,
	"username" -> Identity,
	"content" -> Identity,
	"replyMessage" -> Function@If[MissingQ@#,
		"",
		#username<>":\n"<>#content&@ImportByteArray[StringToByteArray@#, "RawJSON"]
	],
	"files" -> Function@Grid[
		Values@*Query[{"type", "name", "url", "size"}] /@ ImportByteArray[StringToByteArray@#, "RawJSON"]
	, Frame -> All],
	"at" -> Replace[_Missing -> ""],
	_ -> Identity
};


IcalinguaUtils`MessageSearch`GUI`FormatResult[taskAssoc_Association] := With[{
	field = #EvaluationExpression[[1,1, "queryField"]]&@taskAssoc,
	result = #EvaluationResult&@taskAssoc
},
	MapThread[Construct]@{Replace[field, IcalinguaUtils`MessageSearch`$FieldFormatter, {1}], #}& /@ result
		//TableForm[#, TableHeadings -> {Range@Length@#, field}]&
]


IcalinguaUtils`MessageSearch`GUI`LayoutResult[expr_] := Notebook[
	{Cell@BoxData@ToBoxes@Style[expr, ShowStringCharacters -> False]}
, StyleDefinitions -> "Default.nb"
, WindowTitle -> "\:67e5\:8be2\:7ed3\:679c"
]


(* ::Subsubsection:: *)
(*End*)


End[]


(* ::Subsection:: *)
(*End*)


End[]


(* ::Section:: *)
(*End*)


EndPackage[]


(* ::Chapter:: *)
(*End*)


EndPackage[]
