(* ::Package:: *)

Paclet[
	Name -> "IcalinguaUtils",
	Version -> "0.0.2", (* Change entries to functions; Save latest query result; Better DateSetter; More query fields; Some texts. *)
	WolframVersion -> "12+", (* Relational databases are supported in 12.0, so this is **one minimal** requirement. Most newer versions are not tested. *)
	Description -> "Provide tools for cooperating with Icalingua++, especially on message management.",
	Root -> ".",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel",
			Root -> ".",
			Context -> "IcalinguaUtils`",
			Symbols -> {
				IcalinguaUtils`MessageSearchApp,
				IcalinguaUtils`MessageSearch`RegisterRDBEntity,
				IcalinguaUtils`MessageSearch`QueryMessages,
				IcalinguaUtils`MessageSearch`QueryByFormAsync,
				IcalinguaUtils`MessageSearch`$AsyncQueryFinishHandler,
				IcalinguaUtils`MessageSearch`GUI`SelectDatabase,
				IcalinguaUtils`MessageSearch`$FieldFormatter,
				IcalinguaUtils`MessageSearch`GUI`FormatResult,
				IcalinguaUtils`MessageSearch`GUI`LayoutResult
			}
		(* Select[Names["IcalinguaUtils`*"], Capitalize@# === # &@ StringTake[#, 1] &]//StringRiffle[#,"\",\n\t\t\t\t\""]& *)
		}
	}
]
