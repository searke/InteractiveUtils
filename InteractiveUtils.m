(* ::Package:: *)

BeginPackage["InteractiveUtils`", {"GeneralUtilities`"}]

iMap::usage =
"An interactive version of Map that provides a progressbar and a button to prematurely halt the evaluation.
Any unfinished evaluations are represented by a \"MapStopped\" Failure. Results temporarily stored in LocalSymbol[\"tempResult\"]."

iResume::usage =
"Given a list returned by iMap, resumes any stopped computation"

iSelect::usage =
"An interactive version of Select that provides a progressbar and a button to prematurely halt the evaluation.
Any unfinished evaluations are represented by a \"MapStopped\" Failure. Results temporarily stored in LocalSymbol[\"tempResult\"]"

Begin["`Private`"]


defaultPrintFunction = TextString /* (StringTake[#, UpTo[20]] &);
(* Definition For Kernel *)

iMap[func_, list_, printFunction_: defaultPrintFunction] /; ! $Notebooks :=
    MapIndexed[(WriteString[First@$Output, First@#2, " ", printFunction[#1]]; func[#1]) &, list];

iMap::stopped  = "Result Unavailable. Evaluation Stopped.";

(* Definition For Regular Notebook Environment *)

iMap[func_, list_,
    printFunction_: defaultPrintFunction] /; ($Notebooks && !$CloudEvaluation) :=
    Module[{run = True, position = 0},
    	LocalSymbol["tempResult"] = {};
        PrintTemporary@
        With[{startTime = Now, length = Length@list},
            GeneralUtilities`InformationPanel["Map", {
                "Time" :> Round[Now - startTime],
                "Part" :> printFunction[list[[position]]],
                "Progress" :> Column[
                	{ProgressIndicator[position/length],
                	 StringTemplate[" `` of `` "][position, length],
                	 GeneralUtilities`NiceButton["Copy Results", CopyToClipboard@Take[LocalSymbol["tempResult"], position]]}],
                Center -> GeneralUtilities`NiceButton["Stop", run = False]},
                UpdateInterval -> 1]
        ];
        Do[
	         AppendTo[
	         	LocalSymbol["tempResult"],
	        	If[run,
	        		func[list[[i]]],
	        		Failure["MapStopped", <|"MessageTemplate" :> iMap::stopped, "MessageParameters" -> <||>, "func" -> func, "arg" -> list[[i]]|>]]
	         ];
	        position = i;,
	    {i, 1, Length@list}];
	    LocalSymbol["tempResult"]
   ];
(* Definition For Other Environments  *)
iMap[func_, list_, printFuncton_] := Map[func, list];

(* Curried Form *)
iMap[func_][list_] := iMap[func, list];

(* Restart iMap with any failures *)
iResume[list_List] := With[
  {resumeFunc =
    Replace[Failure["MapStopped",
       KeyValuePattern[{"func" -> func_, "arg" -> arg_}]] :> func[arg]]
    },
  iMap[resumeFunc, list]
]

iSelect[list_, crit_ ,
    printFunction_: defaultPrintFunction] /; ($Notebooks && !$CloudEvaluation) :=
    Module[{run = True, position = 0, selected = 0},
    	LocalSymbol["tempResult"] = {};
        PrintTemporary@
        With[{startTime = Now, length = Length@list},
            GeneralUtilities`InformationPanel["Map", {
                "Time" :> Round[Now - startTime],
                "Part" :> printFunction[list[[position]]],
                "Total Selected" :> selected,
                "Progress" :> Column[
                	{ProgressIndicator[position/length],
                	 StringTemplate[" `` of `` "][position, length],
                	 GeneralUtilities`NiceButton["Copy Results", CopyToClipboard@Take[LocalSymbol["tempResult"], position]]}],
                Center -> GeneralUtilities`NiceButton["Stop", run = False]},
                UpdateInterval -> 1]
        ];
        With[
			{selectFunc = Function[{x}, If[crit[x], selected++; x, Nothing]]},
			Do[
	         AppendTo[
	         	LocalSymbol["tempResult"],
	        	If[run,
	        		selectFunc[list[[i]]],
	        		Failure["MapStopped", <|"MessageTemplate" :> iMap::stopped, "MessageParameters" -> <||>, "func" -> selectFunc, "arg" -> list[[i]]|>]]
	         ];
	        position = i;,
	    {i, 1, Length@list}]
		];
	    LocalSymbol["tempResult"]
   ]

(* Older Definition
iSelect[list_, crit_, printFunction_: defaultPrintFunction] :=
	With[
		{selectFunc = Function[{x}, If[crit[x], x, Nothing]]},
		iMap[selectFunc, list, printFunction]
	]
	*)

iSelect[crit_][list_] := iSelect[list, crit];

End[]
EndPackage[]
