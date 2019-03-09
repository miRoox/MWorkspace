(* ::Package:: *)


BeginPackage["TrUtils`",{"GeneralUtilities`"}]


$lang
tr
trFile
trLoad
GetTrStrings
GetWithPre


Begin["`Private`"]


$trFileBase=FileNameJoin@{DirectoryName[$InputFileName],"Translations"}


trFile[lang_String]:=FileNameJoin@{$trFileBase,lang<>".wl"}
trLoad[lang_String]:=trLoad[lang]=With[
  {load=Association@Get[trFile[lang]]},
  If[AssociationQ[load],load,<||>]
]
GeneralUtilities`DefineMacro[tr,tr[text_String]:=tre[text,$lang]]
tre[text_String,Inherited]:=text
tre[text_String,lang_String]:=Lookup[trLoad[lang],text,text]


(*GetWithTr[file_,lang_]:=Block[{pre,tmp,$lang=lang},*)
  (*pre=Import[file,"Text"];*)
  (*tmp=StringReplace[pre,s:("tr["~~__~~"]")/;SyntaxQ[s]:>ToString[ToExpression[s,InputForm],InputForm]];*)
  (*ImportString[tmp,"Package"];*)
(*]*)
parseExprs[{},_:Identity,_: ""]:=Null
parseExprs[lines0_,pre_:Identity,expr0_: ""]:=
  With[{expr=expr0<>First[lines0]<>"\n",lines=Rest[lines0]},
    If[SyntaxQ[expr],
      ToExpression[expr,InputForm,pre];parseExprs[lines,pre,""],
      parseExprs[lines,pre,expr]
    ]
  ];
GetWithPre[file_,pre_]:=parseExprs[ReadList[file,String,NullRecords->True],pre]


GetTrStrings[files_List]:=Flatten[GetTrStrings/@files]
GetTrStrings[file_]:=Cases[Import[file,{"Package","HeldExpressions"}],HoldPattern@tr[text_String]:>text,Infinity]


End[]
EndPackage[]
