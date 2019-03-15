(* ::Package:: *)

(* ::Title:: *)
(*MWorkspace*)


(* ::Section:: *)
(*Set-up*)


BeginPackage["MWorkspace`Palette`",{"GeneralUtilities`","Developer`"}]


workspacePalette


Begin["`Private`"]
<<"TrUtils.wl"



(* ::Subsection::Closed:: *)
(*Config*)


$pacletBase=FileNameJoin@{DirectoryName[$InputFileName,2],"MWorkspace"}


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*FE Load*)


GeneralUtilities`DefineLiteralMacro[imgr,
  imgr[filename_String]:=RawBoxes@DynamicBox@FEPrivate`ImportImage@FrontEnd`FileName[{"MWorkspace"},filename]
]


(* ::Subsection::Closed:: *)
(*Name (Symbol/Context)*)


maybeSymbolNameQ[name_String]:=
  AllTrue[
    StringSplit[If[StringStartsQ[name,"`"],StringDrop[name,1],name],"`",All],
    Internal`SymbolNameQ
  ]
maybeContextNameQ[name_String]:=StringEndsQ[name,"`"]&&maybeSymbolNameQ[StringDrop[name,-1]]


newSymbol[name_String,context_String]:=
  Block[{$Context=context,$ContextPath={}},
    If[NameQ[name],
      Echo[name,"MWorkspace:",StringTemplate[tr["Symbol `1` already exists."]]],
      Symbol[name]
    ]
  ]


shortName[fullname_String]:=Last@StringSplit[fullname,"`"]


(* ::Subsection::Closed:: *)
(*Definitions/Information*)


SetAttributes[GeneralUtilities`HasDefinitionsQ,ReadProtected]
hasOwnValueQ[name_String]:=ToExpression[name,StandardForm,OwnValues]=!={}
hasDownValueQ[name_String]:=ToExpression[name,StandardForm,DownValues]=!={}
hasDefinitionsQ[name_String]:=GeneralUtilities`HasDefinitionsQ[name]
noDefinitionsQ[name_String]:=!GeneralUtilities`HasDefinitionsQ[name]


getValue[name_String,wrapper_:HoldForm]:=
  Replace[ToExpression[name,StandardForm,wrapper],ToExpression[name,StandardForm,OwnValues],-1]
getFromValue[f_,name_String]:=With[{val=getValue[name,Unevaluated]},f[val]]


attributeInfo1[Temporary]=imgr["AttributeTemporary.png"]
attributeInfo1[Protect]=imgr["AttributeProtect.png"]
attributeInfo1[ReadProtected]=imgr["AttributeReadProtected.png"]
attributeInfo1[Locked]=imgr["AttributeLocked.png"]
attributeInfo1[_]=Nothing
attributeInfo[name_String]:=Row[attributeInfo1/@Attributes[name],BaselinePosition->Scaled[0.1]]


headInfo[name_String?hasOwnValueQ]:=getFromValue[Head,name]
headInfo[name_String?hasDownValueQ]:=shortName[name]<>"[\[Ellipsis]]"
headInfo[_String?noDefinitionsQ]=tr["\[LeftSkeleton]Undefined\[RightSkeleton]"]
headInfo[_String]=tr["\[LeftSkeleton]Other Type\[RightSkeleton]"]


moreOwnValueInfo1[name_String/;getFromValue[ArrayQ,name]]:=Row@Flatten@{
    If[getFromValue[Developer`PackedArrayQ,name],"(Packed)",{}],
    "<",Riffle[getFromValue[Dimensions,name],"\[Times]"],">"
  }
moreOwnValueInfo1[name_String/;getFromValue[ImageQ,name]]:=Row@Flatten@{"<",Riffle[getFromValue[ImageDimensions,name],"\[Times]"],">"}
moreOwnValueInfo1[name_String/;getFromValue[BooleanQ,name]]:="(Bool)"
moreOwnValueInfo1[name_String/;getFromValue[StringQ,name]]:=Row@{"<",getFromValue[StringLength,name],">"}
moreOwnValueInfo1[_]=Nothing
moreOwnValueInfo[name_String?hasOwnValueQ]:=moreOwnValueInfo1[name]
moreOwnValueInfo[_]=Nothing


overviewInfo[name_String]:=Row@Through[{attributeInfo,headInfo,moreOwnValueInfo}[name]]


(* ::Subsection::Closed:: *)
(*Layout*)


hBox[{items__},opt:OptionsPattern[Grid]]:=Grid[{{items}},opt]
vBox[{items__},opt:OptionsPattern[Grid]]:=Grid[{{items}}\[Transpose],opt]


(* ::Section:: *)
(*Palette Components*)


(* ::Subsection::Closed:: *)
(*Palette Button*)


SetAttributes[paletteButton,HoldRest]
paletteButton[lbl_,act_,opt:OptionsPattern[Button]]:=Button[lbl,act,Appearance->"FramedPalette",opt]


(* ::Subsection::Closed:: *)
(*SymbolsPicker*)


SetAttributes[makePickerItem,Listable]
makePickerItem[Dynamic[sel_],(Rule|RuleDelayed)[val_,lbl_]]:=
  Item[
    EventHandler[lbl,{
      "MouseClicked":>Which[
        CurrentValue["ControlKey"],If[MemberQ[sel,val],sel=DeleteCases[sel,val],AppendTo[sel,val]],
        True,sel={val}
      ]
    },PassEventsDown->True
    ],
    Background->Dynamic[If[MemberQ[sel,val],LightBlue,None]]
  ]


listGridPicker[sel_Dynamic,items:{{(Rule|RuleDelayed)[_,_]..}...},headers_List]:=
  Deploy@Pane[
    Grid[
      Prepend[makePickerItem[sel,items],headers],
      Dividers->{False,{False,2->True}},Background->{None,{LightGray,{None}}},
      Spacings->{Automatic,0.5},ItemSize->{Automatic,All}
    ],
    Scrollbars->Automatic,
    AppearanceElements->None,
    Alignment->Center
  ]


symbolsPicker[sel_Dynamic,names_List]:=
  listGridPicker[sel,
    {#->shortName[#],#->overviewInfo[#],#->getValue[#]}&/@names,
    {tr["Name"],tr["Info"],tr["Value"]}
  ]


(* ::Subsection:: *)
(*Main Body*)


mainBody:=
  DynamicModule[{},
    DynamicWrapper[
      Dynamic[
        symbolsPicker[
          Dynamic[CurrentValue[EvaluationNotebook[],{TaggingRules,"Selected"}]],
          Take[#,UpTo[15]]&@Names[CurrentValue[EvaluationNotebook[],{TaggingRules,"Context"}]<>"*"]
        ],(*TODO: more filter, refresh*)
        TrackedSymbols:>{}
      ],
      Null
    ],
    SaveDefinitions->True
  ]


(* ::Subsection:: *)
(*Toolbar*)


toolbar:=
  hBox[{(*todo: context, filter, actions, config, etc. *)
    paletteButton[imgr["ActionNew.png"],Null],
    paletteButton[imgr["ActionEdit.png"],Null],
    paletteButton[imgr["ActionImport.png"],Null],
    paletteButton[imgr["ActionExport.png"],Null],
    paletteButton[imgr["ActionDelete.png"],Null],
    paletteButton[imgr["ActionRefresh.png"],Null]
  },
    Spacings->0
  ]


(* ::Subsection:: *)
(*Palette*)


workspacePalette:=
  CreatePalette[
    mainBody,
    TaggingRules->{
      "Selected"->{},
      "Context"->"Global`"
    },
    CellContext->"MWorkspace`Palette`Private`",
    DockedCells->Cell[BoxData@ToBoxes[toolbar],"DockedCells"],
    WindowTitle->tr["Workspace"]
  ]


(* ::Section:: *)
(*Tear-down*)


End[]
EndPackage[]
