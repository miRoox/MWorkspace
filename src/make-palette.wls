#!/usr/bin/env wolframscript


If[$Notebooks,SetDirectory[NotebookDirectory[]]]
Needs["GeneralUtilities`"]
<<"TrUtils.wl"
$info=<<"BuildInfo.wl"
$sourcefile=First@$info["Sources"]
$lang=If[Length@$ScriptCommandLine<2,Inherited,$ScriptCommandLine[[-1]]]
$pacletBase=FileNameJoin@{DirectoryName[Directory[]],"MWorkspace"}
$paletteBase=FileNameJoin@{$pacletBase,"FrontEnd","Palettes"}


outFile[Inherited]:=ExpandFileName@FileNameJoin@{$paletteBase,"MWorkspace.nb"}
outFile[lang_String]:=ExpandFileName@FileNameJoin@{$paletteBase,lang,"MWorkspace.nb"}


(*saveFun=With[*)
  (*{resfun=ResourceFunction["SaveReadableNotebook"]},*)
  (*If[FailureQ@resfun,*)
    (*NotebookSave,*)
    (*resfun*)
  (*]*)
(*]*)

result=Block[{$lang=$lang},
  UsingFrontEnd[
    GetWithPre[$sourcefile,GeneralUtilities`MacroEvaluate];
    Quiet[CreateDirectory[DirectoryName[outFile[$lang]]],CreateDirectory::filex];
    (*Print[Definition[MWorkspace`Palette`workspacePalette]];*)
    NotebookSave[MWorkspace`Palette`workspacePalette,outFile[$lang]]
  ]
]
If[FailureQ@result,
  Print["Failed to generate ",outFile[$lang]];Exit[1],
  Print["Generated ",outFile[$lang]," successfully!"];Exit[0]
]
