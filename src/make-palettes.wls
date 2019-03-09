#!/usr/bin/env wolframscript


$info=<<"BuildInfo.wl"
$langs=Prepend[$info["Languages"],Inherited]
$makeone=AbsoluteFileName["make-palette.wls"]

procs=Association@Table[
  lang->If[StringQ@lang,
    StartProcess[{"wolframscript",$makeone,lang}],
    StartProcess[{"wolframscript",$makeone}]
  ],
  {lang,$langs}
]
toproc=Association@Table[lang->True,{lang,$langs}]

While[AnyTrue[toproc,Identity],
  Do[
    If[toproc[lang]&&ProcessStatus[procs[lang],"Finished"],
      Print@ReadString[procs[lang],EndOfFile,TimeConstraint->1.];
      toproc[lang]=False
    ],
    {lang,$langs}
  ]
]
Print["Completing all build palettes."]
