#!/usr/bin/env wolframscript


<<"TrUtils.wl"
$info=<<"BuildInfo.wl"
$langs=$info["Languages"]

trtexts=<|#->#&/@GetTrStrings[$info["Sources"]]|>

Do[
  Put[
    Merge[{trtexts,trLoad[lang]},Last],
    Echo[trFile[lang],"Update: "]
  ],
  {lang,$langs}
]
Print["Finished."]
