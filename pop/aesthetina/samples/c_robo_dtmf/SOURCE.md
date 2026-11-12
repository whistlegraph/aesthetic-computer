# c_robo_dtmf

Three DTMF phone-key "6" hits (770 + 1477 Hz) from Will's sample library, the same
host Menu Band's CC kit uses (`slab/menuband/Sources/MenuBand/MenuBandCCDrums.swift`).
Third-party audio stays local (see `pop/samples/README.md`); fetch before rendering:

```sh
for i in 1 2 3; do curl -fsSo pop/aesthetina/samples/c_robo_dtmf/dtmf$i.wav \
  "https://cc.oooooooooooooooo.cc/oneoffs_strudel/c_robo_dtmf/dtmf$i.wav"; done
```

`bin/render.mjs` skips the phone-key part when the files are missing.
