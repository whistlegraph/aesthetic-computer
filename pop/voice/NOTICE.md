# NOTICE — third-party material

## Pink Trombone

`vendor/pinktrombone/` (gitignored — fetch via `bin/vendor-pt.sh`) contains the canonical Neil Thapen Pink Trombone source — a single self-contained HTML file with all JS inline.

- **upstream:** https://dood.al/pinktrombone/ (the original, not a github mirror)
- **license: MIT** — *"Copyright 2017 Neil Thapen / Permission is hereby granted, free of charge…"* — verbatim copy in `vendor/pinktrombone/LICENSE`. Read directly from the upstream HTML's comment header (lines 28–46 of v1.1, March 2017).
- **why dood.al, not a github mirror:** dood.al IS the canonical source. Every github mirror is downstream. Some popular mirrors (e.g. `zakaton/Pink-Trombone`) re-license as **GPL-3.0** after refactoring, which is incompatible with shipping a permissive C/WASM jeffrey voice in fedac/native. Single-file dood.al fetch + sha256 pin is the cleanest provenance contract.
- **modifications policy:** keep `vendor/pinktrombone/` *unmodified* relative to upstream. all jeffrey-specific extensions live under `pop/voice/bin/` (e.g. `render-pt.mjs`), which read from the vendor tree.
- **provenance:** `vendor/pinktrombone/UPSTREAM.txt` captures URL, sha256, fetch timestamp, version string, author. Recorded automatically by `bin/vendor-pt.sh`.
- **MIT compliance:** any redistribution of derived AC artifacts that include the PT runtime must carry the MIT notice. The notice in `LICENSE` is the load-bearing copy; downstream packagers reproduce it verbatim.

## ElevenLabs jeffrey-pvc clone

The recorded WAVs under `corpus/raw/` and `corpus/cropped/` are derivative outputs of jeffrey's own ElevenLabs Professional Voice Clone (jeffrey-pvc). Jeffrey owns the source recording rights for that clone. These derivative WAVs are:

- gitignored (see `.gitignore`)
- used internally for fitting; not redistributed
- regenerable from `bin/record-corpus.mjs` (any consumer who needs them can re-pay the ElevenLabs API)

If the harness is ever published as a research artifact, the corpus WAVs are *not* published; only the fitted PT parameters are.

## jeffrey-platter photographs

`measure-jeffrey.py` reads photographs from `papers/jeffrey-platter/` and the AC assets CDN. Those images are subject to the platter's own provenance and licensing (see `papers/jeffrey-platter/README.md`). The harness only stores **derived measurements** (`jeffrey-anthropometry.json`) — never copies of the source photographs.
