# Piecefarm day-end — 2026-07-24

## Shutdown state

- `jas-nzxt` Piecefarm is **stopped and disabled**. It will not start at the next user login.
- No `piecefarm-sdl` or `soup-server.mjs` process remains.
- The Piecefarm PulseAudio stream is gone; Line Out itself was not disabled.
- Final internal Git edition: `b7bb77c season: shutdown at iteration 57398 coverage 56`.
- Persistent state remains at `/home/me/.local/share/piecefarm/state` (7.5 MB).
- Resume without enabling autostart: `systemctl --user start piecefarm.service`.
- Resume and restore autostart: `systemctl --user enable --now piecefarm.service`.

## What became real

- Native SDL3/OpenGL observatory across both 2560×1440 tower displays, sustaining about 143.6 display FPS.
- Fixed 4×3 population of 128×128 RGB organisms with stable A1–D3 addresses.
- A typed, 16-byte-cell, 16-pixel logical margin around each RGB field:
  - 1,956 protected cells for instructions, signatures, camera state, sprite sheets, and verified bodies.
  - 7,260 mutable fringe cells for edge-derived memory and untrusted proposals.
- Raster Lisp instructions are reconstructed from verified margin cells; there is no separate native resident-bytecode array.
- Four 32×32 sprite slots with verified `copy` and `paste` operations (`replace`, `xor`, `add`, `mask`). The generator enforces copy-before-paste dependencies.
- Boxed computation with sanctuary regions that ordinary raster operations, reprobes, and neighbor grafts cannot overwrite.
- Camera and algebraic seed material grounded in AC's existing graph3d matrix conventions.
- A bounded stack evaluator in the margin. `ADD`, `XOR`, `AND`, `OR`, and `SOLARIZE` now execute from verified margin-resident bodies rather than native arithmetic branches.
- Self-synthesized projection expressions over X, Y, depth, time, and measured energy:
  - candidate bodies are written into the mutable fringe;
  - stack shape, instruction set, constants, finiteness, output bounds, and nontrivial variation are verified over a 64-point domain;
  - accepted X/Y/depth bodies are promoted into protected memory;
  - live panels were observed reporting `ALGEBRA PROMOTE … G…` while maintaining display FPS.
- Four projection families: curved/Möbius, perspective-ray, rectilinear chamber, and oblique height field.
- Musigraph audio on tower Line Out with memory-derived wavetables, low-register tonal mapping, percussion, musical-time synchronization, and click-resistant envelopes.
- Population-relative lifecycle logic with neighbor-biased grafts, organized reprobes, yellow forking, SQLite metadata, and archive retirement.
- Terminal death now differs from ordinary poor performance:
  - ordinary low performers retain a 60-second statistical window;
  - HP below 10 plus three failed native reprobes across two reports permits retirement after a 5-second terminal window.
- OpenAI visual curation is armed through the tower's private service environment; secrets are not stored in the experiment or archive.

## End-of-run evidence

- Iterations: **57,398**
- Accepted: **10,768**
- Rejected/dissolved: **46,630**
- Archive cells: **56**
- Internal Git editions: **62**
- SQLite visual reviews: **7**
- Recorded interventions: **109**
- Ledger history: 609 archived, 64 culled, 13,895 dissolving, 50 resident, 2,918 retired records
- Final observed service memory before shutdown: roughly 195 MB (peak roughly 211 MB)
- Final observed display rate: 143.6 FPS
- Full local suite: 46 passing tests
- Native replay plus AddressSanitizer/UndefinedBehaviorSanitizer self-test: passing

## Honest boundaries

- This is not yet a self-hosting Lisp machine. Native C remains the sandbox, scheduler, verifier, renderer, and interpreter.
- Five pointwise raster bodies are margin-resident; shifts, mixing, blur, edge detection, geometry, flood fill, boxes, and sprite transport still have native implementations.
- Projection bodies evolve in live resident memory but are not yet serialized or inherited after process restart or resident replacement.
- The expression synthesizer is native policy. The synthesized expressions live in margin memory, but the rules that synthesize them do not yet rewrite themselves.
- Verified bodies are interpreted, not compiled to SIMD, GPU kernels, or machine code.
- The RTX 3070 accelerated SDL compositing. It was not used for Lisp evaluation; `matador-miner` occupied most GPU compute during the run.
- OpenAI is a bounded curator, not an execution authority.

## Best resume frontier

1. Persist promoted margin bodies and genealogy to SQLite/internal Git, then restore or inherit them.
2. Give margin microcode a readable Lisp form and move the remaining pixel primitives into verified bodies.
3. Compile stable verified bodies into cached SIMD or GPU kernels while retaining interpreter replay as the proof oracle.
4. Expose promotion generation, rejection reason, and failed-reprobe counts in authoritative telemetry.
5. Let synthesis rules themselves become typed margin programs, with proof-carrying promotion and rollback.

The project source changes remain local and uncommitted; the organism/archive history on `jas-nzxt` is committed through the shutdown edition above.
