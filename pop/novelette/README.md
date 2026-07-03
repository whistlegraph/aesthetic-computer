# novelette — the novelizer's first track

A **concept track** (hellsine contract): every voice is a batch-1/batch-2
keeper from the [`novelizer/`](../novelizer/) research lane, played as an
ensemble. The track is the graduation proof — if the novel voices can carry
a composition, the research was real.

**Instrumentation** — each voice staged in its own spatial corner:
- **frictus** (banded-waveguide bowed metal) — the bed, back-left, deepest
  in the reverb: grief drones, root + minor-3rd dyads
- **scanner** (scanned synthesis) — mid-right: nervous 16th arps with
  hash-gated rests, 3+3+2+3+5 accents, and 32nd stammer tics
- **vosim** (Kaegi/Tempelaars pulse-train formants) — front-center, dry:
  the sad melody, plus a hushed 16th "babble" undertow in the build
- **memkick** (batch-2 membrane kick) — center: downbeats, accents dive
- **cavikick** (batch-2 Helmholtz kick) — left-of-center: shuffle ghosts
- **gransnare** (batch-2 grain cloud) — right-of-center: backbeat + rolls
- **cracklesnare, STRETCHED** — wide + wet: the note length scales every
  time constant, turning the snare into multi-second chaos washes that
  mark the section doors

**Form**: D minor, 104 BPM, ~1:22 — quiet, emo, sad; the mania lives in
tics and stammers, never loudness. Each voice is introduced ALONE:
frictus → scanner → vosim → drums, then a long bar-by-bar build, a brief
grieving peak (melody up a 5th over the Dm–Bb–Gm–A ache), and a sudden
collapse to an open fifth. Harmony: Dm–Bb–F–C alternating with
Dm–Bb–Gm–A (major — the C# against the D grief).

```bash
cd c && ./build.sh          # → c/build/novelette
./c/build/novelette         # → out/novelette.wav (48k stereo)
```

Renders land in `out/` (gitignored); preview with `slab-audio out/novelette.wav`.
