# novelizer — the novel-voice research lane

**Last updated**: 2026-07-02

A research lane inside `pop/` (sibling of `voice/` and `dsp/`, not a track
lane). Its whole job: **research and synthesize novel sounds** — instrument
voices that don't already exist in the AC stable — and prove them on real
melodies so the track lanes (and eventually notepat / gm_synth / AC Native)
can pick them up.

"Novel" is measured against what AC already plays: sine bells, supersaw,
hoover, skrill FM, zitar (Karplus-Strong + jawari), the nullnoise carved-noise
engine, the gm_synth waveguide families. A novelizer voice earns its keep by
sounding like none of those.

## Posture

Bottom-up + compositional, same as all of `pop/`. Every voice is a
self-contained portable **C99** program (libm only) in the AC house style —
normalized phase 0..1 advanced by `freq / SR` per tick, deterministic, no
deps — so a keeper can later be ported into `fedac/native/src/audio.c`,
`gm_synth.c`, or wasm without translation.

Each voice is grounded in a real synthesis-research technique with a
citation (Roads, Essl, Verplank/Mathews, Kaegi, Ishizaka–Flanagan, …).
The lane is a reading list that compiles.

## Layout

```
novelizer/
  c/
    novelizer.h        # the harness: WAV writer, test melodies, nv_main()
    voices/<name>.c    # one file per voice, self-contained + harness
    build/             # binaries (gitignored)
  bin/
    analyze.mjs        # WAV → spectral features JSON (FFT, centroid, flux, flatness…)
  out/                 # rendered WAVs + spectrograms + analysis (gitignored)
  REPORT.md            # per-batch findings: novelty / musicality verdicts
```

## The harness contract

A voice implements one function and hands it to the harness:

```c
#include "../novelizer.h"

static void render(const NvMelody *m, float *out, int nframes) {
  /* full-melody render: voice owns all state, writes mono floats */
}

int main(int argc, char **argv) { return nv_main(argc, argv, "myvoice", render); }
```

`nv_main` renders every test melody (or `--melody <name>`) into
`out/<voice>-<melody>.wav`, peak-normalized to −1 dBFS so voices compare
fairly. Melodies exercise different musical demands:

| melody      | tests                                    |
|-------------|------------------------------------------|
| `rising`    | timbre consistency across 3 octaves      |
| `lyrical`   | sustain, expressivity, phrase shape      |
| `stab`      | transients, fast staccato articulation   |
| `drone`     | long-note evolution, internal motion     |
| `beat`      | percussion: accents, retriggers, bleed   |
| `chromatic` | tuning sanity, semitone resolution       |

## Workflow (per batch)

1. Pick candidate techniques (novel vs the AC stable).
2. One builder agent per voice: research → write `voices/<name>.c` →
   compile (`cc -O2 -std=c99 -o build/<name> voices/<name>.c -lm`) →
   render → `bin/analyze.mjs` + ffmpeg spectrogram → iterate.
3. One judge agent per voice: adversarial novelty / musicality /
   entertainability scoring from the renders + features + spectrograms.
4. Human listen (@jeffrey, via slab-afplay) → verdicts land in `REPORT.md`.
5. Keepers graduate: a track lane adopts the voice, or it ports to
   `dance/synths/`-style JS, gm_synth, or AC Native.

## Analysis

`bin/analyze.mjs <file.wav>` prints JSON: duration, peak/RMS/crest,
zero-cross rate, spectral centroid (mean/σ), rolloff, flux, flatness, and
an f0-vs-harmonics inharmonicity estimate. Spectrograms via
`ffmpeg -i in.wav -lavfi showspectrumpic=s=1024x512:legend=1 out.png`.

Novelty is argued, not computed — the features + spectrogram + A/B against
the existing stable are the evidence; the judge (and jeffrey's ears) make
the call.
