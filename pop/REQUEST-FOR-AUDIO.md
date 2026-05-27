# Request for Audio (RFA) — voice-take workflow

A system for slowly replacing computer-spoken / synthesized vocals in
`/pop` tracks with @jeffrey's own recorded voice, one note at a time.

The renderer already holds a precise per-note vocal score (every note
has bar / beat / midi / duration / vowel). RFA turns that score into a
**punch-list of takes to record**, captures them from the MacBook Neo
mic, and feeds them back into the render — pitch-corrected and
time-fit so an imperfect human take still lands on the score.

## The loop

```
renderer ──emits──▶ voice-takes/manifest.json   (every note that needs a voice)
                          │
            rfa.mjs ──reads──▶ for each note without a take:
                          │      • print the syllable + target pitch
                          │      • play count-in + reference tone
                          │      • record from the Neo mic (ffmpeg avfoundation)
                          │      • play back · keep / redo / skip
                          ▼
            voice-takes/<id>.wav     (the recorded take)
                          │
renderer ──on next render──▶ if a take exists for a note:
                                 WORLD-pitch + rubberband-fit it to the
                                 score note, mix it instead of synthVoice
```

## Layout (per lane)

```
pop/<lane>/voice-takes/
  manifest.json          # written by the renderer — the note punch-list
  <id>.wav               # one recorded take per note (id = "<bar>-<beat>")
```

`id` is `"<bar>-<beat>"` with `.` → `_` (e.g. bar 4 beat 2.5 → `4-2_5`).
Stable as long as a note keeps its bar/beat slot.

## manifest.json

```json
{
  "track": "marimbaba", "lane": "marimba",
  "bpm": 56, "beatSec": 1.0714, "barSec": 3.2143,
  "notes": [
    { "id": "4-0", "bar": 4, "beat": 0, "midi": 77, "note": "F5",
      "durBeats": 1.0, "durSec": 1.071, "startSec": 12.857, "vowel": "a",
      "hasTake": false }
  ]
}
```

## rfa.mjs — CLI

```
node pop/bin/rfa.mjs --track marimba              # walk every note missing a take
node pop/bin/rfa.mjs --track marimba --status     # punch-list: takes done / missing
node pop/bin/rfa.mjs --track marimba --only 4-0   # (re)record one note
node pop/bin/rfa.mjs --track marimba --device :1  # pick a CoreAudio input
node pop/bin/rfa.mjs --list-devices               # list avfoundation inputs
```

Per note: prints the prompt, `afplay`s a guide (count-in clicks at the
track BPM + a reference tone at the target pitch for the note's
duration), records `durSec + 1 s` from the mic, plays the take back,
then `keep` / `redo` / `skip` via stdin. Headphones recommended (no
mic bleed); works without.

## Renderer integration

A take routes through the existing alignment pipeline:

- **pitch** — `worldsing.py --note <noteName>` (WORLD f0 replacement)
  clamps the take to the score pitch while keeping the voice timbre.
- **time** — `rubberband -D <durSec>` fits the take to the note slot.

Two modes (per render flag `--voice raw|shaped`):

- **raw** — take is only pitch-corrected + time-fit; jeffrey's actual
  voice, dry. This *replaces* the computer voice.
- **shaped** — the aligned take also runs through `shapeVoice()` (the
  highpass / lowpass / ring-mod / flanger vocoder chain) — the take
  *becomes the synth instrument*, jeffrey as the carrier.

Notes without a take fall back to `synthVoice()`. So the track is
always renderable, and gets more "real" as takes come in.

## Free-sample mode — one-shots, no score

Not every "request for audio" is a sung note. `--sample` records an
arbitrary one-shot — a rattle, a clap, a found sound — with no manifest
and no pitch reference: hit Enter, perform, keep / redo.

```
node pop/bin/rfa.mjs --sample --track hellsine --name rattle [--dur 4]
```

The take is written to `pop/<track>/samples/<name>.wav` (silence-trimmed
+ peak-normalized when a renderer loads it). It's how `hellsine` got its
rattle — the one organic, sampled layer over the all-sine engine. The
engine loads `pop/hellsine/samples/rattle.wav` if present (`--rattle
off|sparse|drive`); absent it, the render stays pure all-sine.

## Reuse across /pop

Any lane whose renderer emits a `voice-takes/manifest.json` in this
format works with `rfa.mjs` unchanged — it's the cross-lane "request
for audio" workflow. A slab-menubar / menuband-tapes button can later
be a thin trigger over `rfa.mjs`.
