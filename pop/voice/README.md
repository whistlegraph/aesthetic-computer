# voice — the jeffrey synth lanes

a research lane inside `pop/`. **two sub-lanes as of 2026-05-04 pivot:**

1. **[`diphones/`](diphones/)** — *the shippable AC-native jeffrey TTS.* jeffrey-pvc ElevenLabs as teacher → forced-aligned diphone bank → C/WASM concat synth. ~3-5 MB shippable assets. Sounds like jeffrey because the diphones ARE jeffrey.
2. **PT lane (this README + the rest of `pop/voice/`)** — *Pink Trombone as a timbral musical instrument.* Confirmed audibly that PT can't reach phoneme-level intelligibility from external trajectory fitting alone — it's a vocal-tract music tool, not a speech synthesizer. The fit pipeline is still useful: anthropometric priors, single-pose vowel matching, and big-pictures-style sustained-vowel timbres.

The PT lane was the first attempt; the pivot below records why it can't ship as jeffrey's voice but stays useful for music. The diphones lane is the new mainline for shippable jeffrey speech.

---

## PT lane (timbral instrument)

original goal — a tiny, runnable, anatomically grounded jeffrey voice — a physical model of jeffrey's vocal tract that we can synthesize from in C/WASM at AC-native budgets — derived from two existing AC corpora:

1. the **jeffrey-pvc** ElevenLabs clone (the audio side — already canonical, already wired to `/api/say`)
2. the **jeffrey-platter** photographic corpus (55 AV-shoot headshots + 38 candid masters + 90 first-person POV captures) — the visual side, already POI-annotated

the synthesis layer is **pink trombone** (neil thapen, 2017) — a browser-side physical model of the vocal tract, ~500 lines of JS, MIT-ish, ports cleanly to C and WASM. PT is the structural prior. jeffrey-pvc is the acoustic target. the platter photos give us the *anatomical* prior so the search lives inside a realistic envelope, not a hacky one.

> "the gear was the gate, the gate was the price tag" — `pop/big-pictures/plork.txt`

the gate here is generic-adult-male defaults. the gate is the price tag.

## why this lane exists

the prior frankenstein attempt — slicing jeffrey-pvc per phoneme and stitching — produced incoherent monsters because the slices come from different acoustic contexts. PT is a *physical* model: optimizing tract parameters lands on something coherent by construction. and once it's a physical model, the parameters mean something — they're tongue position, lip aperture, glottal tension — not opaque embedding deltas.

dropped 2026-05-03: the AC-native formant-resonator path. it sounded like tones, not a voice.

picked up 2026-05-03: the pink trombone path, with anatomical priors derived from the platter, with the question of whether the data can also *improve* PT itself.

## pipeline

```
platter photos ──▶ measure-jeffrey.py  (mediapipe / insightface 3D landmarks)
                       │
                       ▼
                  PHYSIOLOGY.md priors
                  (VTL, lip aperture range, mandible ROM, nasal scale)
                       │
                       ▼
jeffrey-pvc /api/say ──▶ record-corpus.mjs ──▶ corpus/raw/*.wav
                       │
                       ▼
                  fit.py  (CMA-ES + MFCC distance, bounded by priors)
                       │
                       ▼
                  fits/<phoneme>.json   (per-phoneme PT param trajectories)
                       │
                       ▼
                  render.{c,wasm,mjs}    (jeffrey voice at synth time)
```

every step caches to disk. reruns cost $0 (the only paid step is the corpus
recording, and that's content-hashed).

## file map

```
README.md             — this file
CORPUS.md             — phoneme target list + recording protocol
PHYSIOLOGY.md         — anthropometric pipeline (platter → tract priors)
MODEL-EXTENSIONS.md   — research outline: how the physiology data could
                        improve pink trombone itself, not just bound it
NOTICE.md             — third-party license tracking (PT vendor)

bin/
  vendor-pt.sh        — clone neil thapen's pink trombone into vendor/
  record-corpus.mjs   — hits /api/say with jeffrey-pvc per phoneme
  measure-jeffrey.py  — extracts anthropometry from platter photos
  fit.py              — CMA-ES fit of PT params to recorded WAV
  render-pt.mjs       — node-side PT renderer (params → wav, headless)

corpus/
  phonemes.json       — machine-readable target list
  raw/                — recorded jeffrey-pvc WAVs (gitignored)

fits/                 — per-phoneme PT param trajectories (gitignored)
vendor/
  pinktrombone/       — vendored thapen source (gitignored; see NOTICE.md)
jeffrey-anthropometry.json  — derived priors (committed; small JSON)
```

## status

- 2026-05-04 — lane scaffolded. all docs landed, all bin/ scripts are stubs.
- 2026-05-04 — vendored from `dood.al/pinktrombone` directly (single-file HTML, **MIT, Neil Thapen 2017**). A github mirror (`zakaton/Pink-Trombone`) was tried first but turned out GPL-3.0 after refactoring; rejected. License decision resolved — see [NOTICE.md](NOTICE.md).
- 2026-05-04 — `bin/render-pt.mjs` working. Loads PT inline source via node `vm` sandbox (drift-free; re-extracts from upstream HTML on every call). Renders six sustained vowels (`ah ee oo aa schwa hum`) to WAV in ~90 ms each at 22050 Hz. Smoke set on `~/Desktop` is the audible proof.
- 2026-05-04 — `bin/fit.py` working. CMA-ES + librosa MFCC distance, calls `render-pt.mjs` as a subprocess. PT→PT self-test recovers every articulator within tolerance. Real fit on the smoke `pt-ee.wav` with `iy` seed: loss 27.8 → 2.5 over 200 evals (~36 s). Recovered params lined up tightly: `tongueDiameter Δ -0.02`, `f0 Δ +0.6 Hz`, `tenseness Δ +0.006`, `lipMul ≈ 1.0`.
- 2026-05-04 — `bin/measure-jeffrey.py` working (mediapipe Tasks API). Ran on 521 confirmed-jeffrey IG photos → **363 measurements** (74% detection rate). [`jeffrey-anthropometry.json`](jeffrey-anthropometry.json) committed: 8 facial measurements (medians + IQRs) + Fitch-Giedd VTL prior (17.5 cm ± 0.8 σ; population male median, no stated body height yet). `lip_mul_upper_bound = 1.0` consumed by `fit.py`'s bounds clamp.
- 2026-05-04 — smoke recording: 3 jeffrey-pvc clips (/iy/ /ah/ /uw/, iso context) — server returns raw mp3 without with-timestamps for jeffrey-pvc, recorder falls back gracefully.
- 2026-05-04 — fit loss v0.1 → v0.2: replaced MFCC time-mean (20-d) with log-mel mean+std (128-d) + librosa.pyin F0 distance in log-cents. v0.1 reported f0=120 Hz on jeffrey's /iy/ but the actual jeffrey-pvc F0 was 144.94 Hz — v0.1 was 25 Hz off and didn't know it. v0.2 recovers F0 to within 1.4 Hz. Throughput 3.7 → 1.8 evals/s (pyin overhead).
- 2026-05-04 — fit-basin finding: with `--sigma0 0.08 --init iy` the optimizer recovers the *anatomically correct* front-tongue /iy/ pose (`tongueIndex=12.04, tongueDiameter=2.67`). With wider sigma it falls into a back-close basin at slightly lower loss — PT can produce similar vowel-like timbres from multiple tract configurations, so the acoustic loss is multimodal. POSE_INITS + tight sigma is the right disambiguator, not loss-function complexity.
- 2026-05-04 — trajectory rendering landed. `render-pt.mjs` accepts `--keyframes` arrays of `{t, ...params}` interpolated linearly across the duration; PT's tract.movementSpeed adds another smoothing layer. `--word eye` shortcut + WORD_RECIPES table for /aɪ eɪ oʊ aʊ wi/. 2-keyframe joint fit: take 4 ElevenLabs requests for "eye"/"owe"/"we"/"ay", then run fit.py twice per word with `--window-start/--window-end` to recover start + end poses independently. Composed trajectory recovers anatomically correct front/back tongue positions AND the natural rising F0 contour (e.g. "eye" goes 143 → 198 Hz under stress). PT speaking the fitted-from-jeffrey "eye" is `pt-word-eye-fitted.wav` on Desktop.
- **2026-05-04 — pivot:** Audible test of PT-saying-words confirmed PT cannot reach phoneme-level intelligibility. New mainline for shippable jeffrey speech is **[`diphones/`](diphones/)** — ElevenLabs-distilled diphone bank, ~3-5 MB shippable, pure C concat at synth time. Diphone target list generated: 1527 ARPABet pairs, 532 in tier-1. PT remains useful for big-pictures sustained-vowel timbres; it's just not the speech engine.

```bash
# render a hand-crafted word
node bin/render-pt.mjs --word eye --duration 1.0 --out ~/Desktop/pt-eye.wav
# fit a word from jeffrey's recording (two-pass, start + end)
.venv/bin/python bin/fit.py --target jeff-eye.mp3 --init ah --window-end 0.45 --out fits/eye-start.json
.venv/bin/python bin/fit.py --target jeff-eye.mp3 --init iy --window-start 0.55 --out fits/eye-end.json
# (compose keyframes + render — see one-liner in fits/ pipeline)
```

```bash
# quick smoke run
node bin/render-pt.mjs --smoke ~/Desktop
# single vowel with arbitrary params
node bin/render-pt.mjs --params '{"pose":"iy","f0":110}' --duration 2 --out ~/Desktop/pt-ee.wav
# fit any target WAV
.venv/bin/python bin/fit.py --target ~/Desktop/pt-ee.wav --init iy --max-evals 200
# end-to-end self-test (no target needed)
.venv/bin/python bin/fit.py --self-test --max-evals 200
```

## related

- memory: `reference_pink_trombone.md` — origin notes
- memory: `feedback_jeffrey_pvc_settings.md` — stability ≥ 0.5 keeps voice identity
- memory: `feedback_pop_dual_vocal.md` — AC-native vocal lane history (formant attempt dropped)
- repo: [`pop/RESEARCH-DIRECTION.md`](../RESEARCH-DIRECTION.md) — vocal lane in the wider pop/ research
- repo: [`papers/jeffrey-platter/`](../../papers/jeffrey-platter/) — photographic corpus index (the data this lane reads)
- repo: [`portraits/jeffrey/bin/`](../../portraits/jeffrey/bin/) — existing face-match / face-describe pipeline (insightface, GPT-4o); shares dependency surface with `measure-jeffrey.py`

---

*maintained by @jeffrey — update STATUS as the lane advances*
