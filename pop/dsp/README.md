# acdsp — Aesthetic Computer DSP

**Last updated**: 2026-05-23

physically-modeled compressors and an EQ knowledge graph, written in
portable C99 so the same code runs in three places:

1. the pop/ mastering pipeline (native CLI),
2. the AC browser runtime (wasm — pieces can call comp/EQ at `paint`),
3. AC Native (static lib, when fedac/native grows audio out).

zero deps beyond libm. one library, one source of truth. no ffmpeg
`acompressor` approximations.

## why c

ffmpeg's `acompressor` is a generic feed-forward VCA — clean, but program-
independent envelopes and no harmonic generation. that's the audible
"tell" of the current pop/ masters and the reason every lane keeps tacking
a brightening pass on the end. real compressors are *physical*: a JFET's
asymmetric transfer curve, a vactrol's two-time-constant release, a tube's
soft-clip curve. modeling them needs sample-accurate state, not filter
strings.

c was picked because:

- AC Native is c — same toolchain, same calling conventions
- c compiles trivially to wasm via emcc, no glue
- the resulting `acdsp.wasm` can later become an AC piece API
  (`comp.la2a({ peak: 8 })` inside a `paint`)
- the lowest-common-denominator dependency story (libm only)

## build

```bash
cd pop/dsp/c
make            # → ./acdsp (native CLI)
make lib        # → ./libacdsp.a (link target for AC Native)
make wasm       # → ./acdsp.js + ./acdsp.wasm (needs emcc on PATH)
make test       # synthesizes a sine, runs it through the chain
```

## status

| stage                    | done | next                                 |
|--------------------------|------|--------------------------------------|
| native CLI               | ✅    |                                      |
| wasm build target        | ✅ stubbed | wire into AC runtime piece API  |
| AC Native static lib     | ✅ via `make lib` | actual integration when audio out lands |
| biquad EQ (RBJ cookbook) | ✅    |                                      |
| EQ knowledge graph       | ✅    | codegen `eq_graph.c` from JSON       |
| 1176 (FET)               | ✅    |                                      |
| LA-2A (vactrol)          |      | next — two-time-constant release     |
| Fairchild 670 (vari-mu)  |      | tube transfer curve + prog. recovery |
| SSL G-bus (VCA + SC HPF) |      | real sidechain HPF                   |
| loudnorm (ITU-R BS.1770) |      | so ffmpeg shrinks to demux/encode    |
| true-peak limiter        |      | with 4× oversample                   |

## what's physical about the 1176

`src/comp_1176.c`. not a knob-mapped `acompressor`:

- **stereo-linked peak detector** (max of |L|, |R|) feeds a single-pole
  envelope smoother. coefficient is `exp(-1 / (τ · sr))`. attack when the
  static curve wants more reduction; release otherwise.
- **soft-knee static curve** (RBJ-style quadratic), threshold internal at
  −10 dBFS with a 6 dB knee. the user gets the unit's input/output knobs;
  drive the input harder to push more signal into the curve.
- **FET asymmetric saturation** with positive DC bias for 2nd-harmonic
  dominance — the audible FET fingerprint. **drive depth scales with the
  instantaneous gain-reduction amount**: 0 dB GR ⇒ 30% of base depth,
  18 dB GR ⇒ 100%. the FET conducts harder ⇒ more nonlinearity. this is
  the part `acompressor` cannot do.
- **A12 line-amp tanh** on the way out — gentle symmetric warmth from the
  post-FET stage.
- attack/release knobs map to the unit's actual time sweep:
  - attack 1 → 800 μs, attack 7 → 20 μs (exponential)
  - release 1 → 1100 ms, release 7 → 50 ms (exponential)
- ratios 4 / 8 / 12 / 20.

references for the physics: Eichas/Möller JFET papers (2014–2017),
Zölzer's *DAFX* second edition (envelope/curve formulations), Airwindows
1176-ish sources (MIT, for sanity-checking sat curves). **GPL sources
were not consulted** — LSP/Calf/Steinberg are off limits.

## eq knowledge graph

`pop/dsp/eq-graph.json` is the source of truth. each entry resolves to a
single biquad (peak / shelf / HP / LP), carrying a frequency center, Q,
default gain, a `why:` note, and optional `conflicts:` / `pairs_with:`
hints. the C lib mirrors the table in `src/eq_graph.c` (kept in sync by
hand for now; a codegen step is in the TODO).

| intent        | kind        | freq    | default gain | use when                                                  |
|---------------|-------------|---------|--------------|-----------------------------------------------------------|
| `sub`         | highpass    | 30 Hz   | —            | strip subsonic; FM + small speakers can't reproduce       |
| `rumble`      | highpass    | 60 Hz   | —            | aggressive low cut for material with no sub content       |
| `warmth`      | peak        | 150 Hz  | +1.5         | body without mud; lifts chest of vocal / weight of synths |
| `mud`         | peak        | 250 Hz  | −2           | low-mid cloudiness from stacked synths/sub-pad            |
| `chest`       | peak        | 220 Hz  | +2           | vocal body restore (conflicts with `mud`)                 |
| `boxy`        | peak        | 500 Hz  | −2           | cardboard-tube honk on vocals / overdriven mids           |
| `nasal`       | peak        | 900 Hz  | −2           | pinched / nasal on vocals + close-mic'd dialogue          |
| `honk`        | peak        | 1200 Hz | −2           | upper-mid bark on saws, leads, bright synths              |
| `harshness`   | peak        | 3000 Hz | −2           | edginess on cymbals, guitar tops, bright vocals           |
| `presence`    | peak        | 4000 Hz | +2           | forward / in-the-room; lifts vocals through a busy mix    |
| `bite`        | peak        | 5500 Hz | +1.5         | snap on transients — beater click, snare crack, pick      |
| `sibilance`   | peak        | 7500 Hz | −3           | s/t harshness on vocals (narrow Q)                        |
| `air`         | highshelf   | 11 kHz  | +1.8         | open top — silk on vocals, sheen on cymbals               |
| `sparkle`     | highshelf   | 14 kHz  | +1.5         | glass top; mastering-only                                 |
| `tilt-bright` | highshelf   | 8 kHz   | +1.2         | broad tilt for dark mixes                                 |
| `tilt-warm`   | lowshelf    | 200 Hz  | +1           | broad warm tilt for thin material                         |

adding an intent: edit the JSON, mirror the row in `src/eq_graph.c`,
`make`.

## chain spec syntax

stages are colon-separated; multiple stages are space-separated.

```text
1176:ratio=4:in=-12:out=+6:attack=4:release=4:iron=0.5

eq:<intent>[=gain_db]            # knowledge-graph lookup
eq:presence=+2
eq:air                           # uses graph's gain_default
eq:peak:f=3500:q=1.2:g=+2        # raw biquad
eq:hp:f=30                       # raw highpass
eq:highshelf:f=11000:g=+1.8
```

1176 keys:

| key          | meaning                                               | range / default |
|--------------|-------------------------------------------------------|-----------------|
| `ratio`      | 1176 ratio (4 / 8 / 12 / 20)                          | 4               |
| `in`         | input-knob trim in dB (drives signal toward curve)    | 0               |
| `out`        | output-knob make-up in dB                             | 0               |
| `attack`     | knob units 1..7 (1=800 μs, 7=20 μs, exponential)      | 4               |
| `release`    | knob units 1..7 (1=1100 ms, 7=50 ms, exponential)     | 4               |
| `iron`       | FET + A12 nonlinearity depth                          | 0..2, default 0.5 |
| `attack-us`  | direct override in μs (overrides `attack` knob)       | —               |
| `release-ms` | direct override in ms (overrides `release` knob)      | —               |
| `threshold`  | internal threshold override (rarely needed)           | −10 dBFS        |
| `knee`       | soft-knee width in dB                                 | 6               |

## cli

```bash
acdsp in.wav out.wav --chain "stage1 stage2 ..."
                     [--bits 16|24|32]   # PCM int width (default 24)
                     [--float]           # 32-bit IEEE float output
                     [--quiet]
```

example — a danceable master-bus pass:

```bash
./acdsp scratch-mix.wav master.wav \
  --chain "eq:sub 1176:ratio=4:in=-3:out=+3:iron=0.5 eq:mud=-1.5 eq:presence=+2 eq:air=+1.5" \
  --float
```

input WAV formats accepted: PCM 8 / 16 / 24 / 32 int, IEEE float 32, mono
or stereo, any sample rate.

## node api (`pop/lib/master.mjs`)

```js
import {
  acdspAvailable,    // → boolean: is the native binary built?
  eqGraph,           // → parsed eq-graph.json (with why / conflicts notes)
  compressor,        // → "1176:..." spec fragment
  eq,                // → "eq:..." spec fragment
  chain,             // → joins fragments with spaces
  processWav,        // → runs the CLI, returns { ok, ms, stderr }
  presets,           // → curated starting chains
} from "pop/lib/master.mjs";

// build a chain by knobs:
const spec = chain(
  eq("sub"),
  compressor("1176", { ratio: 4, in: -3, out: +3, attack: 5, release: 3, iron: 0.5 }),
  eq("mud", -1.5),
  eq("presence", +2),
  eq("air", +1.5),
);

// or pick a preset and tune it:
const spec2 = presets.danceMasterBus({ in_db: -3, out_db: +3, iron: 0.7 });

// run it:
const r = processWav("in.wav", "out.wav", spec, { float: true });
if (!r.ok) throw new Error(r.stderr);
```

presets currently shipped:

- `danceMasterBus({ in_db, out_db, iron })` — sub HP → 1176 4:1 → mud cut → presence → air
- `vocalLead({ in_db, out_db, iron })` — rumble HP → 1176 8:1 → nasal cut → presence → de-ess → air
- `drumSlam({ in_db, out_db, iron })` — 1176 20:1 ("all-buttons" feel) → bite → air

## using it from a lane

`pop/dance/bin/bake.mjs` is the reference pattern. opt in with `--acdsp`:

```bash
node pop/dance/bin/bake.mjs --acdsp
```

without the flag, the original ffmpeg-only chain runs (zero risk for
shipping lanes). with the flag, the C 1176 + EQ chain runs *before*
ffmpeg's loudnorm/limit/fade, between the post-FX stage and the final
encode. existing lanes can be refit one file at a time — see
`pop/dance/bin/bake.mjs:60-95` for the diff.

## how to add a new compressor

1. write `src/comp_<name>.{h,c}` — a single struct + `init` + `update` +
   `process(buf, n_frames, channels)`. study `comp_1176.c` as the
   reference shape.
2. add a parser branch in `src/chain.c` (mirror `parse_1176`).
3. add the source file to `SRC_LIB` in the Makefile.
4. `make` — done. it'll be callable via `--chain "<name>:..."` in the CLI
   and via the node shim.

if you're modeling a new unit, the priority is the *physical* fingerprint
(envelope shape, harmonic content, sidechain HPF if any, program
dependency), not the knob count. an LA-2A with `attack`/`release` knobs is
not an LA-2A.

## roadmap

- LA-2A: vactrol envelope (LED → LDR resistance lag), fast follower +
  slow leak release model. shorter code than 1176; ~60 lines.
- Fairchild 670: vari-mu tube — gain reduction is the tube's transfer
  curve, so GR amount controls the harmonic flavor. 6-position recovery
  switch with multi-stage release.
- SSL G-bus: clean VCA + real sidechain Linkwitz-Riley HPF + the 4K
  manual's auto-release curve.
- loudnorm: ITU-R BS.1770-4 + EBU R128 integrated LUFS + LRA + TP.
- true-peak limiter: 4× oversampled with ISR detection.

once loudnorm + limiter land, ffmpeg shrinks to a demux/encode wrapper
and the DSP is entirely portable.
