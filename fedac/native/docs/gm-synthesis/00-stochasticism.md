# GM Synthesis Dossier 00 — Bounded Per-Note Stochasticism

A cross-cutting design principle for the Aesthetic Computer native GM synthesis
library (`fedac/native/src/audio.c`). It governs *every* instrument in the four
family dossiers (01–04), the same way a single bow stroke is never bit-identical
to the last on a real instrument.

> **The requirement (jeffrey).** Every sound in the GM system gets a *bit* of
> stochasticism so it reads as organic — but **not** so much that it changes the
> overall timbre or pitch. Just enough that the same voicing playing the same
> note doesn't produce the exact same wave profile every trigger, while staying
> **relatively consistent** — the way the AC percussion hi-hats and snares
> already vary.

This document is the **calibration anchor** for that "bit." The numbers here are
not invented; they are measured from the existing AC percussion system
(`system/public/aesthetic.computer/lib/percussion.mjs`, bundled to the device as
`/lib/percussion.mjs`) and then mapped onto the GM synthesis levers.

---

## 1. The calibration reference: how AC percussion already varies

`percussion.mjs` fires each drum as a stack of `sound.synth()` voices. Two inline
helpers introduce all the per-hit variation (percussion.mjs:115–116):

```js
// rj — jitter a CENTER value by a ± fraction (multiplicative).
const rj = (center, frac) => center * (1 + (Math.random() - 0.5) * 2 * frac);
// rn — uniform value in a range (used for pan offsets).
const rn = (min, max) => min + Math.random() * (max - min);
```

Measuring every call site across the 12 drums, the variation lands in tight bands:

| Lever | What varies | Range used in `percussion.mjs` |
|---|---|---|
| **Per-voice amplitude** | `volume:` on each layer | `rj(v, 0.10)`–`rj(v, 0.18)`, a few tails at `0.20`–`0.25` → **±10–18% typical, ±25% max** |
| **Tail decay / duration** | `duration:` on the ring-out layers | `rj(d, 0.18)`–`rj(d, 0.25)` → **±18–25%** (only on *tails*, never the transient) |
| **Stereo pan** | base `downPan` + per-tail spread | base `rn(-0.02, 0.02)`…`rn(-0.06, 0.06)`; noise tails add `rn(-0.04, 0.04)` → **≈ ±0.02–0.06, up to ±0.10 on diffuse tails** |
| **Tonal frequency** | the drum's pitch (`tone:`) | **NOT JITTERED — zero.** |

The last row is the whole point, and it is stated explicitly in `audio.c` at the
top of the native percussion recipes (audio.c:1827–1834):

> *"Key principle: the iconic 808 frequencies (238 Hz snare, 540/800 Hz cowbell,
> 6-square hat cluster) are NOT jittered — they're what makes each drum sound
> like itself. Per-hit variation comes from TIMBRE jitter (volume balance,
> decay, attack, pan) not tonal jitter."*

**The calibrated feel, distilled:** the percussion system varies *energy
distribution* (which partial is loudest this time, how long the tail rings, where
it sits in the stereo field) by **±10–25%**, and varies *pitch by exactly
nothing*. Variation lives in amplitude/decay/pan; identity lives in frequency.

For pitched GM instruments we cannot hold pitch at *literally* zero variance —
phase-locked unison stacks need a hair of detune to avoid robotic phase-perfect
combing — but we keep the pitch lever an order of magnitude smaller than the
amplitude lever, well under audible mistuning. That is the single most important
calibration decision in this document, and it comes straight from the drums.

### 1.1 How `notepat` invokes it (no jitter params passed)

The native `notepat` calls the shared kernel through `triggerPercussionDown` →
`playPercussion` → `sharedPlayPercussion(sound, letter, { volume, pan,
pitchFactor, phase, holdVoices, onVoice })` (notepat.mjs:1763, 1847, 1857). It
passes **no** random or jitter parameters — the stochasticism is entirely
*internal* to the synth kernel. That is the model GM synthesis must follow: the
caller asks for "note N at velocity V," and the *engine* decides the micro-offsets
at note-on. Pieces never thread RNG state through the API.

---

## 2. The engine already has the machinery

Two facts from `audio.c` / `audio.h` make this cheap to implement:

1. **A per-voice PRNG already exists.** `xorshift32(uint32_t *state)`
   (audio.c:100–107) is the engine's standard PRNG, and every `ACVoice` carries
   a `uint32_t noise_seed` (audio.h:87). It is already seeded per-trigger at
   voice allocation (audio.c:3361):

   ```c
   v->noise_seed = (uint32_t)(audio->next_id * 2654435761u);
   ```

   `audio->next_id` increments on every `audio_synth()` (audio.c:3356), so each
   trigger gets a **distinct, deterministic** seed. This is exactly the property
   we want: *deterministic within a trigger, varied across triggers.* Today only
   `WAVE_NOISE/WHISTLE/GUN/HARP/PIANO` get seeded — the GM types must extend that
   list (see §6).

2. **The phase-increment rule is non-negotiable.** Sustained tones advance a
   phase register and read a wavetable; never `sin(TAU*f*t)` per sample (audio.c
   convention + `MEMORY.md`: *"long sine phase-increment not sin(TAU*f*t)"*). All
   stochastic detune is therefore applied **once, at note-on**, baked into each
   partial's `f_inc` (or KS delay length, FM ratio, filter cutoff). We do **not**
   modulate pitch per-sample. This keeps the variation coherent within a note
   (no white-noise warble) and costs nothing in the inner loop.

---

## 3. The mechanism: a note-on PRNG draw

**Principle: draw once, at note-on; bake into voice parameters; never re-roll
per sample.** Within a single note the micro-offsets are constant, so the note is
*coherent* (a fixed, slightly-unique voicing) rather than chaotic. Across notes
they differ because `noise_seed` differs. This is structurally identical to how
`percussion.mjs` calls `rj()`/`rn()` once per voice at fire time.

Two distinct uses of the per-voice PRNG, which must not be confused:

- **Structural noise** (KS excitation seed, breath/bow/reed turbulence, hammer
  thump) — consumed *per-sample* in the inner loop. This already works via
  `xorshift32(&v->noise_seed)` and is the bulk of "organic" for noise-driven
  voices. A fresh `noise_seed` per trigger means a different excitation burst
  every pluck — exactly the snare-tail behaviour, applied to the string.
- **Parametric jitter** (detune, per-partial amp/phase, decay, cutoff, pan) —
  drawn *once at note-on* from the same PRNG, **before** the structural noise
  loop starts consuming it. Draw all parametric jitter first so the structural
  stream downstream is still effectively white.

A global **`organic`** amount (0..1, default **0.6** — see §5) scales every
parametric lever. `organic = 0` reproduces the old bit-identical behaviour
(precise contexts: tuners, test tones, click tracks); `organic = 1` is the
maximum still-tasteful spread. The default 0.6 places the amplitude lever at
≈ ±9–15%, squarely inside the percussion band.

---

## 4. What to vary, and by how much (bounded ranges)

All ranges below are the spread at **`organic = 1.0`**; the realized spread is
`range × organic`. Bounds are chosen so that at the default `organic = 0.6` the
amplitude/decay levers sit inside the percussion ±10–25% band, and the pitch
lever stays an order of magnitude below audible mistuning.

| # | Lever | Spread @ `organic=1` | @ default `0.6` | Rationale vs. perc reference |
|---|---|---|---|---|
| **a** | **Pitch micro-detune** (per voice, and per-partial for unison/phantom strings) | **±6 cents** | **±3.6 cents** | The drums jitter pitch by **0** for identity. We cannot use 0 (phase-locked partials comb), so we pick the smallest value that breaks phase-lock yet stays *well* under the ~±5–6 cent JND for melodic mistuning. A real piano's own 3-string detune is ±0.3–1.5 cents (dossier 01); ±3.6 cents is "alive," not "out of tune." **Hard ceiling: ±6 cents, never exceeded regardless of `organic`.** |
| **b** | **Per-partial / per-mode amplitude** (modal, additive, drawbar) | **±15%** (±1.4 dB) | **±9%** | Directly mirrors percussion's `rj(v, 0.10–0.18)`. This is *the* primary organic lever — it changes *which partial leads* this note without moving the spectral centroid enough to alter perceived timbre. Apply independently per partial; the sum stays level-normalized. |
| **c** | **Per-partial start phase** (additive/modal/drawbar) | **random 0..1** (full) | full | Free and identity-safe: start phase is inaudible in isolation but decorrelates stacked voices so two simultaneous same-notes don't phase-cancel. Percussion gets this implicitly (noise has no defined phase); pitched additive voices must do it explicitly. **Exception:** struck/plucked attacks where partials must align for a crisp transient — keep those phase-coherent (see §7 mallets). |
| **d** | **Excitation seed** (KS pluck, bow/breath/reed noise) | fresh `noise_seed` per trigger (already happens) | — | The structural analogue of a snare's noise burst differing every hit. No knob needed — it is on whenever the voice consumes `noise_seed`. `organic` does not gate this; turning it off would make plucks robotic. |
| **e** | **KS pluck-position** `β` (comb tap) | **±8% of β** | **±4.8%** | Models the player not hitting the identical spot twice. Shifts the comb notches slightly → timbral shimmer, no pitch change. Stays well inside the audible-but-subtle zone. |
| **f** | **FM index / ratio micro-jitter** | index **±6%**; ratio **±2 cents** | ±3.6% / ±1.2 cents | Index jitter ≈ the amplitude lever (it *is* a brightness/energy lever). Ratio jitter is held to the pitch ceiling (b) because an FM ratio error *is* a detune of the sidebands. |
| **g** | **Attack-time micro-jitter** | **±12%** | **±7.2%** | Percussion staggers multi-burst attacks deliberately (clap 0.005/0.015/0.025); generic attack jitter of ±7% reproduces the "no two strokes land identically" feel without smearing transients. Applied to `v->attack`. |
| **h** | **Velocity / amplitude micro-jitter** (whole-voice gain) | **±8%** (±0.7 dB) | **±4.8%** | The conservative end of percussion's volume band, applied to the *whole* voice (vs. per-partial in **b**). Keeps a repeated note from being a perfect amplitude copy. |
| **i** | **Decay-time micro-jitter** (T60 / per-mode tau) | **±15%** | **±9%** | Straight from percussion's tail jitter `rj(d, 0.18–0.25)`. Apply to per-partial/per-mode decay multipliers and to overall release. The ring-out length is *audibly* alive and very identity-safe. |
| **j** | **Filter cutoff micro-jitter** (subtractive) | **±5%** (≈ ±0.8 semitone of cutoff) | **±3%** | Analog filters drift; ±3% cutoff is "warm analog," not a different patch. Below the threshold where it reads as a timbre change. |
| **k** | **Stereo pan micro-jitter** (optional) | **±0.05** | **±0.03** | Exactly the percussion base pan spread `rn(-0.02..0.06)`. Adds spatial life. Optional — skip for centered mono leads. |

### 4.1 The two invariants

Whatever an instrument does with the levers above, two perceptual guarantees hold
at **all** values of `organic ∈ [0,1]`:

1. **Pitch stays within ±5 cents** of nominal. The pitch lever (a) is capped at
   ±6 cents *spread* (so ±3 cents from center at the default, ±6 cents absolute
   worst case at `organic=1`) and never scales past that hard ceiling. Phantom/
   unison detune layers count against the same budget.
2. **Spectral centroid stays stable.** Per-partial amplitude jitter is
   **zero-mean and independent** across partials, so it perturbs *which* partial
   leads without shifting the centroid in expectation. Never apply a *correlated*
   tilt (that *would* be a timbre change — it's a different instrument, not a
   different stroke).

---

## 5. The single global knob: `organic`

```c
// In ACAudio (engine-global). Default tuned to the percussion feel.
double organic_amount;   // 0.0 = bit-identical, 1.0 = max tasteful spread.
                         // DEFAULT 0.6  → amp lever ≈ ±9%, inside the
                         //                percussion ±10–25% band; pitch ≈ ±3.6c.
```

- **Why 0.6 is the default.** At 0.6 the dominant lever (per-partial amplitude,
  ±15% × 0.6 ≈ ±9%) lands at the *low* end of percussion's measured ±10–18%
  band — deliberately a touch gentler than drums, because pitched sustained tones
  expose variation more than transient drum hits do. Decay jitter at ±9% matches
  drum tails. Pitch at ±3.6 cents is sub-JND. The result *feels* like the hats
  and snares without ever sounding detuned.
- **One knob, global, exposed.** A single `audio_set_organic(double)` (or a JS
  binding) scales every lever. Turn it down for precise contexts (tuner, test
  tone, metronome); leave at 0.6 for music. Per-family multipliers (§7) ride on
  top of this one global so the *relative* character is preserved as you scale.
- **It is bounded, seeded, and coherent — therefore "relatively consistent."**
  Bounded: every lever has a hard ceiling (§4). Seeded: `noise_seed` is a pure
  function of `next_id`, so a trigger's sound is reproducible given its seed.
  Coherent: parametric jitter is drawn once at note-on and frozen for the note's
  life — no drift, no warble, no runaway. This is precisely why percussion
  sounds varied-but-consistent, and the same property carries over.

---

## 6. Per-family application

Each GM family leans on different levers (cross-referenced against dossiers 01,
02, 03, 04). `mul` is a per-family multiplier on the global `organic`, capturing
"how much variation suits this instrument." Pitch is *always* the tightest lever.

| GM range | Family | Primary engine (per dossier) | Dominant organic levers | `mul` | Notes |
|---|---|---|---|---|---|
| 1–4 | Acoustic piano | modal additive + inharmonicity (01) | **b** per-partial amp, **d/hammer** thump seed, **a** 3-string phantom detune, **i** per-partial decay | 0.8 | Phantom-string detune (±0.3–1.5c, dossier 01) *is* lever (a) under the ±6c cap; fresh hammer-noise seed per strike. |
| 5–6 | Electric piano (FM) | 2–4 op FM tine/reed (01) | **f** index/ratio jitter, **g** attack (bark), **h** velocity | 0.7 | Index jitter = attack "bark" variation; ratio jitter stays at pitch ceiling. |
| 7–8 | Harpsichord / Clavi | extended KS (01) | **d** excitation seed, **e** pluck-position β, **b** | 0.9 | Plucked → excitation+β dominate, like a snare's noise burst per hit. |
| 9–15 | Chromatic perc (mallets/bells) | modal resonator bank (01) | **b** per-mode amp (strike-position), **i** per-mode decay, **d** mallet-click seed | 1.0 | Highest `mul`: these *are* percussion. Strike-position → per-mode amplitude jitter is the exact analogue of which drum partial leads. **Keep attack phase-coherent** for a crisp strike (§4 lever c exception). |
| 16 | Dulcimer | KS unison strings (01) | **a** unison detune, **d** excitation, **e** β | 0.9 | Detune across the 2–3 courses uses lever (a) under cap. |
| 17–19 | Hammond organ | additive drawbars + Leslie (01) | **least variation:** **c** per-drawbar phase, tiny **a** tonewheel detune, **g** key-click seed | 0.3 | Organs are the *most consistent* — an electromechanical machine. Mostly key-click variation + a hair of Leslie/tonewheel phase. Low `mul` by design. |
| 20–24 | Pipe / reed / accordion | additive ranks / free-reed subtractive (01) | **a** rank/reed detune (musette), **j** cutoff, breath/chiff seed | 0.5 | Musette detune is intrinsic to the patch; the *organic* part is the small extra wobble + chiff-noise seed. |
| 25–32 | Guitar (all) | extended KS + waveshaper (01) | **d** excitation, **e** β, **b**, **j** on driven variants | 0.9 | Pick-vs-finger excitation seed differs every pluck. Drive variants jitter the pre-gain a touch via **h**. |
| 33–40 | Bass | KS (dark) / subtractive synth (02) | KS: **d/e**; subtractive: **j** cutoff, **a** osc detune | 0.7 | Synth basses lean on cutoff drift (j); acoustic basses on excitation. |
| 41–47 | Strings (solo/section) | bowed waveguide / KS pizz / modal (02) | bow-noise **d**, **a** vibrato-rate + bow detune, **g** | 1.0 | Bowed strings are the headline organic case: fresh bow-noise seed + small vibrato-rate jitter per note = no two strokes alike. Pizz uses excitation+β. |
| 48 | Timpani | modal + pitch drop (02) | **b** per-mode amp, **i** decay, **d** strike seed | 1.0 | Membrane modes; same as mallets. |
| 49–55 | Ensemble (supersaw/strings) | supersaw + chorus (02) | **a** per-saw detune, **c** phase, **j** cutoff | 0.6 | Supersaw is *built* from detune; the organic add is per-note phase + small detune wobble on top of the fixed spread. |
| 52–54 | Choir / voice | formant synthesis (02) | breath/aspiration seed **d**, **a** per-voice detune, **g** onset | 0.9 | Per-singer detune + aspiration noise = a believable section. |
| 56 | Orchestra hit | cluster + transient (02) | **b**, **i**, **d** transient seed | 0.8 | Mostly transient/decay jitter. |
| 57–63 | Brass | brass waveguide / subtractive (02) | breath/lip-noise **d**, **g** attack (blat), **a** small detune, **j** | 0.9 | Attack-timing + breath-noise jitter give the human "blat"; section stacks add detune. |
| 64 | Reeds-as-brass / synth brass | subtractive (02) | **j** cutoff, **a** detune, **g** | 0.7 | Filter drift dominates. |
| 65–72 | Reed (sax/oboe/clarinet…) | reed waveguide nonlinearity (03) | reed-noise **d**, **g** attack chiff, **a** vibrato-rate, breath **h** | 1.0 | Reed turbulence seed per note is the core; vibrato-rate jitter prevents mechanical LFO lock. |
| 73–80 | Pipe (flute/whistle/recorder) | Cook flute waveguide (03) | breath-noise **d**, **a** vibrato-rate, **g** chiff, **h** breath | 1.0 | Already partly organic via `generate_whistle_sample` breath noise (audio.c:195); add vibrato-rate jitter + chiff variation. The flute's 5 Hz vibrato (audio.c:188) should get ±10% rate jitter. |
| 81–88 | Synth lead | subtractive named-waveforms (03) | **j** cutoff drift, **a** detune, **c** phase | 0.6 | Analog-style cutoff/detune drift; keep it tasteful so leads stay tuned. |
| 89–96 | Synth pad | detuned multi-osc + slow sweep (03) | **a** per-osc detune, **c** phase, **j** slow cutoff | 0.5 | Pads are wide already; small per-osc detune + phase decorrelation. |
| 97–104 | Synth FX | PhISEM / pads / FM bells (04) | **d** PhISEM particle seed, **b**, **i** | 0.9 | PhISEM is *inherently* stochastic (random droplet timing) — the seed is the whole point. |
| 105–112 | Ethnic | KS + sympathetic / modal / reed / bowed (04) | per-instrument: **d/e** (sitar/koto/banjo), **a** (sympathetic detune), reed **d** (bagpipe) | 0.9 | Sympathetic strings → small detune across them (a). Sitar sawari buzz benefits from excitation seed variation. |
| 113–120 | Percussive (bells/taiko/steelpan…) | modal banks / membrane modes (04) | **b** per-mode amp, **i** decay, **d** strike seed | 1.0 | Pure percussion — top `mul`, same calibration as the drum reference itself. |
| 121–128 | Sound FX (breath/seashore/bird/heli/applause/gunshot) | filtered noise / PhISEM / `WAVE_GUN` (04) | **d** seed is everything; **i**, pan **k** | 1.0 | These are *defined* by noise — a fresh seed per trigger is the entire organic story. Gunshot reuses `WAVE_GUN`, already seeded. |

**Reading the table:** organs (0.3) and pads/leads (0.5–0.6) are deliberately the
*most consistent* — machine-like, electromechanical. Mallets, percussion, bowed
strings, reeds/pipes, and noise-FX sit at `mul ≈ 1.0` — these are where the human
hand and turbulent air live, and they carry the most variation, exactly as the
hi-hats and snares do in the reference kit.

---

## 7. Engine-level guarantees (why it stays consistent, not drifting)

1. **Bounded.** Every lever in §4 has a hard numeric ceiling. The pitch ceiling
   (±6 cents) is enforced *after* `mul` and `organic` multiply, so no family or
   knob setting can detune a note audibly.
2. **Seeded & reproducible.** `noise_seed = next_id * 2654435761u` is a pure
   function of the trigger index — a given trigger is fully reproducible. Useful
   for testing (assert two triggers differ, but a *replayed* trigger with the
   same id is identical).
3. **Coherent within a note.** Parametric jitter is drawn **once** at note-on and
   frozen. No per-sample re-rolling of pitch/amp/cutoff → no warble, no drift, no
   divergence over a long held note. The only per-sample randomness is structural
   excitation noise, which is *supposed* to be broadband.
4. **Zero-mean & independent.** Per-partial jitter averages to no spectral tilt,
   so timbre identity is statistically preserved across many triggers even as any
   single trigger is unique.
5. **One global escape hatch.** `organic = 0` restores exact, deterministic,
   bit-identical synthesis for any context that needs it.

---

## 8. Reusable C sketch (matches `audio.c` style)

Add near the top of `audio.c`, alongside `xorshift32` (audio.c:100) and `clampd`
(audio.c:109). These are the helpers every per-instrument implementation calls at
note-on. They draw from the voice's own `noise_seed` so the draws are coherent
within the trigger and varied across triggers.

```c
// ============================================================
// Bounded per-note stochasticism (see docs/gm-synthesis/00-stochasticism.md)
// All draws come from the voice's per-trigger noise_seed (seeded in
// audio_synth from next_id). Call these ONCE at note-on and bake the
// result into voice params — never per-sample (phase-increment rule).
// ============================================================

// Global "organic" amount: 0 = bit-identical, 1 = max tasteful spread.
// Default 0.6 places the amplitude lever inside the percussion ±10-25% band.
// Lives on ACAudio; exposed via audio_set_organic(). A file-static mirror
// keeps the note-on helpers dependency-free in the inner code.
static double g_organic_amount = 0.6;

void audio_set_organic(double amt) {
    g_organic_amount = clampd(amt, 0.0, 1.0);
}

// Uniform [0,1) from the voice PRNG.
static inline double voice_rand_unit(ACVoice *v) {
    return (double)xorshift32(&v->noise_seed) / (double)UINT32_MAX;
}

// Bipolar [-1,1] from the voice PRNG. The workhorse for every jitter lever.
static inline double voice_rand_bipolar(ACVoice *v) {
    return voice_rand_unit(v) * 2.0 - 1.0;
}

// Cents → frequency ratio. 1200 cents = 1 octave. cents_to_ratio(0)==1.0.
static inline double cents_to_ratio(double cents) {
    return pow(2.0, cents / 1200.0);
}

// ---- Lever helpers. `mul` is the per-family multiplier (§6, default 1.0). ----

// Bounded multiplicative jitter around `center` by ±`frac` (the percussion
// `rj` idiom): center * (1 ± frac*organic*mul*u). Use for amp, decay, attack,
// cutoff, FM index. Caller picks `frac` from the §4 table (amp 0.15, decay
// 0.15, attack 0.12, cutoff 0.05, fm index 0.06, ...).
static inline double voice_jitter(ACVoice *v, double center,
                                  double frac, double mul) {
    double u = voice_rand_bipolar(v);
    return center * (1.0 + frac * g_organic_amount * mul * u);
}

// Bounded pitch detune in cents → ratio, HARD-CAPPED at ±6 cents regardless
// of organic/mul so perceived pitch never moves audibly (invariant §4.1).
// `spread_cents` is the per-lever spread (default 6.0); pass the partial/voice
// frequency, get the detuned frequency back.
#define ORGANIC_MAX_CENTS 6.0
static inline double voice_detune(ACVoice *v, double freq,
                                  double spread_cents, double mul) {
    double cents = spread_cents * g_organic_amount * mul * voice_rand_bipolar(v);
    if (cents >  ORGANIC_MAX_CENTS) cents =  ORGANIC_MAX_CENTS;
    if (cents < -ORGANIC_MAX_CENTS) cents = -ORGANIC_MAX_CENTS;
    return freq * cents_to_ratio(cents);
}

// Random start phase [0,1) for an additive/modal partial. Decorrelates
// stacked same-notes so they don't phase-cancel. Free + identity-safe.
static inline double voice_rand_phase(ACVoice *v) {
    return voice_rand_unit(v);
}

// Small bounded pan offset (±0.05 spread), added to the voice's base pan.
// Mirrors percussion's rn(-0.02..0.06) spread. Result clamped to [-1,1].
static inline double voice_pan_jitter(ACVoice *v, double base_pan, double mul) {
    double off = 0.05 * g_organic_amount * mul * voice_rand_bipolar(v);
    return clampd(base_pan + off, -1.0, 1.0);
}
```

### 8.1 Usage at note-on (example: a modal mallet voice)

```c
// In audio_synth(), after the base init, for WAVE_MALLET (family mul = 1.0):
// IMPORTANT: draw all PARAMETRIC jitter first, before any per-sample
// structural noise consumes the seed, so the downstream stream stays white.
const double mul = 1.0;  // mallets/percussion: full organic (§6)
for (int m = 0; m < v->nmodes; m++) {
    double fm = f0 * mode_ratio[m];
    fm = voice_detune(v, fm, 6.0, mul);                 // §4a  ±≤6 cents
    v->m_inc[m]   = fm / sr;
    v->m_amp[m]   = voice_jitter(v, base_amp[m], 0.15, mul);  // §4b  ±15%
    // Struck attack → keep partials phase-COHERENT for a crisp transient
    // (§4 lever c exception). Use 0.0, NOT voice_rand_phase, for mallets.
    v->m_phase[m] = 0.0;
    double tau    = voice_jitter(v, base_tau[m], 0.15, mul);  // §4i  ±15% decay
    v->m_dec_mult[m] = exp(-1.0 / (tau * sr));
}
v->pan = voice_pan_jitter(v, v->pan, mul);              // §4k  ±0.05
// (Per-sample mallet-click noise then consumes noise_seed in the inner loop.)
```

For a sustained additive voice (organ, choir, supersaw) the only change is
`v->p_phase[k] = voice_rand_phase(v);` instead of `0.0`, because there is no
transient to keep coherent and phase decorrelation is desirable.

---

## 9. Implementation checklist

- [ ] Add `audio_set_organic()` + `g_organic_amount` (default 0.6) and a JS
      binding so pieces / the global config can tune it.
- [ ] Extend the note-on seeding (audio.c:3359) so the new GM wave types
      (`WAVE_EPIANO/MALLET/ORGAN/PLUCK/FM` etc.) also get `noise_seed` set.
- [ ] Add the §8 helpers next to `xorshift32`.
- [ ] In each new per-instrument note-on path, draw parametric jitter **first**,
      apply the §6 family `mul`, and respect the §4 ranges.
- [ ] Keep all pitch jitter routed through `voice_detune` (hard ±6c cap).
- [ ] Verify the two invariants (§4.1): a tuner reads within ±5 cents; the
      long-term spectral centroid is stable across many triggers.

---

*This dossier (00) is the calibration spine for dossiers 01–04. Numbers anchored
to `percussion.mjs` (amp ±10–25%, decay ±18–25%, pan ±0.02–0.06, pitch 0) and the
engine's existing `xorshift32` + per-voice `noise_seed`. Cross-referenced against
all four family dossiers (01 piano/mallet/organ/guitar, 02 bass/strings/ensemble/
brass, 03 reed/pipe/synthlead/synthpad, 04 synthfx/ethnic/percussive/soundfx).*
