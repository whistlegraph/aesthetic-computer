# GM Synthesis Dossier 01 — Piano, Chromatic Percussion, Organ, Guitar

Real-time **algorithmic** synthesis recipes for General MIDI programs 1–32, written
as an implementation spec for the Aesthetic Computer native C audio engine
(`fedac/native/src/audio.c`). No sample playback — every voice is generated from
oscillators, delay lines, and filters. Target: 44.1 / 48 / 192 kHz, 32-voice
polyphony, per-voice / per-sample render in `generate_sample()`.

> **Scope note.** GM program numbers below are 1-based (Acoustic Grand = 1). The
> four families documented here are 1–32. Three later dossiers will cover the
> remaining 96 programs.

---

## 0. How these fit the existing engine

Before the per-instrument sections, here is the mapping to what already exists in
`audio.c`, so the implementer reuses primitives instead of inventing parallel
machinery.

### 0.1 Existing voice infrastructure (read this first)

The `ACVoice` struct (audio.h:68–228) already carries everything most of these
algorithms need:

- **Phase accumulators** — `double phase`, advanced by `frequency / sample_rate`
  in `generate_sample()`. The repo convention (and a hard-won lesson in
  `MEMORY.md`: *"long sine phase-increment not sin(TAU*f*t)"*) is to advance a
  phase register and read a **wavetable**, never call `sin()` per sample for
  sustained tones. Additive partials must each carry their own phase register.
- **Delay-line buffers** — `float whistle_bore_buf[2048]` + `int whistle_bore_w`
  (write cursor) and `float whistle_jet_buf[512]`. These are **reused per wave
  type** (a voice is only ever one type at a time). The harp's Karplus-Strong
  string lives in `whistle_bore_buf`; every plucked/struck-string instrument
  here reuses the same field. There is also `whistle_frac_read()` for
  fractional (interpolated) delay reads — essential for in-tune KS.
- **Filter state** — biquad fields (`noise_b0..a2`, `noise_x1..y2`) and one-pole
  state (`harp_lp1`, `whistle_lp1`, `gun_bore_lp`). One-pole and biquad helpers
  already exist (`setup_noise_filter`). The gun model shows the pattern for
  **3 parallel biquad body modes** (`gun_body_a1[3]`, `gun_body_y1[3]`) — this
  is *exactly* the modal-resonator pattern the mallet/bell family needs, just
  more modes.
- **Envelopes** — `compute_envelope(v)` (attack / decay / duration / fade) and
  per-sample exponential decay multipliers (the gun model's
  `gun_*_decay_mult = exp(-1/(tau*sr))` idiom). Mallets and pianos want
  per-partial exponential decay multipliers exactly like this.
- **Frequency smoothing** — `target_frequency` → `frequency` one-pole glide
  already in the render loop.

### 0.2 Reuse map (primitive → instrument family)

| Existing primitive | Reused / extended by |
|---|---|
| `generate_harp_sample` (canonical KS + Jaffe-Smith stretch) | All guitars, harpsichord, clavi, dulcimer, music box damper |
| `whistle_bore_buf` fractional delay + `whistle_frac_read` | Every string model (KS / waveguide) + comb pluck-position filter |
| `gun_body_*[3]` parallel biquad bank | Mallets (vibraphone/marimba/glock/xylophone/tubular bells) modal banks, extended to 3–8 modes |
| Wavetable phase-increment oscillator | Organ drawbars (additive sines), FM operators (EPs, bells, clav) |
| `compute_envelope` + per-sample `exp` decay mult | Per-partial / per-mode decay on all struck voices |
| `setup_noise_filter` biquad | Hammer-noise thump, breath/wind for accordion/harmonica, pick-scrape |
| `drive_mix` / tanh soft-clip master FX | Overdrive/distortion guitar waveshaper (but apply **per-voice** for these) |

### 0.3 Proposed new `WaveType` enum entries

The cleanest fit is a small set of new families rather than 32 enum members.
Suggested additions (parameterised by a `gm_program` field on the voice that
selects the sub-variant — mirroring how `gun_preset` selects a `GunPresetParams`
row):

```c
WAVE_EPIANO,    // FM tine/reed (Rhodes, Wurli, DX, FM EP, clav-FM)
WAVE_MALLET,    // modal bank (celesta..tubular bells, dulcimer)
WAVE_ORGAN,     // additive drawbars + optional Leslie
WAVE_PLUCK,     // extended-KS string (guitars, harpsichord, clavi)
WAVE_FM,        // generic 2–4 op FM (bells/celesta/music-box voices)
```

Each carries a `const *Params` table row keyed by GM program, same pattern as
`gun_presets[GUN_PRESET_COUNT]`. Variant timbre lives in data, not code.

---

# Family 1 — Piano (GM 1–8)

**Family verdict:** The struck steel string is *the* hard case. Two viable
algorithmic routes: (a) **modal/additive with inharmonic stretched partials**
(cheap, the engine already had this — see audio.c:515 "Old modal-additive synth
removed"), or (b) **digital-waveguide / commuted synthesis** (Smith & Van Duyne
1995). For real-time 32-voice on the AC target, **modal additive with
inharmonicity** is the right default for acoustic pianos; the electric pianos
(5,6) want **FM tine models** (Chowning), and harpsichord/clavi (7,8) want
**extended Karplus-Strong** (Jaffe-Smith), because they are plucked/struck-
damped, not free-ringing struck strings.

### Core acoustic-piano algorithm (shared by GM 1–4)

**Inharmonicity.** A real piano string is stiff, so partials are stretched:

```
f_n = n · f0 · sqrt(1 + B·n²)          (Fletcher & Rossing 1998, eq. 12.12)
```

`B` is the inharmonicity coefficient: ~0.0001 in the bass register growing to
~0.004 in the treble. This stretch is *the* perceptual signature of a piano vs.
a pure harmonic tone, and it's why detuned-octave "stretch tuning" sounds right.

**Per-sample model** (10 partials, the count the engine previously used):

```c
// state per voice: phase[10], amp[10], dec_mult[10], freq[10]
double s = 0.0;
for (int k = 0; k < NPART; k++) {
    s += v->p_amp[k] * wt_sin(v->p_phase[k]);   // wavetable sine, NOT sinf()
    v->p_phase[k] += v->p_finc[k];              // precomputed inc = f_n/sr
    if (v->p_phase[k] >= 1.0) v->p_phase[k] -= 1.0;
    v->p_amp[k] *= v->p_dec_mult[k];            // exp decay, high partials die first
}
// + hammer thump: short LPF noise burst, ~5ms exp decay (reuse setup_noise_filter)
// + 3 mistuned "phantom" fundamentals (±0.3–1.5 cents) → inter-string beating
return (s + hammer) * compute_envelope(v);
```

Key parameters set at note-on (velocity → brightness): higher velocity adds
upper partials and increases hammer-noise gain. Decay multipliers:
`dec_mult[k] = exp(-1.0 / (tau_k * sr))` with `tau_k` shrinking for higher `k`
(treble partials T60 ≈ 0.3–1 s; fundamental T60 ≈ 5–20 s in bass).

**References (already cited in audio.h:193–215):**
- Fletcher, H. & Rossing, T.D. (1998). *The Physics of Musical Instruments*, 2nd
  ed., Springer, Ch. 12 (inharmonicity). 
- Bank, B. (2000). *Physically-Based Sound Modeling of the Piano*, M.Sc. thesis,
  BME Budapest. — modal/additive grand with stretched partials.
- Bank, B. & Välimäki, V. (2003). "Robust Loss Filter Design for Digital
  Waveguide Synthesis of String Tones," *IEEE SP Letters* 10(1), 18–20.
- Smith, J.O. & Van Duyne, S.A. (1995). "Commuted Piano Synthesis," *Proc. ICMC*,
  Banff. — hammer + soundboard impulse collapsed into the excitation.
- Smith, J.O. *Physical Audio Signal Processing*, CCRMA — https://ccrma.stanford.edu/~jos/pasp/

---

## GM 1 — Acoustic Grand Piano

- **Method:** Modal additive, 10–12 inharmonic partials + hammer thump + 3-string
  phantom detune. (Default `B` schedule above.)
- **Timbre params:** Bright attack, long sustain; `B` small in bass, full
  velocity→partial map. Wide stereo from string position.
- **C sketch:** Shared core above; this is the reference voice.

## GM 2 — Bright Acoustic Piano

- **Method:** Same modal core.
- **Distinguishing params:** Boost upper-partial amplitudes (multiply `amp[k]`
  for `k≥4` by ~1.3–1.6), slower treble decay, slightly **harder hammer**
  (more noise-burst HF, shorter noise tau). Optional one-pole high-shelf on
  output.

## GM 3 — Electric Grand Piano

- **Method:** Modal core but **fewer, cleaner partials** (6–8) with reduced
  inharmonicity (`B` ≈ half of acoustic) — models a sampled/amplified CP-70-style
  grand. Add a gentle tine-like 2nd-partial emphasis.
- **Distinguishing params:** Less hammer noise, tighter (more harmonic) spectrum,
  a touch of the FM tine shimmer from GM 5 mixed at low level. Light tanh drive
  (`drive_mix` per-voice ~0.1) for the "electrified" edge.

## GM 4 — Honky-tonk Piano

- **Method:** Modal core, **dual detuned voices**. The defining trait is two
  strings per note deliberately mistuned.
- **Distinguishing params:** Render the partial set **twice** with the second
  copy's `f_inc` scaled by ~±10–18 cents (`1.006`–`1.010`), summed. This
  produces the characteristic slow chorus beating of a worn upright. Slightly
  shorter sustain, more hammer clack.

---

## GM 5 — Electric Piano 1 (Rhodes / tine)

- **Method:** **FM tine model** (Chowning FM) — the authentic and cheap route.
  The Rhodes tine is a struck asymmetric tuning-fork sensed by an electromagnetic
  pickup whose **asymmetric nonlinearity** generates the bark. Two routes:
  1. *FM (recommended for CPU):* 2-operator FM. Carrier at `f0`, modulator at
     `c:m ≈ 1:1` for body plus a **high-ratio modulator** (`m ≈ 14:1`) at low
     index to inject the metallic "tine ping" on attack. Modulation index
     **decays exponentially** so the bark is loud at attack then mellows to a
     near-sine bell — exactly Chowning's bell envelope idea applied to a tine.
  2. *Physical (reference quality):* mass-spring tine + asymmetric pickup
     waveshaper (Pfeifle & Bader, DAFx 2017).
- **FM recipe (per-sample):**

```c
// carrier c, modulator m; both wavetable-sine, phase-increment
double mod = wt_sin(v->fm_mphase) * v->fm_index;        // fm_index decays per-sample
double car = wt_sin(v->fm_cphase + mod);
v->fm_cphase += v->fm_cinc;   v->fm_mphase += v->fm_minc;
v->fm_index *= v->fm_index_dec;                          // exp decay → mellowing
// + small attack "tine" operator at ~14*f0, its own fast-decaying index
return car * compute_envelope(v) * vel_gain;
```

- **Distinguishing params:** velocity → attack index (harder = more bark);
  long bell-like decay; pickup asymmetry via mild `tanh(a + g·x)` (DC-offset bias
  then DC-block) to add even harmonics — the Rhodes "growl."
- **Refs:** Chowning, J. (1973). "The Synthesis of Complex Audio Spectra by Means
  of Frequency Modulation," *JAES* 21(7), 526–534. Pfeifle, F. & Bader, R. (2017).
  "Real-Time Physical Model of a Wurlitzer and Rhodes Electric Piano," *Proc.
  DAFx-17* — http://www.dafx17.eca.ed.ac.uk/papers/DAFx17_paper_79.pdf

## GM 6 — Electric Piano 2 (Wurlitzer / DX-EP)

- **Method:** Same FM-tine engine, **reed** flavour (Wurlitzer) or glassy DX-EP.
- **Distinguishing params vs GM 5:** Wurlitzer reed → **stronger asymmetric pickup
  nonlinearity** (more even harmonics, hollow/reedy, faster decay), modulator
  ratio nearer `c:m = 1:2`, more aggressive `tanh` bias. The "DX7 E.PIANO 1"
  variant: 4-op stack, `c:m` whole-number ratios, crystalline attack, longer
  release. Add a touch of the bell index for the FM-piano gloss.

## GM 7 — Harpsichord

- **Method:** **Extended Karplus-Strong** (plucked, not struck). Reuse
  `generate_harp_sample` engine. The plectrum pluck is bright and the string is
  hard-damped → short, even decay with characteristic **pluck-position comb**.
- **EKS additions over the bare harp:**
  - *Pick-position comb:* `y = x - x[n - βN]`, `β ≈ 0.13` (pluck near the
    bridge) → bright, nasal notches (Jaffe-Smith `H_β(z)=1 - z^{-⌊βN+0.5⌋}`).
  - *Damping filter:* one-pole loop LPF with low cutoff so high partials persist
    less than a guitar (brighter, drier than nylon).
  - *Tuning allpass:* `H_η(z) = -(η - z^{-1})/(1 - η z^{-1})` for in-tune
    fractional delay (already implicit in `whistle_frac_read`, but the allpass
    is phase-flatter for sustained tones).
- **Distinguishing params:** very bright seed spectrum (don't pre-smooth the
  noise as much as nylon), short-ish decay, the comb is the signature. Two
  unison strings (8'+4') optional: render at `f0` and `2·f0`.
- **Refs:** Karplus & Strong 1983; Jaffe & Smith 1983 "Extensions of the
  Karplus-Strong Plucked-String Algorithm," *CMJ* 7(2), 56–69; Smith *PASP* EKS
  page — https://ccrma.stanford.edu/~jos/pasp/Extended_Karplus_Strong_Algorithm.html

## GM 8 — Clavi (Clavinet)

- **Method:** **Extended KS** with a struck-string excitation + **strong pickup
  coloration**. A Clavinet string is struck by a rubber tangent against an anvil
  → percussive, funky, twangy, with magnetic pickup like an electric guitar.
- **Distinguishing params:** pluck position near the very end (`β ≈ 0.05`) for a
  thin bright tone; **fast decay + hard mute on note-off** (the Clavi's defining
  staccato); pass the KS output through a `tanh` waveshaper for the wah-friendly
  electric bite; optional FM-tine attack click. Velocity → brightness via the
  dynamics LPF `H_L(z)=(1-R_L)/(1-R_L z^{-1})`, `R_L = e^{-πLT}`.

---

# Family 2 — Chromatic Percussion (GM 9–16)

**Family verdict:** Every one of these is a struck metal/wood bar, tube, or
string with a small number of strong, **inharmonic** modes and a percussive
exponential decay. The unanimous best method is **modal synthesis**: a bank of
parallel resonators (decaying sinusoids), one per measured mode, excited by a
short impulse/noise burst. This is the *same architecture as the gun body-mode
bank* (`gun_body_*[3]`) extended to N modes. It is extremely cheap (N phase-
increment sines × per-mode exp decay) and the **modal ratios are the entire
identity** of each instrument — so the data tables below are the spec.

### Core mallet/modal algorithm (shared GM 9–16)

```c
// Per voice: M modes. ratio[m] (×f0), amp[m], dec_mult[m], phase[m], inc[m].
// Excitation: at note-on seed each mode's amplitude (= strike energy × mode
// participation); optionally add a 1–3 ms noise/mallet-click transient.
double s = 0.0;
for (int m = 0; m < v->nmodes; m++) {
    s += v->m_amp[m] * wt_sin(v->m_phase[m]);
    v->m_phase[m] += v->m_inc[m];                 // inc = f0*ratio[m]/sr
    if (v->m_phase[m] >= 1.0) v->m_phase[m] -= 1.0;
    v->m_amp[m] *= v->m_dec_mult[m];              // exp decay; high modes faster
}
return s * compute_envelope(v);
```

> Alternative for very many modes / true impulse response: a parallel **biquad
> resonator bank** excited by a delta (the `gun_body` 2-pole form), each tuned to
> `f0·ratio[m]` with `Q` set from desired T60. Equivalent output; biquads are
> better when you want to re-excite (rolls). For pure struck one-shots the
> decaying-sinusoid additive form above is cheaper and click-free.

**References (modal ratios):**
- Fletcher & Rossing (1998), *The Physics of Musical Instruments*, Ch. 18–21
  (bars, plates, bells).
- CCRMA Music 150/152 percussion notes — https://ccrma.stanford.edu/CCRMA/Courses/150/percussion.html
- Cook, P.R. (2002). *Real Sound Synthesis for Interactive Applications*, AK
  Peters — modal synthesis chapters + STK `ModalBar`.
- Bilbao, S. (2009). *Numerical Sound Synthesis*, Wiley — bar/plate FD models
  (reference, heavier than needed here).

---

## GM 9 — Celesta

- **Method:** Modal — struck steel bar over a resonator box. Near-harmonic,
  bell-like but gentle. Equivalent: **2-op FM** (DX "CELESTA").
- **Modal ratios:** approx `1.0, 4.0, ~10.8` (steel bar, like a soft
  glockenspiel) with the upper modes weak. Decay moderate (T60 ~1–2 s).
- **FM alt:** `c:m = 1:4`, low index, fast index decay → clean bell ping.
- **Params:** soft mallet → small HF content; gentle attack click.

## GM 10 — Glockenspiel

- **Method:** Modal — short steel bar, brilliant.
- **Modal ratios (transverse bar, free-free):** **`1.0 : 2.76 : 5.40 : 8.90`**
  (the classic free-free bar eigenvalue ratios; CCRMA). Only the fundamental
  rings long; upper modes die fast.
- **Params:** very bright strike, high-pitched (G5–C8), short-to-medium decay,
  hard mallet → strong attack transient. T60 of mode 0 ~1–3 s, others < 0.3 s.

## GM 11 — Music Box

- **Method:** Modal — plucked steel comb tooth. Like a tiny glockenspiel with a
  **plucked** (not struck) excitation → softer attack, pure-ish tone.
- **Modal ratios:** dominant fundamental + weak `~6.3, ~17` overtones (cantilever
  beam, clamped-free: `1.0 : 6.27 : 17.55 : ...`). Cantilever ratios, not
  free-free — this is what makes a music box sound thinner/purer than a glock.
- **Params:** soft pluck transient, medium decay, slight per-note level jitter
  for the mechanical feel; add faint comb/mechanism noise.

## GM 12 — Vibraphone

- **Method:** Modal — deeply undercut **aluminium** bar (long decay) + tremolo.
- **Modal ratios:** **`1.0 : 4.0 : ~9.6`** (first overtone tuned to *two octaves*,
  the deliberate vibe tuning; CCRMA). Aluminium → very long decay.
- **Params:** long T60 (3–8 s), **amplitude tremolo** from the motor-driven
  resonator discs — implement as a slow LFO on output gain (~4–7 Hz, depth
  ~0.3), the vibraphone's signature. Soft yarn mallet → minimal HF.

## GM 13 — Marimba

- **Method:** Modal — undercut **rosewood/synthetic** bar + tube resonator.
- **Modal ratios:** **`1.0 : 4.0 : 9.2`** (second partial at two octaves, third
  ~3 octaves + minor third; CCRMA). Wood → faster decay than vibe.
- **Params:** warm, dark (low HF), medium-short decay (T60 ~0.5–1.5 s); tube
  resonator emphasises the fundamental → strong mode-0 amplitude, weak uppers.

## GM 14 — Xylophone

- **Method:** Modal — less-undercut wooden bar, bright and dry.
- **Modal ratios:** **`1.0 : 3.0 : ~6.0`** (first overtone at the *twelfth*,
  ratio 3.0 — the xylophone tuning, vs marimba's 4.0; CCRMA). 
- **Params vs marimba:** higher first-overtone (3.0 not 4.0) → harder, woodier;
  shorter decay; brighter strike transient. This single ratio difference (3 vs 4)
  is the marimba/xylophone distinction.

## GM 15 — Tubular Bells (Chimes)

- **Method:** Modal — long brass tube. **Strongly inharmonic**, with a *phantom*
  strike pitch.
- **Modal ratios:** the audible strike tone arises from modes **4:5:6 ≈ 2:3:4**,
  so the ear infers a fundamental an octave **below** mode 4 (missing-fundamental
  effect). Practical mode set (relative to perceived pitch): approx
  **`2.0 : 3.0 : 4.16 : 5.43 : 6.79 : 8.21`** (brass tube; the cluster is what
  makes chimes shimmer). Very long decay.
- **FM alt (classic):** Chowning bell — inharmonic `c:m` ratio with
  exponentially-decaying index; or Hind's 3-pair tubular-bell FM
  (`c:m` pairs `2.0:5.0`, `0.6:4.8`, `0.22:0.83`). 2-op `m ≈ 3.5·c` is the
  quick recipe (iastate / SOS).
- **Params:** long T60 (5–15 s), metallic clang transient, slow beating between
  near-degenerate modes.

## GM 16 — Dulcimer (Hammered Dulcimer)

- **Method:** **Karplus-Strong** (struck string, multiple unison courses) — not
  modal. Reuse `generate_harp_sample`. A hammered dulcimer is a struck *string*,
  so it rings harmonically with a bright metallic attack.
- **Params:** 2–3 detuned unison strings per note (render KS 2–3× with ±2–6 cents
  → shimmer/beating), hard-mallet bright seed, medium decay, no damping
  (strings ring freely → use the long-sustain KS stretch `S ≈ 0.999`). Slight
  comb from strike position.

---

# Family 3 — Organ (GM 17–24)

**Family verdict:** Two sub-mechanisms. The **electric/Hammond organs (17–19)**
are pure **additive synthesis of sine drawbars** (the original instrument *is*
additive synthesis) — cheap and exact with phase-increment wavetable sines, plus
key-click and optional Leslie. The **pipe/free-reed organs (20–24)** are
better as **subtractive** (bandlimited sawtooth/pulse through formant filters)
for church/reed pipes, and **free-reed additive+detune** for accordion/harmonica
(beating reed banks). All are **sustained** voices (no decay) — the cleanest fit
to the engine's existing sustained-oscillator path.

### Core drawbar (Hammond) algorithm — shared GM 17–19

Nine drawbars, fixed harmonic ratios (Electric Druid):

| Drawbar | 16′ | 5⅓′ | 8′ | 4′ | 2⅔′ | 2′ | 1⅗′ | 1⅓′ | 1′ |
|---|---|---|---|---|---|---|---|---|---|
| Ratio ×f0 | **0.5** | **1.5** | **1.0** | **2.0** | **3.0** | **4.0** | **5.0** | **6.0** | **8.0** |

```c
// 9 sine partials at the fixed ratios above, each with a drawbar amplitude
// (registration). All phase-increment wavetable sines — sustained, no decay.
double s = 0.0;
for (int d = 0; d < 9; d++) {
    s += v->drawbar_amp[d] * wt_sin(v->db_phase[d]);
    v->db_phase[d] += v->db_inc[d];               // inc = f0*ratio[d]/sr
    if (v->db_phase[d] >= 1.0) v->db_phase[d] -= 1.0;
}
// key-click: 2–5 ms noise/HF burst at note-on (contact bounce); percussion: see GM18
return s * compute_envelope(v);   // attack/release short; full sustain
```

**Refs:** Hammond drawbar/tonewheel: Electric Druid "Technical aspects of the
Hammond Organ" — https://electricdruid.net/technical-aspects-of-the-hammond-organ/ ;
Hammond organ (Wikipedia) — pseudo-harmonic tonewheel series. Leslie: Doppler
+ tremolo + sideband modulation (rotating horn fast ~6.9 Hz / slow ~0.8 Hz,
bass drum ~5.7 / 0.7 Hz).

## GM 17 — Drawbar Organ

- **Method:** Additive 9 drawbars. Classic full registration (e.g. `88 8000 000`
  or `888 000 000`). Slight tonewheel leakage (faint inharmonic hum) + key-click.
- **Params:** registration table; small per-wheel detune for the tonewheel
  "swirl"; optional slow Leslie.

## GM 18 — Percussive Organ

- **Method:** Drawbar additive + **Hammond percussion**: a single extra harmonic
  (2nd `4′` or 3rd `2⅔′`) added at note-on with a **fast exponential decay**
  (the "ping" of B3 percussion, ~0.2–0.6 s), non-retriggering on legato.
- **Params:** percussion harmonic select (2nd/3rd), fast/slow decay, soft/normal
  volume — the four front-panel B3 percussion switches.

## GM 19 — Rock Organ

- **Method:** Drawbar additive driven into **overdrive** + fast Leslie. The rock
  sound is a Hammond through a cranked Leslie/amp.
- **Params:** full drawbars + `tanh` waveshaping (per-voice `drive_mix` ~0.4–0.7)
  for grit, fast Leslie (horn ~6.9 Hz) with chorus/vibrato. Add upper-drawbar
  emphasis for bite.

### Leslie rotary (shared, optional, for 17–19)

Apply as a **per-voice or bus** effect: amplitude tremolo (LFO on gain) +
Doppler pitch wobble (tiny modulated delay line — the `whistle_jet_buf` or
`wobble_buf` ring is ideal) + a slight comb. Two rotors at different rates (horn
faster than bass drum), with fast/slow (chorale/tremolo) speed switch and
spin-up/spin-down inertia.

## GM 20 — Church Organ (Pipe)

- **Method:** **Additive** of (near-)harmonic pipe ranks, OR subtractive: sum of
  several octave-spaced ranks (8′ + 4′ + 2′ + mixtures) each a soft sine/triangle,
  with a breathy attack chiff. Flue-pipe tone is close to a few harmonics.
- **Params:** many ranks → big, slow attack (chiff = short noise burst), very
  slight detune between ranks for the cathedral shimmer, long reverb-friendly
  sustain, no tremolo. Add 2⅔′/1⅗′ mutation ranks for the bright plenum.

## GM 21 — Reed Organ

- **Method:** **Free-reed additive/subtractive** — buzzy sustained tone. Sum of
  harmonics with a sawtooth-ish spectrum through a gentle lowpass + a hint of
  reed beating (two slightly detuned copies).
- **Params:** static spectrum, soft attack, mild breath noise, no tremolo. Less
  bright than accordion.

## GM 22 — Accordion

- **Method:** **Free-reed**: multiple reed banks slightly detuned (the "musette"
  wet tuning). Subtractive sawtooth/pulse + detuned unison stack.
- **Params:** 2–3 detuned oscillators per note (±10–25 cents — the musette
  shimmer is the identity), bellows-driven gentle amplitude swell, breath noise,
  bright buzzy spectrum (saw → lowpass).

## GM 23 — Harmonica

- **Method:** **Free-reed**, single/dual reed, **breath-driven**. Subtractive
  saw/pulse + strong breath-noise component + amplitude tremolo from breath.
- **Params:** prominent airy breath noise (filtered noise mixed in), expressive
  attack/vibrato, slight pitch bend on attack ("draw" bend), narrow bright
  spectrum, hand-wah lowpass optional.

## GM 24 — Tango Accordion (Bandoneon)

- **Method:** Same free-reed engine as GM 22, **drier/sharper** tuning.
- **Params vs accordion:** less wet detune (tighter musette or dry), more
  reed-buzz HF, sharper bellows attack, the characteristic bandoneon "bite."
  Often dual-reed at the octave.

---

# Family 4 — Guitar (GM 25–32)

**Family verdict:** All eight are **plucked steel/nylon strings** → the
unanimous best method is **Extended Karplus-Strong / digital waveguide**
(Jaffe-Smith), which the engine already implements as `WAVE_HARP`. The whole
family is one shared EKS string model plus per-variant **excitation
(pick/finger), pluck position, damping, and an output nonlinearity** (clean →
muted → overdrive → distortion is a *waveshaping continuum* on the same string).
This is the most reuse-dense family in the dossier: one engine, eight data rows.

### Core guitar string (shared GM 25–32) — extends `generate_harp_sample`

```c
// String = KS delay line in whistle_bore_buf, length = sr/f0 (fractional).
// Per sample (extends the harp loop):
double x = whistle_frac_read(buf, N, w, string_delay);      // delayed sample
//  -- pick-position comb (Jaffe-Smith H_β): subtract a tap at βN --
double picked = x - v->pick_amt * read_tap(buf, w, beta*string_delay);
//  -- loop damping LPF (one-pole; cutoff = brightness; lower => darker) --
double damp = (1.0 - v->loop_b) * picked + v->loop_b * v->harp_lp1;
v->harp_lp1 = damp;
double y = v->stretch * damp;                                // S<1 decay (EKS)
buf[w] = (float)y;  w = (w+1)%N;
//  -- output nonlinearity (clean..distortion continuum) --
double out = (v->drive > 0.0) ? tanh_wt(v->pre*y)*v->post : y;
return out * compute_envelope(v);
```

**Excitation at note-on:** seed the delay line with one wavelength of shaped
noise/impulse. *Pick* (steel/electric) = brighter, sharper seed (less pre-
smoothing). *Finger/nylon* = pre-smoothed, rounder seed (more lowpass passes on
the noise). Pick-direction lowpass `H_p(z)=(1-p)/(1-p z^{-1})`.

**Refs:** Karplus & Strong 1983; Jaffe & Smith 1983 (EKS — pick position,
dynamics, stretch); Smith *PASP* — Karplus-Strong & EKS pages
(https://ccrma.stanford.edu/~jos/pasp/). Distortion/feedback via nonlinear
shaping of KS output: Karplus-Strong (Wikipedia, "electric guitar / feedback"
section) — output through tanh waveshaper modelling an overdriven amp, with a
feedback delay back into the string for sustained-feedback notes; Sullivan, C.
(1990). "Extending the Karplus-Strong Algorithm to Synthesize Electric Guitar
Timbres with Distortion and Feedback," *CMJ* 14(3), 26–37 (the canonical paper
for distortion/feedback guitar — *this is the key citation for GM 28–32*).

## GM 25 — Acoustic Guitar (Nylon)

- **Method:** EKS, **finger** excitation, body resonance.
- **Params:** pre-smoothed soft seed (mellow attack), moderate pluck position
  (`β ≈ 0.13`), darker loop LPF, medium-long decay, **body resonance** (1–3
  biquad modes ~100/200 Hz — reuse `gun_body`) for the wooden box. No drive.

## GM 26 — Acoustic Guitar (Steel)

- **Method:** EKS, **pick** excitation, bright body.
- **Params vs nylon:** brighter seed (sharper pick), brighter loop LPF (more HF
  sustain), pluck nearer bridge (`β ≈ 0.1`), metallic ring, stronger body
  resonance / sympathetic shimmer. Optional pick-scrape noise transient.

## GM 27 — Electric Guitar (Jazz)

- **Method:** EKS, pick, **dark** magnetic-pickup tone, no drive.
- **Params:** warm/dark loop LPF (rolled-off highs — neck humbucker tone),
  medium decay, light compression feel (softer dynamics curve), no body box
  (solid/hollow electric → minimal acoustic resonance). Mellow, round.

## GM 28 — Electric Guitar (Clean)

- **Method:** EKS, pick, brighter than jazz, no drive.
- **Params:** brighter loop LPF than jazz (bridge-pickup sparkle), slightly
  shorter decay, a hint of chorus optional. Glassy Strat-clean.

## GM 29 — Electric Guitar (Muted)

- **Method:** EKS, pick, **palm-mute** = very short decay + lowpass.
- **Params:** the muted variant = **strong loop damping + low stretch
  `S`** (fast decay, T60 ~0.1–0.25 s) + aggressive lowpass → the percussive
  "chunk." Pluck near bridge, short noisy attack. Often plays staccato.

## GM 30 — Overdrive Guitar

- **Method:** EKS string → **soft `tanh` waveshaper** (mild drive) → optional
  feedback tap.
- **Params:** moderate `drive` (pre-gain ~3–5× into `tanh`, post-attenuate),
  brighter sustain, slight feedback delay (a fraction of output back into the
  string buffer → blooming sustain). Compression of dynamics. Per Sullivan 1990.

## GM 31 — Distortion Guitar

- **Method:** EKS string → **hard waveshaper** (heavy clipping) + feedback.
- **Params vs overdrive:** much higher pre-gain into `tanh`/hard-clip (saturated,
  square-ish), pre-emphasis EQ before the shaper (boost mids), strong feedback
  for self-sustaining notes, longer sustain. Power-chord friendly. Sullivan 1990
  is the reference (distortion **and** feedback).

## GM 32 — Guitar Harmonics

- **Method:** EKS string with the excitation **forced to a node** — a pluck-
  position comb at a harmonic node kills the fundamental, leaving a pure high
  partial (natural harmonic). Equivalently, seed the delay line at half/third
  length, or set `β` to a node and boost the surviving partial.
- **Params:** very pure, bell-like high tone (touch a node at 12th/7th/5th fret →
  2nd/3rd/4th harmonic). Implement by driving the string at the harmonic
  frequency with strong loop sustain and a node comb that suppresses the
  fundamental; long ringing, glassy, slight chime. Often with light drive.

---

## Appendix A — Consolidated modal-ratio tables (the data that *is* the timbre)

| GM | Instrument | Mode ratios (×f0) | Notes |
|---|---|---|---|
| 10 | Glockenspiel | 1.0, 2.76, 5.40, 8.90 | free-free steel bar |
| 11 | Music Box | 1.0, 6.27, 17.55 | clamped-free (cantilever) tooth |
| 12 | Vibraphone | 1.0, 4.0, 9.6 | undercut Al, octave-octave tuning |
| 13 | Marimba | 1.0, 4.0, 9.2 | undercut wood, tube resonator |
| 14 | Xylophone | 1.0, 3.0, 6.0 | first overtone = twelfth (3.0) |
| 15 | Tubular Bells | 2.0, 3.0, 4.16, 5.43, 6.79, 8.21 | strike modes 4:5:6≈2:3:4, phantom f0 |
| 9 | Celesta | 1.0, 4.0, 10.8 | soft steel bar (weak uppers) |

## Appendix B — Hammond drawbar ratios (GM 17–19)

`16′=0.5, 5⅓′=1.5, 8′=1.0, 4′=2.0, 2⅔′=3.0, 2′=4.0, 1⅗′=5.0, 1⅓′=6.0, 1′=8.0`

## Appendix C — Method selection summary

| Family | Programs | Primary method | Engine basis |
|---|---|---|---|
| Acoustic piano | 1–4 | Modal additive + inharmonicity (`f_n=n·f0·√(1+Bn²)`) | wavetable partials + per-partial exp decay |
| Electric piano | 5–6 | 2–4 op FM tine/reed + asym. pickup `tanh` | FM operators, `drive_mix` |
| Harpsichord/Clavi | 7–8 | Extended Karplus-Strong (pluck-position comb) | `generate_harp_sample` |
| Chromatic perc. | 9–15 | Modal resonator bank (measured ratios) | `gun_body` biquads → N modes / decaying sines |
| Dulcimer | 16 | Karplus-Strong (struck unison strings) | `generate_harp_sample` |
| Hammond organ | 17–19 | Additive 9-drawbar sines + Leslie + click | wavetable sines + `wobble_buf` Leslie |
| Pipe/reed organ | 20–24 | Additive ranks / subtractive free-reed + detune | sines/saw + biquad formants |
| Guitar (all) | 25–32 | Extended KS string + per-voice waveshaper | `generate_harp_sample` + per-voice `tanh` + feedback |

## Appendix D — Full reference list

- Karplus, K. & Strong, A. (1983). "Digital Synthesis of Plucked-String and Drum
  Timbres," *Computer Music Journal* 7(2), 43–55.
- Jaffe, D.A. & Smith, J.O. (1983). "Extensions of the Karplus-Strong Plucked-
  String Algorithm," *CMJ* 7(2), 56–69.
- Smith, J.O. *Physical Audio Signal Processing* (online, CCRMA). KS:
  https://ccrma.stanford.edu/~jos/pasp/Karplus_Strong_Algorithm.html ; EKS:
  https://ccrma.stanford.edu/~jos/pasp/Extended_Karplus_Strong_Algorithm.html
- Sullivan, C.R. (1990). "Extending the Karplus-Strong Algorithm to Synthesize
  Electric Guitar Timbres with Distortion and Feedback," *CMJ* 14(3), 26–37.
- Chowning, J.M. (1973). "The Synthesis of Complex Audio Spectra by Means of
  Frequency Modulation," *JAES* 21(7), 526–534.
- Pfeifle, F. & Bader, R. (2017). "Real-Time Physical Model of a Wurlitzer and
  Rhodes Electric Piano," *Proc. DAFx-17*, Edinburgh —
  http://www.dafx17.eca.ed.ac.uk/papers/DAFx17_paper_79.pdf
- Fletcher, H. & Rossing, T.D. (1998). *The Physics of Musical Instruments*, 2nd
  ed., Springer (inharmonicity Ch.12; bars/plates/bells Ch.18–21).
- Bank, B. (2000). *Physically-Based Sound Modeling of the Piano*, M.Sc. thesis,
  BME Budapest.
- Bank, B. & Välimäki, V. (2003). "Robust Loss Filter Design for Digital
  Waveguide Synthesis of String Tones," *IEEE Signal Processing Letters* 10(1),
  18–20.
- Smith, J.O. & Van Duyne, S.A. (1995). "Commuted Piano Synthesis," *Proc. ICMC*,
  Banff.
- Cook, P.R. (2002). *Real Sound Synthesis for Interactive Applications*, AK
  Peters (modal synthesis; STK ModalBar / Mandolin / Plucked).
- Välimäki, V., Pakarinen, J., Erkut, C. & Karjalainen, M. (2006). "Discrete-time
  modelling of musical instruments," *Reports on Progress in Physics* 69, 1–78
  (survey covering KS, waveguides, modal, FM).
- Bilbao, S. (2009). *Numerical Sound Synthesis*, Wiley (FD bar/plate models).
- Hammond/Leslie: Electric Druid, "Technical aspects of the Hammond Organ" —
  https://electricdruid.net/technical-aspects-of-the-hammond-organ/ ; Hammond
  organ — Wikipedia (tonewheel pseudo-harmonic series).
- CCRMA Music 150/152 percussion modal notes —
  https://ccrma.stanford.edu/CCRMA/Courses/150/percussion.html

---

*32 / 32 GM programs documented (programs 1–32). Next dossier: 33–64 (Bass,
Strings, Ensemble, Brass).*
