# GM Synthesis Dossier 02 — Bass, Strings, Ensemble, Brass (programs 33–64)

Real-time algorithmic synthesis recipes for 32 General MIDI instruments, targeted
at the Aesthetic Computer native audio engine (`fedac/native/src/audio.c`). No
samples — everything here is a per-voice, per-sample state machine that runs
inside `generate_sample()` at up to 192 kHz across 32 voices.

## How this maps onto the existing AC engine

The engine already ships three physical-model generators that are the direct
templates for everything below. Read them first; the new instruments are
variations on these, not new infrastructure:

| Existing generator | Technique | Reuse target |
| --- | --- | --- |
| `generate_whistle_sample` (audio.c:177) | Cook/STK waveguide flute: bore delay + jet delay + cubic limit-cycle NL + 1-pole loss LPF + DC blocker | **Bowed strings** (swap jet+cubic for a bow-table friction junction) and **brass** (swap cubic for a lip-filter quadratic NL) |
| `generate_harp_sample` (audio.c:300) | Karplus-Strong: noise-seeded delay line + 2-point averaging loss filter + Jaffe-Smith stretch | **All plucked/struck strings**: acoustic/electric/fretless/slap bass, pizzicato strings, harp |
| `generate_gun_sample` / classic layers (audio.c:1216) | Layered transient + parallel body biquads + Friedlander excitation | **Timpani** (modal biquads + noise transient) and **orchestra hit** (cluster + transient) |

Key shared primitives already present and to be reused verbatim:

- `whistle_frac_read(buf, N, w, delay)` — fractional-delay ring read (audio.c:140). The fundamental waveguide/KS read.
- `xorshift32(&v->noise_seed)` — cheap white noise.
- `compute_envelope(v)` — attack / sustain / decay / kill-fade envelope (audio.c:~110).
- The biquad pattern in `WAVE_NOISE` (audio.c:1485) and `setup_noise_filter` — a ready transposed-DF2 biquad with state `noise_x1/x2/noise_y1/y2`.
- Phase-increment oscillators (`v->phase += f/sr`) — the repo standard; **never** `sin(TAU*f*t)`.
- The shared `whistle_bore_buf[2048]` / `whistle_jet_buf[512]` ring buffers — a voice is exactly one wave type at a time, so these are the scratch delay lines for any new waveguide/KS model.

Because the `ACVoice` struct is a tagged union-by-convention (each model squats on
the same buffers), new models cost almost no extra per-voice memory: a handful of
doubles plus a couple of small biquad state arrays. A practical plan is to add a
small number of new `WaveType` enum entries — e.g. `WAVE_BOWED`, `WAVE_BRASS`,
`WAVE_VOICE` (formant), `WAVE_SUPERSAW`, `WAVE_MODAL` — and select the GM-specific
*variant* via a per-voice `int gm_program` (or a `model_variant` byte) read inside
the generator. Variant differences (finger vs pick bass, muted vs open trumpet,
violin vs cello) are almost always just parameter values, not new code paths.

### Performance budget

At 192 kHz × 32 voices the per-sample cost ceiling is ~6.1 M voice-ticks/s. The
existing whistle/harp models already hit this with one fractional read + a 1-pole
filter + a cubic, so the headroom rule is: **one delay-line read, ≤2 biquads, ≤1
transcendental (or a table lookup) per voice per sample.** Formant voices (the
most expensive here) use 3–4 parallel biquads — still fine for the ~8–16 voices a
choir patch realistically needs. Supersaw uses 7 phase-increment saws (trivial).
Where a `sin`/`pow` appears in a hot loop below, prefer a 512/1024-entry LUT
(matching `AUDIO_WAVEFORM_SIZE` conventions) or the polynomial approximations the
gun code already favors.

---

# Family A — Bass (GM 33–40)

All eight basses are **plucked/struck strings**, so the foundation is the existing
Karplus-Strong harp generator (audio.c:300), extended per Jaffe & Smith 1983.
Synth basses (39–40) are the exception: subtractive oscillator + filter.

**Shared KS recipe (extended Karplus-Strong / "EKS"):**

```
delay length  N      = sr / freq                         (one wavelength)
loss filter          y[n] = g·(h0·x[n] + h1·x[n-1])      (2-point averaging, unity-DC)
stretch / decay      g    = S  (S<1 sets T60; high notes need S→1)
pluck excitation     seed N samples of (optionally lowpassed) white noise
pluck position comb  multiply seed spectrum by (1 - z^-βN), β = pluck point (0..0.5)
dynamics→brightness  excitation LPF cutoff ∝ velocity (louder = brighter)
```

The pluck-position comb filter (Jaffe & Smith 1983, §"Pluck Position") is the
single most important timbral control for bass: plucking near the bridge (small
β ≈ 0.1) gives a thin, bright, harmonically-rich tone; plucking over the
fingerboard (β ≈ 0.3–0.4) gives the round, dark fundamental-heavy tone. It is
implemented for free as a 1-zero comb on the seed: `seed[n] -= seed[n - round(βN)]`.

References for the whole family:
- Karplus, K. & Strong, A. (1983). "Digital Synthesis of Plucked-String and Drum Timbres," *Computer Music Journal* 7(2), 43–55.
- Jaffe, D. A. & Smith, J. O. (1983). "Extensions of the Karplus-Strong Plucked-String Algorithm," *CMJ* 7(2), 56–69.
- Smith, J. O. *Physical Audio Signal Processing*, CCRMA — https://ccrma.stanford.edu/~jos/pasp/Karplus_Strong_Algorithm.html
- Karplus–Strong overview — https://en.wikipedia.org/wiki/Karplus%E2%80%93Strong_string_synthesis

### 33 — Acoustic Bass

Method: **Karplus-Strong, dark variant.** Upright/double bass is gut/round-wound on
a large resonant body. Use a low-cutoff excitation (pluck the felt finger, not a
pick) and a body resonator.

- `N = sr/freq`; freq range ~41 Hz (E1) to ~250 Hz. At 192 kHz E1 ⇒ N≈4680 — **exceeds the 2048 bore buffer**; either grow the buffer to 8192 for bass voices or run bass voices at a /4 internal rate (48 kHz) and upsample. Recommend a dedicated 8192-sample buffer for KS bass voices.
- Loss filter: 2-point average `0.5(x[n]+x[n-1])`, stretch `S≈0.996` for a ~1–2 s plucked decay.
- Pluck position β ≈ 0.35 (over fingerboard) → fat fundamental.
- Excitation: white noise through a 1-pole LPF at ~1.5 kHz before seeding → soft thumb attack, very little high-frequency "zing."
- Body resonance: one parallel biquad bandpass at ~90–110 Hz (the big air mode of the corpus), low gain, fed by the string output. Reuse the `gun_body_*` biquad slots.
- Key timbre params: low excitation cutoff, high β, body BP at ~100 Hz, no attack click.

### 34 — Electric Bass (finger)

Method: **KS, round-wound flatwound variant** — brighter than upright, with a
short attack thump but a sustained, slightly metallic ring (magnetic pickup ≈ a
fixed comb + gentle high-mid emphasis).

- Same N/loss as acoustic, but stretch `S≈0.998` (longer sustain, electric strings ring) and pluck position β ≈ 0.25.
- Excitation LPF cutoff ~3 kHz, velocity-scaled (`cutoff = 1500 + 4000·vel`).
- Add a fixed **pickup comb**: tap the delay line at a second point ~10–15% along its length and sum (models the magnetic pickup's position picking up a node-weighted spectrum) — cheap second `whistle_frac_read`.
- Finger attack: a tiny (~3 ms) noise burst LPF'd at ~800 Hz layered at note-on = the fingertip "thp".
- Distinguish from pick (35): softer attack burst, lower excitation cutoff, β larger.

### 35 — Electric Bass (pick)

Method: **KS, bright pick variant.** The plectrum injects a sharper, broadband
transient and excites the string nearer the bridge.

- Pluck position β ≈ 0.12 (near bridge) → bright, hollow, lots of upper harmonics.
- Excitation: *less* lowpassing — cutoff ~6 kHz, plus a 1-sample bipolar "click" prepended to the seed (the pick release transient). Reuse the gun click-layer idea (`gun_click_*`).
- Stretch `S≈0.997`; slightly faster decay than finger.
- Loss filter weighted toward brightness: use `0.6·x[n]+0.4·x[n-1]` (less HF damping than the symmetric average).
- Key timbre params vs finger: small β, sharp transient click, higher excitation cutoff, brighter loss filter.

### 36 — Fretless Bass

Method: **KS with continuous-glide pitch + "mwah" lowpass.** Fretless = the same
string model as finger bass, but (a) pitch can glide (no fret quantization) and
(b) the characteristic resonant "mwah" from the string pressing directly on the
fingerboard wood = a *time-varying* lowpass that opens slightly after attack.

- Base = electric finger (34) with β ≈ 0.3.
- Portamento: smooth `frequency → target_frequency` already exists (audio.c:1514) — slow the slew (use ~0.0008 instead of 0.0003) so legato notes audibly glide.
- "Mwah" filter: a resonant 1-pole/biquad LPF on the *output* whose cutoff envelopes from ~700 Hz up to ~2.5 kHz over ~120 ms at note-on, Q≈3. This is the defining fretless timbre.
- Slightly longer stretch `S≈0.9985` and a touch of slow vibrato (reuse `whistle_vibrato_phase`, ~5 Hz, depth ±0.3%).
- Key timbre params: glide slew, attack-swept resonant LPF, vibrato.

### 37 — Slap Bass 1

Method: **KS + dual excitation (thumb slap + snap).** Slap is two gestures —
*thumb* striking the string against the frets (percussive, broadband, with a
metallic "clack" from string-on-fret) and the string's normal ringing.

- Base KS with β ≈ 0.2, `S≈0.997`.
- Thumb-slap transient: a short (~5 ms) noise burst **bandpassed at ~2–3 kHz** (string slapping the fret) summed at note-on, plus a sub-ms HF click. This is the "clack."
- Fret-buzz: briefly raise the loss-filter gain toward 1.0 for the first ~20 ms so the string rings against the fret with extra high-harmonic energy, then settle. Equivalent to momentarily reducing damping.
- Strong velocity→brightness coupling (slap is dynamically extreme).
- Key timbre params: prominent bandpassed clack, transient under-damping, β small-to-mid.

### 38 — Slap Bass 2

Method: **KS slap, "pop" variant.** Slap 2 in most GM sets is the *popped* (pulled)
string — the finger yanks the string and lets it snap back against the fingerboard.

- Same engine as Slap 1 but: pop transient is **brighter and tighter** — noise burst bandpassed higher (~3–5 kHz), shorter (~3 ms), with a hard click.
- Pluck position β ≈ 0.1 (pulled near bridge) → very bright.
- Faster decay (`S≈0.995`) and a more pronounced fret-slap impact (the snap-back). Add a secondary excitation ~8–12 ms after the first (string returning and hitting the wood) — reuse the gun `gun_secondary_trig` mechanism.
- Key timbre params vs Slap 1: higher/tighter pop band, secondary snap-back impact, smaller β.

### 39 — Synth Bass 1

Method: **Subtractive — single/dual saw or square through a resonant 24 dB/oct
lowpass with an envelope-swept cutoff.** This is the classic Minimoog/TB-303 bass,
not a physical model.

- Oscillator: 1–2 phase-increment sawtooths (reuse `WAVE_SAWTOOTH` math), optionally one detuned −7 cents or one square sub-octave for weight.
- Filter: a resonant lowpass — implement a 2-pole state-variable or ladder-style LPF (cheap: a cascade of two of the existing biquads, or a TPT/Zavalishin 1-pole pair for stability under modulation). Cutoff swept by an envelope: fast attack to ~2–3 kHz, decay to ~300–500 Hz over ~150 ms. Resonance Q≈2–4.
- Slight pitch-envelope (drop ~+2 semitones → 0 over ~10 ms) gives the punchy synth attack.
- Key timbre params: filter envelope amount/time, resonance, sub-oscillator mix.

### 40 — Synth Bass 2

Method: **Subtractive, FM/harder variant.** GM Synth Bass 2 is typically more
aggressive/metallic — either a squarewave-led subtractive patch or a 2-operator
FM bass.

- Option A (subtractive): square + saw, higher resonance (Q≈4–6), more filter-envelope sweep, faster decay — a "rubbery" acid bass.
- Option B (FM, recommended for contrast): 2-op FM — carrier at f0, modulator at 1× or 2× f0, modulation index enveloped (high at attack → low at sustain). Both operators are phase-increment sines (use a sine LUT). `out = sin(2π(φc + I·sin(2πφm)))`. This gives the bright metallic attack mellowing to a hollow body. Cite Chowning 1973 FM.
- Key timbre params vs Synth Bass 1: squarier/FM-brighter spectrum, faster more aggressive envelope.

Reference: Chowning, J. (1973). "The Synthesis of Complex Audio Spectra by Means of
Frequency Modulation," *JAES* 21(7).

---

# Family B — Strings & friends (GM 41–48)

Programs 41–44 (Violin/Viola/Cello/Contrabass) are **solo bowed strings** → bowed
digital waveguide. 45 (Tremolo) is bowed + amplitude LFO. 46 (Pizzicato) and 47
(Harp) are plucked → KS. 48 (Timpani) is modal membrane.

## Bowed digital waveguide (shared by 41–45)

This is the marquee algorithm of the dossier and the one with the most rigorous
literature. The architecture mirrors the existing whistle waveguide: a delay line
loop is the resonator, but the *bow* replaces the *jet*, and a **friction
nonlinearity (the "bow table")** replaces the cubic.

**Physical basis — Helmholtz motion via the friction curve:** McIntyre, Schumacher
& Woodhouse (1983) showed bowed-string oscillation is a *stick-slip* limit cycle
governed by a memoryless friction characteristic relating the bow-string
*differential velocity* (`bow velocity − string velocity at the bow point`) to the
force on the string. Smith (1986) recast this as a digital waveguide: two delay
lines split at the bow point, and a **scattering junction** at the bow whose
reflection/transmission depends on the instantaneous differential velocity through
the friction curve.

**STK `Bowed` per-sample tick (the canonical implementation, Cook/Scavone), exact arithmetic:**

```c
// two delay lines split at the bow point by betaRatio_ (bow position 0..1):
//   bridgeDelay_.setDelay(baseDelay * betaRatio_);
//   neckDelay_.setDelay  (baseDelay * (1 - betaRatio_));
//   baseDelay_ = sr/freq - 4.0;     // -4 corrects for filter group delays

bowVelocity      = maxVelocity_ * adsr_.tick();          // bow speed × envelope
bridgeReflection = -stringFilter_.tick(bridgeDelay_.lastOut());  // bridge: 1-pole loss + sign-invert
nutReflection    = -neckDelay_.lastOut();                // nut: rigid sign-invert
stringVelocity   = bridgeReflection + nutReflection;     // velocity AT the bow
deltaV           = bowVelocity - stringVelocity;         // differential velocity

newVelocity      = deltaV * bowTable_.tick(deltaV);      // FRICTION NONLINEARITY
bridgeDelay_.tick(bridgeReflection + newVelocity);       // inject into both halves
neckDelay_.tick (nutReflection     + newVelocity);

out = 0.1248 * bodyFilterCascade(bridgeDelay_.lastOut()); // 6-biquad body, scaled
```

**The bow table (friction curve), exact STK formula:**

```c
// BowTable::tick(input == deltaV):
sample = (input + offset_) * slope_;      // offset_≈0 default; slope_ ∝ bow force
sample = fabs(sample) + 0.75;
sample = pow(sample, -4.0);               // power-law friction falloff
if (sample < minOutput_) sample = minOutput_;   // typ. 0.01
if (sample > maxOutput_) sample = maxOutput_;    // typ. 0.98
return sample;                            // multiplied by deltaV by caller
```

The `^(-4)` power law is the key: near zero differential velocity (stick) the
multiplier saturates high → string locks to the bow; as |deltaV| grows (slip) the
multiplier collapses → string breaks free. That sharp transition *is* the
Helmholtz stick-slip cycle. `slope_` encodes **bow force/pressure** (steeper =
more pressure = wider stick region) and `betaRatio_` encodes **bow position**
(near bridge = brighter/"sul ponticello," near fingerboard = softer/"sul tasto").

**Porting into AC's whistle generator:**
- Reuse `whistle_bore_buf` as the combined string delay; maintain two read taps (`bridgeDelay` length and `neckDelay` length) instead of jet+bore. Total tap = `sr/freq`.
- Replace the cubic `pd*(pd*pd-1)` with the bow-table `pow(...,-4)` (precompute as a 256-entry LUT keyed on `slope·deltaV` to avoid `pow` in the hot loop).
- `stringFilter_` (bridge loss) = the existing 1-pole loop LPF (`whistle_lp1`), tuned a touch brighter than flute.
- Body filter: 1–3 parallel biquads (not STK's 6) at the violin's main resonances; reuse the `gun_body_*` slots. Output scale ~0.12.
- Vibrato: reuse `whistle_vibrato_phase` modulating `baseDelay`.
- Attack: ramp `maxVelocity_` (bow speed) over ~30–60 ms — the slow bow onset is what makes it read as bowed and not plucked.

References:
- McIntyre, M. E., Schumacher, R. T. & Woodhouse, J. (1983). "On the oscillations of musical instruments," *JASA* 74(5), 1325–1345. https://pubs.aip.org/asa/jasa/article-pdf/74/5/1325/11420986/1325_1_online.pdf
- Smith, J. O. "Digital Waveguide Bowed-String," *Physical Audio Signal Processing*, CCRMA. https://ccrma.stanford.edu/~jos/pasp/Digital_Waveguide_Bowed_String.html
- Smith, J. O. "MUS420 Lecture: Digital Waveguide Modeling of Bowed Strings." https://ccrma.stanford.edu/~jos/BowedStrings/BowedStrings.pdf
- Cook, P. R. & Scavone, G. P. STK `Bowed` / `BowTable` classes. https://ccrma.stanford.edu/software/stk/classstk_1_1Bowed.html  · source: https://github.com/thestk/stk/blob/master/src/Bowed.cpp
- Välimäki, V. et al. (2006). "Discrete-Time Modelling of Musical Instruments," *Reports on Progress in Physics* 69. (Digital waveguide review.)

### 41 — Violin

- Pitch range G3 (196 Hz) – ~E7. `baseDelay = sr/f − 4`; comfortably inside a 2048 buffer above ~94 Hz at 192 kHz.
- `betaRatio_ ≈ 0.13` (bow ~1/7 from bridge — typical violin bow point).
- Bridge loss filter: bright, only gentle HF rolloff (small strings lose little).
- Body resonances: violin "main air" ~280 Hz and "main wood" ~460 Hz → two body biquads. Plus the **bridge-hill** broad peak ~2.5–3 kHz (a wide biquad) for the singing brilliance.
- Vibrato ~5–6 Hz, depth ±0.5–1%.
- Key timbre param vs viola/cello: shortest delay (highest pitch) + highest body resonances.

### 42 — Viola

- Range C3 (131 Hz) – ~A6, a fifth below violin. Slightly **larger body** but acoustically "too small" for its range — the famously nasal, dark-but-thin viola voice.
- Same waveguide, `betaRatio_ ≈ 0.12`.
- Body resonances dropped ~25–30%: air ~220 Hz, wood ~350 Hz. Bridge hill lower (~2 kHz) and **less prominent** (the viola's missing brilliance) → reduce that biquad's gain.
- Slightly stronger bridge loss (a touch darker than violin).
- Key timbre param: lower body modes + weakened bridge hill.

### 43 — Cello

- Range C2 (65 Hz) – ~A5. At 192 kHz C2 ⇒ delay ≈ 2950 > 2048 → **needs the 8192 bass buffer** (share the same enlarged buffer as acoustic bass).
- `betaRatio_ ≈ 0.10` (bow closer to bridge proportionally on the long strings).
- Body resonances: air ~110 Hz, main wood ~180–200 Hz; the big resonant corpus → use 3 body biquads with higher Q for the woody warmth.
- Stronger low-frequency body gain; bridge hill ~1.2 kHz.
- Vibrato slower/wider (~5 Hz, ±0.8%).
- Key timbre param: long delay (8192 buffer), low high-Q body modes, warm bridge.

### 44 — Contrabass

- Range C1/E1 (~33–41 Hz) – ~G4. Definitely the **8192 buffer**, possibly run at a /4 internal rate.
- `betaRatio_ ≈ 0.08`.
- Heaviest bridge loss → dark, fundamental-dominated; very few audible upper partials.
- Body modes ~60 Hz / ~100 Hz, broad. Bow attack noise more prominent (rosin grind audible on big strings) → add a low-level bandpassed noise gated to the bow-onset.
- Key timbre param: lowest delay, darkest loss filter, audible bow-grind transient.

### 45 — Tremolo Strings

Method: **Bowed waveguide (violin-family) + fast amplitude LFO + retrigger bow
direction.** Tremolo = rapid back-and-forth bowing; the spectral content is a
section-string tone whose amplitude pulses ~7–12 Hz with a slightly noisy, gritty
attack on each stroke.

- Base = a *blend* of violin+cello waveguide voices (or a single mid-range bowed voice) but layered/detuned like an ensemble (see Family C) for the section feel.
- Tremolo LFO: amplitude modulation at ~8–10 Hz, depth ~40–60% (not to zero — strokes overlap). Reuse a phase-increment LFO.
- On each LFO trough, briefly re-inject bow-grind noise (bow direction change) → the characteristic "shimmer/grit."
- Key timbre param: LFO rate + depth + per-stroke grit; otherwise inherits the bowed model.

### 46 — Pizzicato Strings

Method: **Karplus-Strong, short bright pluck, section-detuned.** Pizzicato = the
bowed string *plucked* — short decay, bright attack, body resonance.

- KS with `N=sr/f`, β ≈ 0.15 (plucked near the fingerboard end), excitation cutoff ~4 kHz.
- **Short** stretch `S≈0.992` → ~0.3–0.6 s decay (pizz dies fast; the finger damps it).
- Body biquads from the relevant string body (violin-ish, ~280/460 Hz) for the "tock."
- Section feel: run 2–3 slightly detuned/delayed KS voices (±a few cents, ±a few ms onset) so it's a *section* pizz, not a solo. Or apply the ensemble-detune trick to a single voice's excitation.
- Key timbre param: short stretch (fast decay) + body "tock" + section detune.

### 47 — Orchestral Harp

Method: **Karplus-Strong, long sustain** — already implemented as `WAVE_HARP`
(audio.c:300). The existing generator *is* the GM harp.

- Reuse as-is: `S≈0.9985` (T60 ~15 s), 2-point loss filter, noise pluck.
- Refinements for realism: β ≈ 0.2 pluck-position comb; a soundboard body biquad ~150–250 Hz; gentle excitation LPF for the nylon-ish softness already noted in-code.
- Key timbre param: long stretch (let it ring), soft excitation, soundboard resonance.

### 48 — Timpani

Method: **Modal synthesis (parallel resonant biquads) + noise strike transient.**
A struck, tuned membrane: not a string. The kettledrum's *principal* modes form a
near-harmonic series because of air loading.

- **Modal ratios (Rossing):** the preferentially-excited diametric modes (1,1),(2,1),(3,1),(4,1),(5,1) ring at frequency ratios **1 : 1.50 : 1.99 : 2.44 : 2.90** relative to the nominal pitch. (Note the principal three are ≈ 2:3:4, giving timpani their clear pitch.) Map the perceived note to mode (1,1).
- Each mode = one resonant biquad (bandpass) excited by a shared strike impulse; per-mode decay time (lower modes ring longest). Reuse the `gun_body_*` parallel-biquad machinery (extend to ~5 modes).
- Strike transient: a short (~4–8 ms) noise burst (the mallet) LPF'd ~2 kHz, mixed at note-on — the "thwack" before the pitched ring blooms.
- Pitch-bend support: timpani are tuned by pedal; allow `frequency` glide (already supported).
- Key timbre params: modal ratios fixed, per-mode decay, strike-noise amount/hardness.

References:
- Rossing, T. D. (1982). "The physics of kettledrums," and Rossing et al. modal analyses — modes (1,1)…(5,1) in ratios 1, 1.5, 2, 2.44, 2.9. https://pubs.aip.org/asa/jasa/article/66/S1/S18/731734  · https://www.fistecamb.com/JASA117-2.pdf
- Fletcher, H. & Rossing, T. D. (1998). *The Physics of Musical Instruments*, 2nd ed., Springer (Ch. on membranes/timpani).

---

# Family C — Ensemble (GM 49–56)

Two sub-types: **bowed-section emulations** done with supersaw + slow attack
(49–52), and **vocal formant** patches (53–55), plus the **orchestra hit** (56).

## Supersaw (shared by 49–52)

Section strings and synth-strings are most efficiently and convincingly done not as
N physical models but as a **detuned saw ensemble (supersaw)** plus a slow attack
and a chorused stereo image. Adam Szabo's bachelor thesis reverse-engineered the
Roland JP-8000 supersaw and gives exact, ready-to-port formulas.

**7 phase-increment sawtooths**, one center (always in tune) + 6 detuned (3 above,
3 below). Relative detune offsets (center = 1.0), from Szabo Table 1:

```
osc1: 1 − 0.11002313·d      osc5: 1 + 0.01991221·d
osc2: 1 − 0.06288439·d      osc6: 1 + 0.06216538·d
osc3: 1 − 0.01952356·d      osc7: 1 + 0.10745242·d
osc4: 1.0 (center)
```

where `d` is **not** the raw detune knob but the knob passed through an 11th-order
fit (Szabo eq.) so the spread is musical across the range:

```
d = 10028.7312891634·x^11 − 50818.8652045924·x^10 + 111363.4808729368·x^9
  − 138150.6761080548·x^8 + 106649.6679158292·x^7  − 53046.9642751875·x^6
  + 17019.9518580080·x^5  − 3425.0836591318·x^4    + 404.2703938388·x^3
  − 24.1878824391·x^2     + 0.6717417634·x          + 0.0030115596
```

(`x` = detune knob 0..1; precompute `d` once per note, not per sample.)

**Mix law (Szabo §3.2)** — center and side gains vs the mix knob `m` (0..1):

```
centerGain = −0.55366·m + 0.99785
sideGain   = −0.73764·m² + 1.2841·m + 0.044372   // applied to EACH of the 6 sides
```

So at higher "mix" the center drops and the sides swell (parabolically). Finally a
**pitch-tracked highpass** at the fundamental removes the sub-pile-up muddiness
(Szabo: HP at the 1st harmonic, "pitch tracked"). One-pole HP is enough.

Per sample: sum 7 phase-increment saws with these gains, HP, → slow ADSR. ~7
adds + 1 filter per voice — trivially cheap.

References:
- Szabo, A. (2010). "How to Emulate the Super Saw," BSc thesis. https://www.adamszabo.com/internet/adam_szabo_how_to_emulate_the_super_saw.pdf
- "An Analysis of Roland's Super Saw Oscillator…" (A. Shore). https://static1.squarespace.com/static/519a384ee4b0079d49c8a1f2/t/592c9030a5790abc03d9df21/1496092742864/An+Analysis+of+Roland's+Super+Saw+Oscillator+and+its+Relation+to+Pads+within+Trance+Music+-+Research+Project+-+A.+Shore.pdf
- Roland JP-8000 — https://en.wikipedia.org/wiki/Roland_JP-8000

### 49 — String Ensemble 1

Method: **Supersaw + slow attack + ensemble chorus + lowpass.** The lush
"orchestra strings" pad.

- 7-saw supersaw, moderate detune (`x≈0.25–0.35`), high mix (`m≈0.7`).
- Slow attack ~120–250 ms, slow release ~300–500 ms (bowed onset).
- Gentle lowpass ~4–6 kHz (sawtooth is too buzzy raw; real strings roll off) + a slight ~3 kHz "bridge hill" bump for sheen.
- Stereo chorus: a slow ~0.3–0.6 Hz LFO modulating a short delay (reuse the engine's `wobble` infrastructure) for the moving-section shimmer.
- Add a slow ~5 Hz collective vibrato at low depth.
- Key timbre param vs Ensemble 2: warmer, lower lowpass, slightly less detune.

### 50 — String Ensemble 2

Method: **Supersaw, brighter/wider variant** (often the "slow strings" or a wider
analog-string sound).

- More detune (`x≈0.4–0.5`) and higher mix → fatter, wider.
- Slightly slower attack still (the GM "slow strings" reading) ~200–350 ms.
- Higher lowpass (~7 kHz) for more air, more chorus depth.
- Key timbre param vs Ensemble 1: wider detune, brighter, slower swell.

### 51 — SynthStrings 1

Method: **Supersaw, overtly synthetic.** No attempt at acoustic body — the analog
poly-synth string machine (Solina/Juno) sound.

- Supersaw with `x≈0.3`, mix `m≈0.6`; **no body resonance**, just a resonant lowpass with a slow filter-envelope sweep (cutoff rises over the attack).
- Add the classic ensemble BBD chorus (multi-tap modulated delay) heavily — this is the signature of string machines.
- Faster attack than acoustic ensembles (~60–120 ms).
- Key timbre param: filter-envelope sweep + heavy chorus, no body modes.

### 52 — SynthStrings 2

Method: **Supersaw, brighter/more-resonant synth variant.**

- Higher resonance on the LPF (Q≈3), more detune, a touch faster attack.
- Optional PWM square layer mixed in for a hollower, reedier synth-string color.
- Key timbre param vs SynthStrings 1: more filter resonance, square layer.

## Vocal formant synthesis (shared by 53–55)

Choir/voice patches are **source–filter formant synthesis**: a glottal/buzz source
(rich in harmonics) shaped by 3–5 resonant **formant** bandpass filters at vowel-
specific frequencies. Two implementation schools:

1. **Parallel formant filters (Klatt-style, recommended for AC):** drive a single
   buzz source (saw or impulse train at f0, optionally with a glottal pulse shape)
   into 3–4 parallel resonant biquads tuned to F1–F4, summed with the table's
   per-formant gains. Cheap (3–4 biquads), maps cleanly onto the engine's biquad
   machinery, and the formant frequencies are *absolute* (independent of pitch) —
   exactly what makes a vowel a vowel.
2. **FOF / CHANT (Rodet):** generate each formant as a damped sinusoidal grain
   ("forme d'onde formantique") fired once per glottal period. Higher quality for
   solo voice but more state; overkill for a choir pad.

**Use real formant tables.** Csound's FOF-synthesis appendix tabulates F1–F5,
gains (dB) and bandwidths for soprano/alto/tenor/bass × a,e,i,o,u. The two GM
vowels we need are **/a/ ("Aah")** and **/u/ → /o/ ("Ooh")**. Bass-voice values
(good for a mixed-choir center; transpose up for higher sections):

| Voice | Vowel | F1 | F2 | F3 | F4 | F5 | gains dB (F1..F5) | BW (F1..F5) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Bass | a | 600 | 1040 | 2250 | 2450 | 2750 | 0,−7,−9,−9,−20 | 60,70,110,120,130 |
| Bass | o | 400 | 750 | 2400 | 2600 | 2900 | 0,−11,−21,−20,−40 | 40,80,100,120,120 |
| Bass | u | 350 | 600 | 2400 | 2675 | 2950 | 0,−20,−32,−28,−36 | 40,80,100,120,120 |
| Tenor | a | 650 | 1080 | 2650 | 2900 | 3250 | 0,−6,−7,−8,−22 | 80,90,120,130,140 |
| Tenor | o | 400 | 800 | 2600 | 2800 | 3000 | 0,−10,−12,−12,−26 | 40,80,100,120,135 |
| Alto | a | 800 | 1150 | 2800 | 3500 | 4950 | 0,−4,−20,−36,−60 | 50,60,170,180,200 |
| Alto | o | 450 | 800 | 2830 | 3500 | 4950 | 0,−9,−16,−28,−55 | 70,80,100,130,135 |
| Soprano | a | 800 | 1150 | 2900 | 3900 | 4950 | 0,−6,−32,−20,−50 | 80,90,120,130,140 |
| Soprano | u | 325 | 700 | 2700 | 3800 | 4950 | 0,−16,−35,−40,−60 | 50,60,170,180,200 |

(Full Csound table covers e,i and counter-tenor too; gain in dB → linear =
`10^(dB/20)`; biquad bandpass Q ≈ `Fn / BWn`.) The **singer's formant** — a strong
cluster ~2.8–3.2 kHz that lets voices cut over an orchestra — is visible as the
elevated F3/F4 in tenor/bass and can be boosted slightly for a "trained choir"
sheen.

**Choir-ness (the multi-voice shimmer):** a real choir = many slightly detuned,
slightly out-of-phase, slightly differently-vibratoed voices. Get it cheaply with:
- 3 detuned source oscillators per voice (±5–12 cents) feeding the *same* formant bank, OR
- per-voice random f0 jitter + independent vibrato phase + a chorus on the output.
- Random slow vibrato (~5–6 Hz, ±1–2%, decorrelated phase per layer) is essential — a static formant source sounds like an organ, not a choir.

References:
- Klatt, D. H. (1980). "Software for a cascade/parallel formant synthesizer," *JASA* 67(3), 971–995. (Canonical formant-synth architecture, formant freq/BW control.)
- Rodet, X., Potard, Y. & Barrière, J.-B. (1984). "The CHANT Project: From the Synthesis of the Singing Voice to Synthesis in General," *Computer Music Journal* 8(3). FOF/CHANT. http://anasynth.ircam.fr/home/english/media/singing-synthesis-chant-program
- Csound FOF formant tables (soprano/alto/tenor/bass × a,e,i,o,u; freq/dB/BW). https://www.csound-tutorial.net/floss_manual/Release06/Cs_FM_06_ScrapBook/default_026.html
- Praat KlattGrid vowel defaults (e.g. /a/: F1 800/F2 1200/F3 2300/F4 2800, BW 80/80/100/140). https://www.fon.hum.uva.nl/praat/manual/Create_KlattGrid_from_vowel___.html
- Peterson, G. E. & Barney, H. L. (1952). "Control Methods Used in a Study of the Vowels," *JASA* 24(2). (Reference vowel formant data.)

### 53 — Choir Aahs

Method: **Formant synthesis, vowel /a/, multi-voice.** The bright, open choir
"aah."

- Source: bandlimited saw or glottal pulse at f0; 3 detuned layers (±7 cents).
- Formant bank: F1–F4 from the **/a/** rows above (pick voice section by pitch: bass below C3, tenor/alto mid, soprano above C5 — or blend two adjacent rows). F1≈600–800, F2≈1040–1150 — the wide-open /a/.
- Slow attack ~80–150 ms, slow release; collective decorrelated vibrato ~5.5 Hz.
- Light breath noise into the formants for the airy choral texture.
- Key timbre param: /a/ formants, pitch-dependent voice section, choir detune+vibrato.

### 54 — Voice Oohs

Method: **Formant synthesis, vowel /u/–/o/, multi-voice.** The rounded, dark
"ooh."

- Identical engine to Choir Aahs but formant bank from the **/u/** (or /o/) rows:
  F1≈325–400 (low), F2≈600–800 (low), high formants weak (−20 to −35 dB). The low,
  close F1/F2 is what makes it "ooh."
- Slightly darker/softer; less breath noise; rounder source (more lowpassed).
- Key timbre param: /u/ formants (low close F1/F2), darker source.

### 55 — Synth Voice

Method: **Formant synthesis, synthetic/"vocoder" voice** — a single (non-choir)
formant voice, often static vowel between /a/ and a neutral schwa, with a clearly
synthetic edge.

- One source (saw or pulse), one formant bank — no choir detune (or minimal), so it
  reads as a solo synthetic voice.
- A neutral/blended vowel (e.g. between /a/ and /o/: F1≈500, F2≈900, F3≈2500) or
  let it morph slightly with a slow LFO between two vowel tables for the "talking"
  shimmer.
- Brighter source, optional ring-mod or slight chorus for the synthetic sheen.
- Key timbre param: single voice (no choir), neutral/morphing vowel, synthetic source.

### 56 — Orchestra Hit

Method: **Cluster of pitched partials + broadband transient + fast decay** — the
iconic "orch hit" stab (originally a Fairlight/sampled chord-stab, reproducible
algorithmically as a dense detuned cluster).

- A dense **chord cluster**: stack ~6–10 sawtooth/pulse oscillators across a wide
  pitched cluster (root + octave + fifth + a spread of detuned partials) for the
  "everyone plays at once" mass.
- **Transient:** a short broadband noise burst + brass-like fast attack at t=0 (the
  ensemble impact). Reuse the gun crack/click layers for the front-end snap.
- **Envelope:** near-instant attack, fast decay (~250–500 ms), no sustain — it's a
  stab. Add a quick downward pitch-blip (+~1 semitone → 0 over ~15 ms) for the
  "punch."
- Optional formant-ish broad bandpass ~1–2 kHz for the choral/brass blend the
  original sample has.
- Key timbre param: cluster width/density, transient brightness, short percussive
  envelope.

---

# Family D — Brass (GM 57–64)

Programs 57–62 are **lip-reed brass** → brass digital waveguide. 63–64
(SynthBrass) are subtractive/analog-brass patches with the classic filter-swell
"brass envelope."

## Brass digital waveguide (shared by 57–62)

The brass model is the whistle waveguide's sibling: a bore delay-line resonator
driven by a **pressure-controlled valve** — but where the flute used a jet+cubic,
brass uses a **lip-reed**: a tuned resonant filter (the lip's mechanical
resonance) plus a quadratic pressure nonlinearity. The lip resonance must track
the played note (the player "buzzes" at pitch), which is what makes brass models
playable and gives them their characteristic attack and overblow behavior.

**STK `Brass` per-sample tick (Cook "TBone"/HosePlayer lineage), exact arithmetic:**

```c
breathPressure = maxPressure_ * adsr_.tick();              // breath envelope
breathPressure += vibratoGain_ * vibrato_.tick();          // add vibrato

mouthPressure  = 0.3  * breathPressure;                    // pressure at the lips
borePressure   = 0.85 * delayLine_.lastOut();              // returning bore wave
deltaPressure  = mouthPressure - borePressure;             // pressure across lips

deltaPressure  = lipFilter_.tick(deltaPressure);           // LIP RESONANCE (biquad @ f0)
deltaPressure *= deltaPressure;                            // quadratic NL (pressure→area)
if (deltaPressure > 1.0) deltaPressure = 1.0;              // valve can only open so far

// reed/valve scatter: blend mouth & bore by the (squared) lip opening
out = deltaPressure * mouthPressure + (1.0 - deltaPressure) * borePressure;
out = delayLine_.tick( dcBlock_.tick(out) );               // DC-block, into bore
```

with setup:
```c
delayLine_.setDelay( sr/frequency * 2.0 + 3.0 );  // half-wave bore (closed-open ⇒ ×2)
lipFilter_.setResonance( frequency, 0.997 );      // lip biquad tracks the note, near-unit pole
lipFilter_.setGain( 0.03 );
dcBlock_.setBlockZero();
```

**Why it works:** the lip filter is a high-Q resonator centered on the played pitch
— it preferentially feeds energy back at f0, so the bore locks to that harmonic of
its resonance (brass players "lip" to a partial). The **squared** nonlinearity is
the valve: it's a one-sided pressure-to-flow law (the lips can open but the
quadratic + clip prevents negative opening), which is exactly what sustains the
buzz and generates the bright, harmonic-rich brass spectrum that *brightens with
breath pressure* (more `maxPressure_` → the NL saturates harder → more harmonics =
crescendo gets brassier). That breath→brightness coupling is the signature of real
brass and falls out of the model for free.

**Porting into AC's whistle generator:**
- Reuse `whistle_bore_buf` as the bore (delay = `sr/f·2+3`, half-wave). Single delay line (no jet).
- Replace the cubic with: `lipFilter` (a biquad tracking f0, near-unit pole 0.997) then `x*x` then `min(1)`.
- The lip biquad = one of the existing biquad slots, `setResonance` recomputed on note + on pitch glide.
- `dcBlock_` = the existing `whistle_hp` 1-pole DC blocker.
- `maxPressure_` = breath envelope target (drives loudness *and* brightness — don't normalize it away).
- Vibrato: reuse `whistle_vibrato_phase`.
- Mutes / variants are mostly a lowpass + pressure/lip-gain change (below).

References:
- Cook, P. R. (1991). "TBone: An Interactive WaveGuide Brass Instrument Synthesis Workbench for the NeXT Machine," *Proc. ICMC*. https://www.semanticscholar.org/paper/3fe3399da2ef21815debf47dcf2e3e4e81f23d1b
- Cook, P. R. & Scavone, G. P. STK `Brass` class. https://ccrma.stanford.edu/software/stk/classstk_1_1Brass.html · source: https://github.com/thestk/stk/blob/master/src/Brass.cpp
- Smith, J. O. *Physical Audio Signal Processing* (waveguide wind instruments), CCRMA. https://ccrma.stanford.edu/~jos/pasp/
- Vergez, C. & Rodet, X. (work on trumpet physical models / lip nonlinearity, IRCAM) — companion to the lip-reed approach.

### 57 — Trumpet

- Highest, brightest open brass. Pitch ~E3–C6. `delay = sr/f·2+3`.
- High `maxPressure_` → bright, harmonically rich; lip pole 0.997.
- Minimal bore loss (small bright instrument) → keep the loop bright.
- Bell radiation: a gentle high-shelf/zero on the output (brass radiates highs more efficiently) → reuse a 1-zero HPF like the gun `gun_radiation_a`.
- Attack: a short noise "spit" + fast pressure ramp (~20–40 ms).
- Key timbre param vs others: shortest bore, brightest (high pressure + low loss), bell HPF.

### 58 — Trombone

- Lower, big-bore brass; pitch ~E2–F4. Longer `delay`; possibly the 8192 buffer for the lowest notes.
- Slightly **darker** than trumpet (bigger bore = more HF loss) → stronger loop LPF.
- Slide → supports continuous pitch *glissando*: slow the frequency slew so legato slides glide (like fretless).
- Slightly slower attack (larger air column).
- Key timbre param: long bore, darker loss, glissando-capable glide.

### 59 — Tuba

- Lowest brass; pitch ~D1–F3. **8192 buffer / possibly /4 internal rate.**
- Heavy bore loss → very dark, fundamental-dominated, few upper partials.
- Lower lip pole / lower `maxPressure_` brightness coupling (tuba rarely sounds "brassy/bright"); broad, round tone.
- Slow attack (~50–80 ms — moving a lot of air).
- Key timbre param: longest bore, darkest loss, soft round attack.

### 60 — Muted Trumpet

Method: **Trumpet waveguide + mute filter.** A straight/cup mute = a resonant
notch+lowpass on the bell radiation plus a nasal mid-peak.

- Base = trumpet (57) but: insert a **mute filter** on the output — a lowpass (~3–4 kHz) plus a resonant bandpass peak ~1.5–2 kHz (the mute's nasal "pinch"), and reduce overall level/brightness.
- Reduce bell-radiation HPF (mute kills the highs).
- Slightly increase lip damping (mute loads the bore).
- Key timbre param vs open trumpet: mute LPF + nasal mid-peak, reduced brightness — this is the *only* difference, so a single `muted` flag toggling the output filter covers it.

### 61 — French Horn

Method: **Brass waveguide, dark/mellow, conical, hand-in-bell.** The horn is long,
conical, and played with a hand in the bell → famously round, dark, blendy.

- Long bore; mellow lip pole (slightly lower Q, e.g. 0.995) → rounder, less edgy than trumpet.
- Stronger loop LPF (conical + hand-in-bell rolls off highs) → warm.
- **Soft attack** (~40–70 ms, little spit) — horn entrances are smooth.
- Vibrato minimal (orchestral horn vibrato is subtle).
- Key timbre param: dark loss + mellow lip + soft attack; the "noble warm" brass.

### 62 — Brass Section

Method: **Stack of 3–4 detuned brass-waveguide voices** (or a brass waveguide
layered like the supersaw ensemble) — a *section* of trumpets/trombones/horns
hitting together.

- Run 3 brass voices at the played pitch, detuned ±5–12 cents and onset-staggered ±10–25 ms (sections never attack perfectly together) → the fat unison "stab/swell."
- Shared brighter pressure (sections play loud), light chorus on the output.
- If voice budget is tight: one brass waveguide + the ensemble detune/chorus trick on its output (cheaper, still convincing).
- Key timbre param: detune spread + onset stagger + collective brightness.

### 63 — SynthBrass 1

Method: **Subtractive analog brass** — sawtooth(s) → resonant lowpass with a
fast filter-envelope swell (the classic "brass" patch: cutoff snaps up on attack
then settles), plus PWM and slight oscillator detune.

- 2 detuned saws (±5–10 cents) + optional PWM square → resonant LPF.
- **Filter envelope (the brass signature):** cutoff fast-attacks to ~5–6 kHz then decays to ~1.5–2 kHz over ~80–150 ms; resonance Q≈2. This filter swell *is* the synth-brass.
- Slight pitch overshoot on attack; light vibrato/PWM motion on sustain.
- Key timbre param: filter-envelope amount/speed, detune, PWM.

### 64 — SynthBrass 2

Method: **Subtractive synth brass, brighter/harder or FM variant** (often the more
aggressive, faster, "analog stab" brass).

- More resonance, faster/snappier filter envelope, brighter base spectrum; or a
  2-op FM brass (carrier f0, modulator ~1×, index enveloped) for a metallic edge.
- Tighter, more percussive attack than SynthBrass 1.
- Key timbre param vs SynthBrass 1: faster/harder filter env, higher resonance / FM edge.

---

## Implementation roadmap (mapping to audio.c)

1. **Add WaveTypes** (audio.h enum): `WAVE_BOWED`, `WAVE_BRASS`, `WAVE_VOICE`, `WAVE_SUPERSAW`, `WAVE_MODAL`, plus a `WAVE_KS_BASS` (or reuse `WAVE_HARP` with a `model_variant`). Add a per-voice `uint8_t gm_program` / `model_variant` to select variant params.
2. **Bowed** (41–45): fork `generate_whistle_sample` → `generate_bowed_sample`. Two delay taps off `whistle_bore_buf` (bridge+neck split by `betaRatio`), bow-table LUT (256-entry `pow(x,-4)`), 1-pole bridge loss, 1–3 body biquads (reuse `gun_body_*`). Params per instrument from the violin/viola/cello/contrabass tables above.
3. **Brass** (57–62): fork the whistle → `generate_brass_sample`. Single bore delay (`sr/f·2+3`), lip biquad (one biquad slot, `setResonance(f0, 0.997)`), `x*x` + clip NL, existing DC blocker. Mute (60)/section (62) are output-filter / layering options.
4. **KS bass + pizz + harp** (33–38, 46–47): extend `generate_harp_sample` with: pluck-position comb on the seed, velocity→excitation-cutoff, body biquad, and per-program `(β, S, excitation-cutoff, transient)` params. **Grow the delay buffer to 8192** (or add a low-rate path) for bass/cello/contrabass/tuba.
5. **Supersaw** (49–52): new `generate_supersaw_sample` — 7 phase-increment saws with the Szabo detune offsets + mix law + pitch-tracked HP, then slow ADSR + output chorus (reuse `wobble`).
6. **Formant voice** (53–55): new `generate_voice_sample` — buzz source (saw/glottal) → 3–4 parallel formant biquads from the vowel table (select section by pitch), decorrelated vibrato + chorus for choir-ness.
7. **Modal** (48 timpani, 56 orch-hit): extend the `gun_body_*` parallel-biquad machinery to ~5 modes with the Rossing ratios + a noise/transient strike; orch-hit = detuned cluster + transient + fast decay.
8. **Synth bass/brass** (39–40, 63–64): a small subtractive core (saw/square/FM osc → resonant LPF with a filter envelope) — the only genuinely new primitive needed is a **modulatable resonant lowpass** (a TPT/SVF or cascaded biquads), worth adding once and sharing.

The only new shared infrastructure: a larger (8192) delay buffer for sub-94 Hz
voices, a 256-entry bow-table LUT, a sine LUT for FM, and a modulatable resonant
SVF/ladder lowpass. Everything else reuses existing whistle/harp/gun primitives.
