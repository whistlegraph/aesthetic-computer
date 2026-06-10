# GM Synthesis Dossier — Part 04

## Synth Effects · Ethnic · Percussive · Sound Effects (GM programs 97–128)

Real-time, algorithmic synthesis methods for the final 32 General MIDI
programs, written for implementation in C inside the Aesthetic Computer
native audio engine (`fedac/native/src/audio.c`). Every recipe is sized
for the existing per-sample voice loop: a `switch` on `WaveType`, one
`ACVoice` worth of state, 32 voices, sample rates up to 192 kHz.

This part deliberately reuses the engine's existing physical models:

| Existing model (`audio.c`)            | Reused for |
|---------------------------------------|------------|
| `WAVE_HARP` — Karplus-Strong loop (`generate_harp_sample`) | sitar, banjo, shamisen, koto, fret noise |
| `WAVE_GUN` — DWG bore + body modes + Friedlander blast (`generate_gun_*`) | gunshot, taiko/drum transients |
| `WAVE_WHISTLE` — Cook flute waveguide (`generate_whistle_sample`) | bagpipe, shanai reeds, bird tweet |
| `WAVE_NOISE` — biquad-filtered xorshift noise | seashore, applause, breath, helicopter, reverse cymbal |
| `WAVE_PIANO` — modal additive + inharmonic partials | template for *all* modal percussion (steel drum, bell, agogo, woodblock, tom, kalimba) |

Two new reusable primitives are proposed and used throughout:

1. **`modal_bank`** — a parallel bank of N two-pole resonators
   (biquad band-pass), each tuned to a measured modal frequency with its
   own T60 decay. This is the workhorse for *all* pitched/struck metal &
   wood percussion and for the kalimba tine. It generalizes the piano's
   additive partial loop into a struck-resonator loop.
2. **`phisem`** — Perry Cook's *Physically Informed Stochastic Event
   Modeling*: a stochastic shake-energy variable that fires random
   collision impulses into one or more resonators. This is the workhorse
   for the *particle* sounds: rain, applause, seashore, and the shaker
   family.

---

### Shared primitive A — Modal resonator bank

A single mode is a two-pole resonator (a biquad with the band-pass
numerator). Given a mode frequency `f_k`, a pole radius `R_k` (decay),
and the sample rate `sr`:

```
ω_k = 2π·f_k / sr
R_k  = exp(−π / (T60_k · sr) · 6.9078 / 6.9078)   // see below
a1   = −2·R_k·cos(ω_k)
a2   = R_k·R_k
// band-pass numerator (resonator, unity-ish peak gain):
b0   = (1 − R_k)              // simple normalized 2-pole resonator
y[n] = b0·x[n] − a1·y[n−1] − a2·y[n−2]
```

**T60 → pole radius.** A mode that loses 60 dB over `T60` seconds has a
per-sample amplitude multiplier `R = 10^(−3/(T60·sr)) = exp(−6.9078/(T60·sr))`.
Equivalently, using the loss factor formulation from Nathan Ho's modal
synthesis notes, `R_k = 1/τ_k` with `τ ≈ T60/4.605` (τ is the 1/e time).
Frequency-dependent damping uses `R_k = b1 + b3·f_k²` so high modes die
faster — exactly the perceptual "bright attack mellowing to a hum" the
piano model already exploits (Weinreich's `damping ∝ ω²`).

The struck excitation is a 1–3 sample impulse (or a short ~2 ms
raised-cosine burst) injected as `x[n]` into every resonator in parallel;
the modes ring on their own afterward. This is cheaper than additive
sine partials because each mode is 1 multiply-add chain instead of a
`sin()` per sample, and it self-decays without an envelope multiply.

> **Sources:** Adrien, J.-M. (1991) "The Missing Link: Modal Synthesis,"
> in *Representations of Musical Signals*, MIT Press, pp. 269–298.
> Smith, J.O., *Physical Audio Signal Processing* (CCRMA, online),
> "Modal Synthesis" — https://ccrma.stanford.edu/~jos/pasp/ .
> Nathan Ho, "Exploring Modal Synthesis" —
> https://nathan.ho.name/posts/exploring-modal-synthesis/ (loss-factor
> `R_k = b1 + b3·f_k²`; free-free & cantilever beam ratios used below).

### Shared primitive B — PhISEM particle engine

Cook's PhISEM models a system of many colliding particles (beans, beads,
coins, raindrops, clapping hands) by a single energy variable that leaks
away and stochastically fires collision events into resonator(s).

The canonical STK `Shakers` per-sample tick:

```
// shakeEnergy is pumped on noteOn / shake; it leaks each sample:
shakeEnergy *= systemDecay;              // e.g. 0.999 (maraca)
// probability that one of N particles collides this sample:
if (random_uniform() < (numObjects * shakeEnergy)) {
    sndLevel += shakeEnergy;             // inject a collision impulse
}
sndLevel *= soundDecay;                  // e.g. 0.95 — per-collision ring decay
input = sndLevel * white_noise();        // each collision = a noise grain
// excite a 2-pole resonator at the system resonance:
output = input - a1*y1 - a2*y2;          // a1=-2R cosω, a2=R²
y2 = y1; y1 = output;
```

**Measured STK constants** (Cook / STK `Shakers.cpp`):

| Instrument  | numObjects | systemDecay | soundDecay | resFreq(s) Hz | pole R | gain |
|-------------|-----------:|------------:|-----------:|---------------|-------:|-----:|
| Maraca      | 25         | 0.999       | 0.95       | 3200          | 0.96   | 4.0  |
| Sekere      | 64         | 0.999       | 0.96       | 5500          | 0.60   | 4.0  |
| Cabasa      | 512        | 0.997       | 0.96       | 3000          | 0.70   | 8.0  |
| Tambourine  | 32         | 0.9985      | 0.95       | 2300/5600/8100| .96/.99/.99 | .1/.8/1 |
| Sleighbells | 32         | 0.9994      | 0.97       | 2500/5300/6500/8300/9800 | 0.99 | 1/1/1/.5/.3 |
| Guiro       | 128        | —           | 0.95       | 2500/4000     | 0.97   | 1/1  |

The same engine, with `numObjects` raised into the hundreds–thousands
and per-grain resonance broadened, becomes rain, applause, frying bacon,
a waterfall, and crowd noise — Cook explicitly reports all of these
emerge from the one rain model by sliding parameters.

> **Sources:** Cook, P.R. (1997) "Physically Informed Sonic Modeling
> (PhISM): Synthesis of Percussive Sounds," *Computer Music Journal*
> 21(3), pp. 38–49. Cook, P.R. (2002) *Real Sound Synthesis for
> Interactive Applications*, A K Peters — ch. on PhISEM/particle models
> (rain, applause, maracas, windchimes, coins, gravel, ice).
> STK `Shakers` class — https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html .
> McGill MUMT-618 PhISEM notes — https://www.music.mcgill.ca/~gary/618/week12/phism.html .

---

# Family 1 — Synth Effects (GM 97–104)

These are *sound-design recipes*, not single physical instruments. The
GM/Roland Sound Canvas FX patches are layered: a tonal core (FM or
detuned saws), a noise/texture bed, and a time-domain effect
(delay/echo, chorus, ring mod). The engine already has a global
reverb/room, glitch, drive, and wobble/flange; these patches mostly need
a per-voice **delay line**, an **LFO**, and a **ring-mod** multiply,
which are cheap additions to `ACVoice`.

> **General source for this family:** Reid, G., "Synth Secrets" (Sound On
> Sound, 63-part series, 1999–2004), esp. *Synthesizing Bells* (FM
> enharmonic partials) — https://www.soundonsound.com/techniques/synthesizing-bells —
> and the additive/pad/ring-mod installments:
> https://www.soundonsound.com/series/synth-secrets-sound-sound .

## 97 — FX 1 (rain)
- **Method:** PhISEM with very high `numObjects` (≈800–2000 droplets),
  short `soundDecay` (≈0.92), and a wide band-pass resonance (low R ≈
  0.6) around 1.5–4 kHz so each drop is a soft tick rather than a pitch.
  Layer a slowly-detuned shimmer (two sines a few cents apart, ring-mod'd)
  for the "musical FX-1" Roland flavor that distinguishes the GM patch
  from a pure rainfall SFX. "Heavier rain" = raise `shakeEnergy` floor
  and `numObjects`.
- **C state:** `phisem{energy, sysDecay=0.9995, sndDecay=0.92, num, res biquad}`
  + a 2-osc shimmer pair.
- **Key params:** droplet density (energy × num), resonance brightness,
  shimmer detune.

## 98 — FX 2 (soundtrack)
- **Method:** Detuned sawtooth pad (2–3 saws, ±5–12 cents) through a slow
  LFO-swept low-pass, plus a slow stereo chorus. This is the classic
  "sweeping ensemble" pad. Use 3 phase-increment saws summed, a 1-pole
  LP whose cutoff is modulated by a ~0.1–0.3 Hz triangle LFO.
- **C state:** 3 saw phases + LFO phase + LP state.
- **Key params:** detune spread, LFO rate/depth, cutoff range.

## 99 — FX 3 (crystal)
- **Method:** Bell-like FM (a la Synth Secrets *Synthesizing Bells*): one
  modulator FM-ing a carrier at a non-integer ratio (e.g. 1:1.41 or
  1:3.5) to make inharmonic metallic partials, with a fast-decaying
  envelope and a bright, glassy attack. Add a short feedback delay
  (~80–150 ms) for the twinkling repeats. Two FM pairs detuned makes the
  "dense enharmonic fog" Reid describes.
- **C state:** carrier phase + modulator phase + mod index env + delay ring.
- **Key params:** C:M ratio (inharmonicity), mod index decay, delay time/feedback.

## 100 — FX 4 (atmosphere)
- **Method:** Soft, breathy pad: filtered noise bed (LP noise, slow
  amplitude LFO) + a low detuned sine/triangle drone + slow chorus. Think
  "guitar-harp-into-pad" Roland patch — a plucked KS attack (reuse
  `WAVE_HARP`) crossfading into a sustained noise-pad tail.
- **C state:** KS loop (existing harp) + noise biquad + drone osc + xfade env.
- **Key params:** noise/tone balance, attack pluck brightness, drone pitch.

## 101 — FX 5 (brightness)
- **Method:** Bright additive/FM pad with a strong high-harmonic content
  and a slow filter-open sweep. Stacked saw + a high-ratio FM partial
  (C:M ≈ 1:7) with the modulator on an attack-rising envelope so the
  timbre brightens *into* the note. PWM (Reid's pad trick) adds motion.
- **C state:** saw phase + FM pair + rising mod-index env + LFO for PWM.
- **Key params:** brightness (top harmonic level), sweep rate, PWM depth.

## 102 — FX 6 (goblins)
- **Method:** Dark, vocal, evolving pad with ring modulation and slow
  random/sample-hold modulation of pitch and filter — the "ominous voice"
  patch. Core is a low detuned-saw pad ring-modulated by a low (40–120 Hz)
  sine to fracture it into a growl, plus a slow random LFO (sample-and-hold)
  bending the cutoff. Formant-ish band-passes give the "ah/oh" vowel.
- **C state:** saw pair + ringmod sine + S&H LFO + 2 formant biquads.
- **Key params:** ringmod freq, S&H rate, formant centers.

## 103 — FX 7 (echoes)
- **Method:** A bright pluck/bell tone fed into a long, regenerating
  multi-tap delay with each tap filtered darker (HF damping) so repeats
  fade *and* dull — the canonical "echo drops." Reuse the harp pluck or a
  short FM ping as the source.
- **C state:** source voice + delay ring (≥400 ms @ sr) + per-tap 1-pole LP + feedback.
- **Key params:** delay time (often tempo-synced), feedback, HF damping.

## 104 — FX 8 (sci-fi)
- **Method:** Aggressive sweep: a saw/square swept by a fast envelope
  through a resonant filter, with pitch glide and ring/FM noise bursts —
  "laser zap / sci-fi" gesture. Reuse the engine's resonant biquad with a
  fast downward cutoff sweep + a downward pitch sweep (the gun-classic
  pitch-sweep machinery is directly applicable). Add a noise burst at the
  attack.
- **C state:** osc phase + pitch-sweep mult + resonant biquad with swept cutoff + noise burst env.
- **Key params:** sweep direction/rate, resonance Q, noise transient level.

---

# Family 2 — Ethnic (GM 105–112)

Plucked members (sitar, banjo, shamisen, koto) are **extended
Karplus-Strong** — the engine's `WAVE_HARP` is the base; each adds a
characteristic tweak. Kalimba is a **modal tine** (cantilever beam).
Bagpipe and shanai are **reed waveguides + drone** (extend `WAVE_WHISTLE`).
Fiddle is a **bowed waveguide** (friction-driven loop).

> **Plucked-string foundation (all four):** Karplus, K. & Strong, A.
> (1983) "Digital Synthesis of Plucked-String and Drum Timbres," *CMJ*
> 7(2), pp. 43–55. Jaffe, D. & Smith, J.O. (1983) "Extensions of the
> Karplus-Strong Plucked-String Algorithm," *CMJ* 7(2), pp. 56–69
> (fractional delay, tunable loop filter, decay-stretch). Smith,
> *Physical Audio Signal Processing* (CCRMA) —
> https://ccrma.stanford.edu/~jos/pasp/ . These are already cited in
> `audio.h` above the harp fields.

## 105 — Sitar
- **Method:** Karplus-Strong main string **+ sympathetic strings +
  jawari (buzzing bridge) nonlinearity.** The jawari is a curved bridge
  the string slaps against; it imposes a *position-dependent, dynamic
  delay-length modulation* (the string's effective length shortens when it
  touches the bridge), generating the buzzing high-overtone shimmer.
  Practical real-time recipe (Siddiq): run the main KS string, then pass
  the loop signal through a soft nonlinearity / dynamic fractional-delay
  modulator keyed by signal amplitude (large excursions "buzz" against the
  bridge → add a short, signal-dependent delay perturbation + mild
  waveshaping). Add 9–13 detuned sympathetic strings as cheap parallel KS
  loops (or a comb bank) tuned to the raga, lightly coupled to the main
  string's output — they bloom as the note sustains.
- **C state (extends harp):** main KS loop (existing) + `jawari_thresh`,
  `jawari_depth` (dynamic delay perturbation) + array of N sympathetic
  KS delay lines with their own `lp1`.
- **Key params:** jawari buzz depth/threshold (the defining timbre),
  sympathetic count/tuning/coupling, pluck position (loop-filter
  brightness).

> **Sources:** Siddiq, S. (2012) "A Physical Model of the Nonlinear Sitar
> String," *Archives of Acoustics* 37(1) —
> https://acoustics.ippt.pan.pl/index.php/aa/article/view/129 .
> "The Physical Modelling of a Sitar," ISSTA —
> http://issta.ie/wp-content/uploads/The-Physical-Modelling-of-a-Sitar.pdf
> (jawari as dynamically-changing delay line). Tanpura jvari: same
> bridge-string collision mechanism.

## 106 — Banjo
- **Method:** Karplus-Strong with a **bright, lightly-damped loop**
  (short, fast-decaying — banjo notes ring briefly and plinky) **+ a
  resonant drumhead body.** The banjo head is a tensioned membrane: model
  it as 2–3 parallel biquad body resonances (≈300 Hz head mode + a couple
  higher) excited by the string output, lending the snappy "pop." Keep the
  KS loop-filter cutoff high (less HF damping than guitar) for the
  characteristic twang; short T60.
- **C state (extends harp):** KS loop + 2–3 body biquads (reuse the gun
  `gun_body_*` 3-biquad bank verbatim).
- **Key params:** loop decay (short), loop brightness (high), head
  resonance freq/Q, body mix.

> **Source:** Politis, Bank et al. / "Acoustics of the banjo: measurements
> and sound synthesis," *Acta Acustica* 5 (2021) —
> https://acta-acustica.edpsciences.org/articles/aacus/full_html/2021/01/aacus200055/aacus200055.html
> (head membrane + bridge resonances over the string).

## 107 — Shamisen
- **Method:** Karplus-Strong **+ sawari buzz** (the shamisen's deliberate
  buzzing at the nut on the lowest string — a *lighter cousin of the
  jawari*) **+ skin-membrane body.** Use the sitar's jawari nonlinearity
  at low depth, a percussive plectrum (bachi) attack (short bright noise
  burst before the pluck), and 2 body resonances for the skin. Strong
  attack transient, fast decay.
- **C state:** KS loop + low-depth jawari perturbation + attack noise
  burst + 2 body biquads.
- **Key params:** sawari buzz (subtle), bachi attack brightness, decay
  (short), body resonance.

## 108 — Koto
- **Method:** Karplus-Strong **+ pitch bend** (the koto's signature is
  *oshi-de* / *ato-oshi* pressing the string left of the movable bridge to
  bend pitch up a semitone/tone). The existing harp's `target_frequency`
  smoothing already provides glide — drive it from a per-note bend
  envelope. Mellow, harp-like loop (more HF damping than banjo), medium
  decay, with a soft felt-pick attack.
- **C state (extends harp):** KS loop + bend envelope writing
  `target_frequency`.
- **Key params:** bend amount/curve (defining gesture), loop decay
  (medium-long), attack softness.

> **Source (koto bend technique):** Koto (instrument), Wikipedia —
> https://en.wikipedia.org/wiki/Koto_(instrument) (ōshi pressure-bending
> raises pitch a half/whole tone).

## 109 — Kalimba
- **Method:** **Modal tine** — a thumb-piano lamella is a *cantilever
  beam* (clamped one end, free the other, intermediate bridge). Its
  overtones are strongly inharmonic: measured ratios put the prominent
  overtones at **≈5× and ≈14× the fundamental** (Euler-Bernoulli beam).
  Use the modal bank with 3 modes at ratios `{1.0, 5.4, 14.7}` (cantilever
  `B^cant`: 0.597, 1.494, 2.500 → squared-scaled gives the ~5×, ~14×
  family), fast HF decay (top mode T60 ≈ 60 ms, fundamental ≈ 600 ms–1 s),
  struck by a short impulse. A soft "thunk" body resonance (the wooden
  box) under it. This is far more authentic than KS for the kalimba's
  pure, bell-like tine ping.
- **C state:** `modal_bank` with 3 resonators (freqs, T60s, gains) + 1
  body biquad.
- **Key params:** inharmonic mode ratios (≈1 / 5 / 14), per-mode decay,
  attack hardness, box resonance.

> **Sources:** "The tones of the kalimba (African thumb piano)" — and
> "Vibrational frequencies and tuning of the African mbira" (ResearchGate)
> — first three transverse beam modes, prominent overtones ~5× & ~14×
> fundamental:
> https://www.researchgate.net/publication/221780579_The_tones_of_the_kalimba_African_thumb_piano .
> Cantilever-beam ratios from Nathan Ho / modal-synthesis literature
> (`B^cant(1)=0.597, (2)=1.494, (3)=2.500`).

## 110 — Bag pipe
- **Method:** **Reed waveguide chanter + continuous drones.** Reuse the
  whistle/flute waveguide (`WAVE_WHISTLE`) but swap the air-jet excitation
  for a *single/double reed* nonlinearity: a pressure-controlled reed
  reflection (clipped/saturating reflection coefficient at the bore
  entrance) gives the buzzy, constant-pressure timbre. Crucially: **no
  envelope silence between notes** (bagpipe is continuously blown — pitch
  changes are slurred) and **two or three fixed drone voices** (tonic +
  octave) sounding underneath at all times.
- **C state:** whistle bore delay + reed reflection table/saturation +
  separate sustained drone voices.
- **Key params:** reed stiffness (buzz), bore length (pitch), drone
  pitches, continuous (no-gap) legato.

> **Source:** woodwind digital-waveguide reed modeling (reed as SHO +
> bore transmission line; clipped reflection) — "Synthesis of woodwind
> instruments sounds using digital waveguide modelling" —
> https://www.academia.edu/8223502/ . Cook flute model already in
> `generate_whistle_sample` (`audio.h:88`) is the structural base.

## 111 — Fiddle
- **Method:** **Bowed digital waveguide.** A string delay loop driven by a
  *friction (bow) excitation*: at the bow point, the string velocity is
  pushed by a nonlinear bow-friction curve (stick-slip — a sharply
  saturating function of the velocity difference between bow and string).
  This is the McIntyre-Schumacher-Woodhouse / Smith bowed-string model:
  two delay rails (nut side, bridge side) summed at the bow point through
  the friction table; add vibrato LFO on the loop length. Sustained, not
  plucked — the bow continuously injects energy.
- **C state:** two delay rails (or one loop + bow-point tap) + friction
  lookup/saturation + bow velocity/force params + vibrato LFO + body biquads.
- **Key params:** bow velocity & force (timbre/loudness), bow position
  (harmonic content), vibrato depth/rate, body resonance.

> **Sources:** Smith, J.O., *PASP*, "Bowed Strings" / digital-waveguide
> bowed string — https://ccrma.stanford.edu/~jos/pasp/ . McIntyre, M.,
> Schumacher, R., Woodhouse, J. (1983) "On the oscillations of musical
> instruments," *JASA* 74(5). "Empirical physical modeling for bowed
> string instruments" (ResearchGate, friction model).

## 112 — Shanai (shehnai)
- **Method:** **Double-reed waveguide + drone**, very close to bagpipe but
  *with* expressive per-note envelopes and bends (it's a played melodic
  oboe-like instrument, not a constant drone instrument — though often
  accompanied by a sur/drone). Reuse the bagpipe reed waveguide with a
  brighter, more open reflection (richer high harmonics, nasal formant
  via a couple of band-passes), strong vibrato, and pitch-bend glides.
- **C state:** whistle bore + double-reed saturation + 2 formant biquads
  + vibrato + bend env.
- **Key params:** reed brightness, nasal formant centers, vibrato,
  expressive bend.

---

# Family 3 — Percussive (GM 113–120)

The pitched/struck metal & wood instruments (tinkle bell, agogo, steel
drum, woodblock, melodic tom, synth drum) are **modal synthesis** — the
shared modal bank, tuned to measured frequency ratios. Membrane drums
(taiko, tom) add a 2D-membrane / nonlinear-tension flavor. Reverse cymbal
is the odd one out: **reversed noise envelope.**

## 113 — Tinkle Bell
- **Method:** Modal bank, **bell ratios.** A small struck bell has
  characteristic inharmonic partials. Use 4–5 modes at the classic bell
  ratios relative to a nominal strike note: **hum ≈0.5, prime 1.0, tierce
  ≈1.2 (minor third), quint ≈1.5, nominal ≈2.0**, plus a high "tink"
  partial ~3–4×. High partials decay fast (tinkle = bright, short); a
  small bell skips the long hum, so weight the upper modes and keep T60
  ≈0.3–0.8 s.
- **C state:** `modal_bank` 5 modes (freqs from ratios, decreasing T60).
- **Key params:** mode ratios (esp. minor-third tierce → "bell" identity),
  brightness, short decay.

> **Source:** Reid, "Synthesizing Bells" (Sound On Sound) — bell partials
> hum/prime/tierce(minor 3rd)/quint/nominal; FM or additive realizations —
> https://www.soundonsound.com/techniques/synthesizing-bells .
> Fletcher & Rossing, *The Physics of Musical Instruments* (2nd ed.,
> Springer 1998), bell-partial chapter.

## 114 — Agogo
- **Method:** Modal bank, **two-pitch metal bell** (the agogo is a pair of
  tuned cowbell-like bells, usually a ~minor-third or fourth apart).
  Cowbell-style metal: 2–3 sharply inharmonic modes (a clangy, slightly
  detuned pair around ~550 Hz & ~830 Hz for the big bell), very short
  bright decay, hard mallet attack (1-sample impulse + tiny click). Pick
  bell A vs B by note range.
- **C state:** `modal_bank` 2–3 modes × two presets (hi/lo bell).
- **Key params:** the two bell pitches, inharmonic detune, hard short
  decay, metallic brightness.

## 115 — Steel Drums (steelpan)
- **Method:** Modal bank with the steelpan's **specific inharmonic
  modes.** Measured steelpan notes are tuned so the *octave and twelfth
  are reinforced*, but the third prominent mode is characteristically
  **non-harmonic** — often near an **octave-plus-a-third or
  octave-plus-a-fourth** above the fundamental (≈2.5–2.66×) rather than a
  pure 2× or 3×. Use ~5 modes at ratios `{1.0, 2.0, ~2.6, 3.0, ~4.2}` with
  medium decay and a bright shimmer; the note "blooms" because energy
  transfers between modes (nonlinear coupling). A cheap nod to that
  nonlinearity: feed a little of the fundamental mode's output, squared,
  into the upper modes so they swell after the strike.
- **C state:** `modal_bank` 5 modes + optional nonlinear cross-feed term.
- **Key params:** the non-harmonic 3rd mode ratio (the steelpan
  fingerprint), bloom/coupling amount, medium-long ringing decay.

> **Sources:** Rossing, T.D. et al., steelpan modal studies; Monteil, Touzé
> et al., "Identification of mode couplings in nonlinear vibrations of the
> steelpan," *Applied Acoustics* (2015) —
> https://www.sciencedirect.com/science/article/abs/pii/S0003682X14002151
> (nonlinear energy exchange between modes). Stockholm Steel Band, "Tone
> generation in steel pans" — third mode often an octave-plus-third/fourth
> above fundamental:
> https://stockholmsteelband.se/pan/tuning/theory20_tone_generation.php .

## 116 — Woodblock
- **Method:** Modal bank, **free-free bar / wood block** with very few,
  high, fast-decaying modes. A wood block is mostly one dominant pitched
  "tock" plus a couple of higher partials. Use 2–3 modes (`f`, ~2.7·f,
  ~5.4·f from free-free beam ratios `B^free`: 1.506, 2.500, …) with very
  short T60 (≈40–120 ms) and a hard impulse excitation. The "tock" comes
  from the fast decay + the dominant low mode; wood = quick HF rolloff.
- **C state:** `modal_bank` 2–3 modes, short T60, hard impulse.
- **Key params:** dominant pitch, very short decay, wooden (dull-but-clicky)
  brightness.

> **Source:** free-free beam mode ratios (`B^free(1)=1.506, (2)=2.500,
> (k)=k+0.5`) — xylophone/marimba/wood-bar family — Nathan Ho modal notes;
> Fletcher & Rossing ch. on bars.

## 117 — Taiko Drum
- **Method:** **Membrane modes + body + pitch drop.** A large drum is a
  circular membrane: its modes follow Bessel-zero ratios `{1.0, 1.59,
  2.14, 2.30, 2.65, 2.92, …}` (the `(m,n)` modes `J_(m-1)n`). For a deep
  taiko boom, weight the low modes heavily, add a **downward pitch sweep**
  (membrane tension/air-load makes the perceived pitch drop right after
  the hit — the gun-classic `boom` pitch-sweep machinery is a perfect
  fit), plus a big low-frequency body/shell resonance and a noise "thwack"
  transient. The existing `WAVE_GUN` boom+tail layering is *directly*
  reusable for taiko (low sine boom with exp pitch drop + LPF noise tail).
- **C state:** 3–5 membrane modes (or reuse gun boom-sine + pitch-sweep) +
  shell biquad + attack noise burst.
- **Key params:** fundamental (big & low), pitch-drop depth/rate, membrane
  vs body balance, attack noise.

> **Sources:** Fletcher & Rossing, membrane chapter (Bessel-zero mode
> ratios). Cook, *Real Sound Synthesis*, drum/membrane modeling. Reuse of
> the engine's `generate_gun_classic_sample` boom+tail (`audio.c:1451+`,
> documented `audio.h:142+`).

## 118 — Melodic Tom
- **Method:** Same membrane-mode engine as taiko, **tuned and pitched**
  across notes, less boom and more tonal "dooong." Fewer low modes,
  emphasize the membrane fundamental, moderate pitch drop, tighter tuning
  (tunable drums favor the first membrane mode). Pitch tracks the MIDI
  note. Medium decay.
- **C state:** membrane modes scaled to note + moderate pitch sweep + body
  biquad.
- **Key params:** note pitch, modest pitch drop, decay length, head
  tension (mode spread).

## 119 — Synth Drum
- **Method:** **Pitched sine/triangle with a fast downward pitch sweep**
  (the 808/Simmons "pew" tom) + a click transient. This is the synthetic
  cousin of the melodic tom — *not* physically modeled. A single sine,
  amplitude AD-envelope (fast attack, exp decay), frequency exponentially
  sweeping from ~3–5× start down to the note (reuse `gun_boom` sweep). Add
  a tiny noise click. Optionally a touch of FM for the "electro" zap.
- **C state:** 1 sine osc + pitch-sweep mult + amp env + click env.
- **Key params:** start/end pitch ratio (the "dewww" amount), decay,
  click level, optional FM index.

## 120 — Reverse Cymbal
- **Method:** **Reversed-envelope filtered noise.** A cymbal is dense
  high-frequency noise; "reverse" means the amplitude envelope *rises* to
  a peak and then cuts — a swell. Generate band-passed/high-passed bright
  noise (reuse `WAVE_NOISE` with a high cutoff, plus a couple of resonant
  metallic band-passes for shimmer) under a **rising attack envelope**
  (linear or exp rise over ~0.5–2 s) terminated by a hard cut at the
  downbeat. No pitch.
- **C state:** noise biquad(s) + rising-envelope generator + hard cut.
- **Key params:** swell duration, brightness (cutoff), 2–3 metallic
  resonances, cut sharpness.

---

# Family 4 — Sound Effects (GM 121–128)

Noise shaping + filtering + amplitude/pitch modulation. Several map onto
existing engine models almost directly (gunshot → `WAVE_GUN`, bird →
`WAVE_WHISTLE`, seashore/applause/breath/helicopter → `WAVE_NOISE` /
PhISEM).

## 121 — Guitar Fret Noise
- **Method:** Short **filtered-noise squeak/scrape** — finger sliding on a
  wound string. A band-passed noise burst (centered ~1.5–4 kHz) with a
  fast pitch/cutoff glide (the squeak rises or falls as the finger moves)
  and a very short envelope, optionally a faint plucked KS click at the
  end (the finger landing). Reuse `WAVE_NOISE` + a swept band-pass cutoff.
- **C state:** noise biquad with swept center freq + short env.
- **Key params:** scrape brightness, glide direction/rate, duration
  (very short).

## 122 — Breath Noise
- **Method:** **Filtered-noise puff** — band-limited (low-pass ~2–4 kHz,
  high-pass ~300 Hz) white noise with a soft attack-decay envelope, gentle
  amplitude wobble. This is the same air-noise the whistle/flute model
  already injects; standalone it's just shaped noise. Optionally add a
  faint formant band-pass for a more "hh" vocal breath.
- **C state:** noise biquad (BP) + AD env + slow amp LFO.
- **Key params:** band center/width, puff length, breathiness.

## 123 — Seashore
- **Method:** **PhISEM / filtered-noise surf** — broadband noise shaped by
  a *slow swell LFO* (waves come and go over ~3–8 s) through a low-pass
  whose cutoff opens at the wave's peak (the "shhh" brightens as the wave
  breaks). Equivalent to Cook's rain model with huge `numObjects` and a
  very slow energy LFO. Layer two or three independent swells at different
  rates/phases for a natural, non-repeating shore.
- **C state:** noise biquad + 2–3 slow swell LFOs (random rates) modulating
  amplitude *and* cutoff.
- **Key params:** swell rate(s) & depth, brightness at peak, number of
  overlapping swells.

> **Source:** Cook, *Real Sound Synthesis* — the rain/PhISEM model yields
> waterfall/surf by parameter change; filtered-noise-with-swell is the
> standard ocean recipe.

## 124 — Bird Tweet
- **Method:** **Pitch-swept sine / small-index FM** in the 2–8 kHz bird
  range. A bird chirp is a fast frequency glide (often up-then-down) on a
  near-sine, with rapid trills (a fast LFO on pitch) and short syllables.
  Recipe: a sine whose frequency follows a short swept envelope (e.g.
  rise 3→5 kHz over 40 ms), optional shallow FM for the "warble," gated
  into 1–3 quick syllables. Reuse the whistle waveguide for a more "tweet"
  timbre, or a plain swept sine for the classic GM bird.
- **C state:** sine phase + pitch-sweep envelope + trill LFO + syllable
  gating env.
- **Key params:** frequency range/sweep shape, trill rate, syllable count.

> **Source:** birdsong is dominated by rapid FM in the ~2–8 kHz band —
> Stowell & Plumbley (2014), "Large-scale analysis of frequency modulation
> in birdsong databases," *Methods in Ecology and Evolution* —
> https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12223 .

## 125 — Telephone Ring
- **Method:** **Dual-sine tone, gated.** A North-American ringtone is two
  sines (440 Hz + 480 Hz) summed, hard-gated in a **2 s on / 4 s off**
  cadence (the classic telephone-ring pattern, conceptually DTMF-like dual
  tones). For a "trimphone/electronic" warble, frequency-modulate the
  pair. Trivial: 2 phase-increment sines + a square-wave gate envelope.
- **C state:** 2 sine phases + cadence gate counter.
- **Key params:** the two frequencies (440/480 = US; 400/450 = UK),
  on/off cadence, warble rate.

## 126 — Helicopter
- **Method:** **Amplitude-modulated broadband noise (blade chop) + low
  rotor thump.** The rotor produces periodic amplitude modulation (PAM) of
  broadband aero-noise: take low-pass noise and multiply by a periodic
  pulse train at the blade-passage rate (~10–20 Hz: rotor RPM × blade
  count), giving the "whump-whump-whump." Add a low body/engine rumble
  (LP noise or a low buzzy oscillator) and a high turbine whine. The pulse
  shape (sharper = more "slap") sets the character.
- **C state:** noise biquad (broadband) × periodic AM pulse (phase-increment
  at blade rate, shaped) + low rumble layer.
- **Key params:** blade-passage rate (chop tempo), pulse sharpness (blade
  slap), rumble/whine balance.

> **Source:** periodic amplitude modulation (PAM) model of rotor
> aero-noise — "An analytical approach to rotor blade modulation,"
> *Applied Acoustics* (2021) —
> https://www.sciencedirect.com/science/article/abs/pii/S0165212521000603 .

## 127 — Applause
- **Method:** **PhISEM crowd model.** Each clap is a short filtered-noise
  burst; a crowd is many stochastic clap events. Use the PhISEM engine
  with high `numObjects` (hundreds–thousands of clappers), a clap
  resonance band-pass (~1–2 kHz), and an "affinity" parameter (Cook's
  ClapLab): low affinity → dense random wash (full applause), high
  affinity → synchronized rhythmic clapping. Swell the overall energy in
  and out. This is literally one of Cook's named PhISEM outputs.
- **C state:** PhISEM (energy, sysDecay, large num, clap biquad) +
  affinity/clustering term + global swell env.
- **Key params:** crowd density (num × energy), clap brightness, affinity
  (sync vs wash), swell.

> **Source:** Cook, *Real Sound Synthesis* — ClapLab / applause from
> PhISEM (mean/SD of clap center freq, period, and "affinity" =
> sync tendency, 0 random … 128 unison).

## 128 — Gunshot
- **Method:** **Use the existing `WAVE_GUN` model directly.** The engine
  already implements both a 3-layer classic gunshot (crack BPF burst +
  pitched boom with downward sweep + LPF noise tail + sub-ms click) and a
  physical DWG model (Friedlander blast-wave excitation → bore resonance +
  3 body-mode biquads + radiation HPF + ground-reflection echo), with 12
  weapon presets. For GM "Gunshot," `GUN_PISTOL` or `GUN_RIFLE` classic is
  the canonical short bang. No new code needed — just route program 128 to
  `audio_synth_gun(...)`.
- **C state:** existing `gun_*` fields in `ACVoice`.
- **Key params:** preset (pistol/rifle), `pressure_scale` (loudness/size),
  model (classic vs physical).

> **Source:** in-engine model, documented `audio.h:38–179` &
> `audio.c:generate_gun_*`. Friedlander blast-wave reference embedded
> there; classic 3-layer per `audio.h:142+`.

---

## Implementation map — what to add to `audio.c`

1. **`WAVE_MODAL`** + a `modal_bank` (N≤6 two-pole resonators in
   `ACVoice`: `mode_f[6]`, `mode_R[6]`, `mode_g[6]`, `mode_y1[6]`,
   `mode_y2[6]`, struck by an impulse on note-on). Serves: kalimba, tinkle
   bell, agogo, steel drum, woodblock, melodic tom (and taiko's tonal
   part). Tables of `{ratios, T60s, gains}` per instrument, exactly like
   `gun_presets[]`.
2. **`WAVE_PHISEM`** + a particle struct (`energy, sysDecay, sndDecay,
   numObjects, res biquad(s), gain`). Serves: rain, seashore, applause,
   and the shaker family. Constant tables from the STK numbers above.
3. **Reuse**: `WAVE_HARP` (sitar/banjo/shamisen/koto/fret — add a
   `jawari` nonlinearity flag + sympathetic-loop array + bend env),
   `WAVE_WHISTLE` (bagpipe/shanai/bird — add a reed-saturation reflection
   option + drone voices), `WAVE_NOISE` (breath/helicopter/reverse-cymbal
   — add swept cutoff + AM-pulse + rising-envelope options), `WAVE_GUN`
   (gunshot/taiko boom — already present).
4. **Small new per-voice DSP utilities** shared by Synth FX: a delay-line
   ring (echoes/crystal/atmosphere), an LFO phase, a ring-mod multiply.

Per-sample cost stays low: modal = N×(1 mul-add chain); PhISEM = 1 RNG +
1 resonator; everything else is the existing oscillator/noise/waveguide
loops. All fit comfortably inside 32 voices at 192 kHz.

---

## Consolidated references

- Karplus, K. & Strong, A. (1983). "Digital Synthesis of Plucked-String
  and Drum Timbres." *Computer Music Journal* 7(2): 43–55.
- Jaffe, D.A. & Smith, J.O. (1983). "Extensions of the Karplus-Strong
  Plucked-String Algorithm." *CMJ* 7(2): 56–69.
- Smith, J.O. *Physical Audio Signal Processing* (online, CCRMA). https://ccrma.stanford.edu/~jos/pasp/
- Cook, P.R. (1997). "Physically Informed Sonic Modeling (PhISM)." *CMJ* 21(3): 38–49. https://ccrma.stanford.edu/software/stk/classstk_1_1Shakers.html
- Cook, P.R. (2002). *Real Sound Synthesis for Interactive Applications.* A K Peters.
- McGill MUMT-618 PhISEM notes. https://www.music.mcgill.ca/~gary/618/week12/phism.html
- Adrien, J.-M. (1991). "The Missing Link: Modal Synthesis." In *Representations of Musical Signals*, MIT Press.
- Nathan Ho, "Exploring Modal Synthesis." https://nathan.ho.name/posts/exploring-modal-synthesis/
- Siddiq, S. (2012). "A Physical Model of the Nonlinear Sitar String." *Archives of Acoustics* 37(1). https://acoustics.ippt.pan.pl/index.php/aa/article/view/129
- "The Physical Modelling of a Sitar," ISSTA. http://issta.ie/wp-content/uploads/The-Physical-Modelling-of-a-Sitar.pdf
- "Acoustics of the banjo: measurements and sound synthesis." *Acta Acustica* 5 (2021). https://acta-acustica.edpsciences.org/articles/aacus/full_html/2021/01/aacus200055/aacus200055.html
- "The tones of the kalimba (African thumb piano)." https://www.researchgate.net/publication/221780579
- Monteil, Touzé et al. (2015). "Identification of mode couplings in nonlinear vibrations of the steelpan." *Applied Acoustics*. https://www.sciencedirect.com/science/article/abs/pii/S0003682X14002151
- Stockholm Steel Band, "Tone generation in steel pans." https://stockholmsteelband.se/pan/tuning/theory20_tone_generation.php
- Fletcher, H. & Rossing, T.D. (1998). *The Physics of Musical Instruments,* 2nd ed., Springer.
- McIntyre, M., Schumacher, R., Woodhouse, J. (1983). "On the oscillations of musical instruments." *JASA* 74(5).
- Reid, G. "Synth Secrets" (Sound On Sound). https://www.soundonsound.com/series/synth-secrets-sound-sound — esp. "Synthesizing Bells" https://www.soundonsound.com/techniques/synthesizing-bells
- Stowell, D. & Plumbley, M. (2014). "Large-scale analysis of frequency modulation in birdsong databases." *Methods in Ecology and Evolution.* https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12223
- "An analytical approach to rotor blade modulation." *Applied Acoustics* (2021). https://www.sciencedirect.com/science/article/abs/pii/S0165212521000603

*Documented: 32 / 32 GM programs (97–128). — fedac native audio research dossier, part 04.*
