# novelizer — batch reports

# batch 2 — percussion (kicks + snares)

**Rendered + judged**: 2026-07-02 · 6 builders + 6 adversarial judges
(12 agents, ~628k subagent tokens). New harness melody: `beat` — a 2-bar
104 BPM 16th accent map at C1, the percussion battery. **All six kept**
(threshold: novelty ≥ 6, punch ≥ 6, mixability ≥ 5).

| voice        | role  | technique (citation)                                    | N | P | M |
|--------------|-------|---------------------------------------------------------|---|---|---|
| memkick      | kick  | tension-modulated Bessel membrane (Fletcher & Rossing; Bilbao) | 7 | 9 | 8 |
| cavikick     | kick  | Helmholtz port + Ingard–Ising orifice loss (Thiele/Small) | 8 | 7 | 8 |
| implokick    | kick  | chaotic-collapse feedback FM (Tomisawa; chaosfm lineage) | 7 | 8 | 8 |
| wiresnare    | snare | membrane + wire contact collisions (Rossing; Bilbao)    | 8 | 8 | 8 |
| cracklesnare | snare | logistic-map spike train (May 1976; Shaw's faucet)      | 8 | 8 | 7 |
| gransnare    | snare | stochastic pulsaret cloud (Roads; pulsar lineage)       | 7 | 7 | 7 |

N novelty · P punch · M mixability. Standouts: **memkick**'s pitch drop is
emergent (glide depth/speed/spectrum/decay co-vary with strike force through
one energy variable — accents dive, ghosts barely bend); **cavikick** is the
air side of a drum, not the head (fixed Helmholtz pitch, velocity-squared
port loss = physical compression, flow-gated chuff); **wiresnare**'s and
**cracklesnare**'s tails granulate/sputter instead of hissing (collision
process / chaotic orbit, not filtered noise); **gransnare**'s body is a
statistical condensation of phase-locked grains.

Judges' improvement threads: memkick low-vel T60 cap + small-speaker
translation (write kick lines at C2+), cavikick knock presence, implokick
optional HF tilt, wiresnare persistent wire bank for true rolls,
cracklesnare top-end air + body register floor, gransnare fold floor
~160-180 Hz.

**First deployment**: `pop/novelette/` — memkick (downbeats) + cavikick
(shuffle ghosts) + gransnare (backbeat) + cracklesnare in a STRETCHED
adaptation (note length scales every time constant → multi-second chaos
washes at the section doors).

---

# batch 1 report

**Rendered + judged**: 2026-07-02 · 6 builder agents + 6 adversarial judges
(12 agents, ~617k subagent tokens) · harness `c/novelizer.h`, features via
`bin/analyze.mjs`, spectrograms via ffmpeg. Every voice compiled warning-free,
rendered all five test melodies with zero NaN/silence, and passed chromatic
tuning. **All six judged keepers** (threshold: novelty ≥ 6 AND musicality ≥ 6).

| voice   | technique (citation)                                  | N | M | E | shines on |
|---------|-------------------------------------------------------|---|---|---|-----------|
| scanner | scanned synthesis (Verplank/Mathews/Shaw, ICMC 2000)  | 8 | 7 | 7 | drone     |
| pulsar  | pulsar synthesis (Roads, *Microsound* 2001)           | 8 | 8 | 7 | drone     |
| frictus | banded waveguides (Essl & Cook, ICMC 1999)            | 8 | 8 | 7 | drone     |
| twomass | two-mass folds (Ishizaka–Flanagan '72 / Herzel '95)   | 8 | 7 | 8 | stab      |
| chaosfm | feedback-FM bifurcation (Tomisawa '81 / Slater '98)   | 7 | 7 | 8 | drone     |
| vosim   | VOSIM (Kaegi & Tempelaars, JAES 1978)                 | 7 | 7 | 7 | lyrical   |

N = novelty vs the AC stable · M = musicality · E = entertainability (1–10,
adversarial judges instructed to default-skeptic and verify the C implements
the cited technique, not a rebrand).

## verdicts

### scanner — the wavetable that is alive
Struck kalimba/e-piano whose overtone mix refuses to freeze: a 128-mass
haptic-band (2.1–25 Hz) string is hammered at note-on and scanned as the
wavetable, so pitch stays scan-locked while partial amplitudes crawl,
breathe, and tremolo at finger rate. Unlike zitar/KS (whole spectrum decays
together at audio rate), partials here move independently.
**Next**: sustain sits ~20 dB under the strike (push finger force /
compensate), smooth the table's HF scan images, route velocity into finger
force, expose finger rate/depth.

### pulsar — a vowel fired from a laser
Trains of expodec-windowed dual-partial pulsarets; formant ridge
(750–2850 Hz, velocity-scaled) zaps down into a rounder vowel while duty
widens; long notes get masking-driven granular flutter riding a rock-steady
fundamental. Decisively unlike skrill's swept-filter formants — the formant
is built from particle geometry.
**Next**: per-note vowel targets (break the identical zap fingerprint),
unlock f2 from 2.6·f1 for a real vowel space, expose masking density,
thicken the high register.

### frictus — bowed metal that breathes
Only voice in the stable that *sustains* an inharmonic mode stack: bow
stick-slip friction holds uniform-bar partials (1 : 2.76 : 5.40 : 8.93)
aloft with noise skirts; bow-bite attack, metallic ring-out on release,
pressure/speed LFOs make long notes breathe.
**Next**: brighten low-register drones (upper-mode weights vs f0), lowpass
the bow-noise injection (~6–8 kHz), add a glass/bowl ratio table, make
`Band[]` stack-local, ±0.2% ratio jitter to de-clone repeats.

### twomass — the creature
Real self-oscillating two-mass folds (Bernoulli-pumped limit cycle) into a
Kelly-Lochbaum tube with per-note wa/eee diphthong glides; hard-blown notes
(vel > 0.86) roughen into subharmonic growl. Mid/upper register judged
release-ready.
**Next (the one real defect in the batch)**: the D2 drone under-collides —
reads ~1.6 st flat and mostly aspiration hiss. Try register-specific
collision stiffness / phonation-threshold, steeper mouth lowpass on low
notes, clamp the F0 calibration in the bottom octave.

### chaosfm — playing the bifurcation
Single Tomisawa averaged-feedback operator with β enveloped 0.35 → 1+2·vel
per note: sine → quasi-saw → fizzy controlled chaos → re-congeal, a +3.6 ¢
second operator making the chaotic crest beat. The expressive axis is
bifurcation depth — no modulator, no formant.
**Next**: 2× oversampling (deep-chaos stabs reach Nyquist), vary the β arc
per note (identical mid-note bloom will read samey across a track), expose
detune/mix/β-ceiling as lane params.

### vosim — the robot vowel
Faithful 1978 VOSIM: N sin² pulses of decaying amplitude + gap per period;
one broad resonance glides a-e-i-o-u-ae with delayed 6-cent vibrato; velocity
widens the formant band from dull two-pulse to bright eight-pulse buzz.
Drier and buzzier than skrill — the resonance rides ON the harmonics, no
filter anywhere.
**Next**: fix high-register collapse (above ~660 Hz every period degenerates
to N=1 — transpose the vowel table by register), add a second pulse group
for F2 (make the talking literal), soften the 60 ms release, gap-noise
breath at high velocity.

## listening

`slab-audio out/<voice>-<melody>.wav ...` opens the tiled jukebox wall
(cover art/metadata/waveform tiles, auto-advancing queue) — built alongside
this batch. `slab-afplay` for headless playback.

## graduation candidates

- **frictus** and **scanner** are natural chillwave/ambient lane material
  (sustained evolving texture).
- **chaosfm** wants a dance/hellsine-adjacent role (the bloom arc is a
  build-and-drop gesture at note scale).
- **twomass** is a character voice — jungle dub-siren territory, or a
  comodiddies mascot.
- **pulsar** and **vosim** are lead/hook voices; vosim especially wants a
  second formant before shipping.

## batch 2 seeds (not yet built)

Judges' cross-cuts suggest: two-formant VOSIM (F2 group), glass-bowl ratio
table for frictus, register-aware twomass, and fresh techniques still
unexplored — waveset distortion (Wishart), single-cycle concatenative
scrambling, Chua-circuit oscillators, modal snow (stochastic modal
excitation).
