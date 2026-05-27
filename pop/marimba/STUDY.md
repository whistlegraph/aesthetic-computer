# marimba — synthesis study

Classic-research notes behind `synths/marimba.mjs`. Goal: a marimba
voice that holds up against a sampled library while staying pure-DSP
and JS↔C portable (same constraint as zitar / skrill / hoover).

## the instrument in one paragraph

A marimba bar is a wooden (rosewood) or synthetic (Kelon / Klyperon)
beam, ~24 cm wide and a few cm thick, supported at its two nodal lines
(~22% from each end — the points where mode 1 has zero displacement),
struck on top by a mallet, with a half-wavelength PVC tube hanging
underneath. The bar produces the pitch through transverse bending
modes; the tube boosts only the fundamental; the mallet sets the
attack character.

## modal tuning — the 1 : 4 : 10 rule

A uniform free-free beam (Euler-Bernoulli) has modes at non-harmonic
ratios:

```
mode 1 : mode 2 : mode 3 : mode 4 : mode 5
1.000  : 2.756 : 5.404  : 8.933  : 13.34
```

These ratios are musically useless — a struck uniform bar sounds
"clangy," not pitched. Marimba bars are deeply UNDERCUT (an arch milled
out of the underside, deepest at the center) which lowers the higher
modes more than the fundamental. The undercut depth and shape are
tuned so that:

```
mode 1 : mode 2 : mode 3
1.000  : 4.000  : ~9.2 (rosewood) or ~10.0 (Kelon)
```

- **Mode 2 = 4f** (two octaves above fundamental) — held within a few
  cents by every professional maker. This is what gives marimba its
  pitched identity (an octave above is unambiguous).
- **Mode 3 ≈ 10f** (three octaves + major third) — this one varies
  between makers and bar materials. Rossing's measurements of
  rosewood concert bars cluster near 9.2; the design target on Kelon
  synthetic bars is 10.0 (verified by Bretos et al. FEM).
- **Mode 4 and above** are untuned and decay too fast to perceive
  pitch — they contribute to the "wooden" attack noise.

Compare neighbours:
- Xylophone: 1 : 3 : 6 (a more dissonant overtone, brighter)
- Vibraphone: 1 : 4 : 10 (same as Kelon marimba; the difference is
  metal bars with much longer decay + motorised disc tremolo)

References for the ratios:
- Rossing, T. D. (2000). *Science of Percussion Instruments.* World
  Scientific. Chapter 4 tabulates measured modal frequencies.
- Bork, I. (1995). "Practical tuning of xylophone bars and resonators."
  *Applied Acoustics* 46:103–127.
- Bretos, Santamaría, Moral (1999). "Vibrational patterns and frequency
  responses of the free bars of a xylophone and a marimba measured by
  holographic interferometry." *JASA* 105(3):1432–1440.

## decay times

Each mode has its own T60 (time to drop 60 dB). On rosewood:

| mode | T60 (low register) | T60 (high register) |
|------|--------------------|---------------------|
| 1    | 1.5 – 2.5 s        | 0.4 – 0.8 s         |
| 2    | 0.3 – 0.6 s        | 0.1 – 0.2 s         |
| 3    | 0.05 – 0.15 s      | 0.02 – 0.05 s       |

The very-fast decay of higher modes is what makes the attack feel
"woody" and the sustain feel "pure" — by the time you hear the
sustaining note, only the fundamental is left.

Synthetic bars (Kelon) have shorter mode-1 decay (lossier material) but
cleaner mode-3 tuning. They trade off ring for repeatability.

## the tube resonator

A closed-bottom PVC tube of length L ≈ c / (4 f₀) is a quarter-wavelength
resonator — its first resonance is exactly at the fundamental. Hung
under the bar, it narrow-band-amplifies the fundamental only. The
higher modes pass through unmagnified, which is why marimba's attack
is so distinctly percussive (broadband content) and its sustain is so
pitched (single sine).

In the synth this is a Chamberlin SVF band-pass at f₀ with Q ≈ 12–25,
matching Bork's measurements for typical 50 mm PVC tubes.

## mallet excitation

A marimba strike is a brief impulse. The duration of mallet-bar
contact (1 ms hard rubber → 4 ms soft yarn) acts as a low-pass filter
on the input spectrum:

- **short contact (hard mallet, 1 ms)** — flat spectrum out to ~500 Hz
  cutoff → all modes excited → bright, percussive, "woody"
- **long contact (soft yarn, 4 ms)** — low-passed at ~125 Hz → mostly
  the fundamental → warm, dark, "singing"

The synth models this as a half-cosine force pulse of width = mallet
contact time. Same trick as Chaigne & Doutaut (1997) used for their
xylophone simulation boundary excitation.

Mallet hardness is the single biggest tonal control a player has, and
in the synth it's the `mallet` preset field (seconds).

## strike position

Striking exactly on the center maximally drives mode 1 (the centerline
is an antinode for mode 1, but a node for mode 2 and an antinode for
mode 3). Off-center strikes pick up more mode 2 colour — the bouncy,
slightly hollow "ploink" you hear in fast marimba runs.

We approximate the mode shape at fraction x along the bar as
|cos(nπx)|, which is the textbook simplification used in Bork (1995)
and the STK Marimba model. Center strike (x=0.5) → mode 2 cancels
(perfect octave purity); x=0.33 → all three modes audible.

## synthesis approaches considered

### 1. Modal additive (what we use)

Sum of N exponentially-damped sinusoids at the modal frequencies.

- ✅ Pure float, JS↔C portable
- ✅ Parameters map 1:1 to physical measurements
- ✅ Cheap — 3 sines per voice
- ❌ Cannot capture nonlinear pitch glide of very hard strikes (a
  rosewood bar's mode 1 stretches ~5 cents sharp at fortissimo). Not
  audible in pop-stack context.

### 2. Banded waveguides (Essl & Cook 1999, STK)

N short waveguides, one per mode, looped through a mallet model.

- For percussion (impulse excitation), algebraically reduces to a
  damped sinusoid per band — i.e. same impulse response as approach 1.
- Only audibly different when there's continuous excitation (bow,
  scrape). Mallet strikes don't qualify.
- Costs more CPU for the same output. Skip.

### 3. FM (DX7-style marimba)

Single carrier + modulator. Cheap, but parameters are opaque (no
direct link to mode frequencies or decay times) and the inharmonicity
is constrained to integer ratios. We pay 3× more CPU than FM but get
real physical parameters, which matters for the next step (porting to
C and exposing via .np scores).

### 4. FDTD beam simulation (Chaigne & Doutaut 1997)

Solves the actual PDE of the bar via finite differences.

- Gold standard for research — captures every subtle nonlinearity.
- ~1000× more CPU than approach 1.
- Modal additive IS this approach's steady-state impulse response.
  We get 95% of the sound for 0.1% of the CPU.

### 5. Sampling

Out of bounds for the pop/ lane (SCORE.md posture is bottom-up
synthesis, not sampled instruments).

## the tuned-percussion family — one engine, seven cousins

The same modal-bank engine generalises across the whole mallet family
just by swapping partial ratios and decays. Numbers below are from
Fletcher & Rossing (1998), *The Physics of Musical Instruments*,
chapter 19, and Rossing (2000) chapter 4.

| instrument   | partials          | notes                         |
|--------------|-------------------|-------------------------------|
| marimba      | 1 : 4 : 9.2–10    | undercut wood, tube resonator |
| xylophone    | 1 : 3 : 6         | shallower undercut, brighter  |
| vibraphone   | 1 : 4 : 10        | aluminium, T60 = 4–10s, motor |
| glockenspiel | 1 : 2.76 : 5.4    | unundercut steel, mode-2 pitch|
| gamelan saron| 1 : 2.4 : 4.7     | bronze, inharmonic, soft pad  |
| woodblock    | 1 : 1.8 : 2.7 : …| barely pitched, mostly attack |
| kalimba      | 1 : 5.9 : 8.1     | tine (lamella), no resonator  |

Two engine extensions were needed to cover this range:

- **Motor tremolo** — vibraphones have a disc above each resonator
  that periodically opens/closes the tube mouth. Modelled as an LFO
  on the resonator output only (not the dry signal), so the disc
  modulates only the boosted fundamental, not the bar's full
  spectrum. Fields: `tremHz` (1–10 Hz typical), `tremDepth` (0–1).

- **Stick-click noise** — woodblock and xylophone have an audible
  wood-on-wood click that the half-cosine mallet impulse alone
  doesn't generate. A short broadband noise burst (≈ 12 ms
  exponential decay) layered with the modal bank. Field: `noise`
  (0 = off, 0.05 = subtle xylo click, 0.35 = full woodblock rattle).

For the glockenspiel ratios it's worth noting that the bars are NOT
undercut — they're left close to the uniform-beam ratios (1 : 2.76 :
5.40 : 8.93). In the high register where glockenspiel lives, mode 1
is so high that the ear locks onto mode 2 as the perceived pitch.
That's why glockenspiel notation reads as "what you hear" but
mode 1 is actually an octave or so higher than written.

## next steps

- [ ] Compare presets against Rossing's published spectra
  (frequency-vs-time plots from rosewood measurements).
- [ ] Add stick noise (3–5 ms broadband click layered with the modal
  bank — currently the mallet contact is implicit in the half-cosine
  pulse but you don't HEAR the wood-on-wood click yet).
- [ ] Port to fedac/native — drop `generate_marimba_sample()` next to
  `generate_harp_sample()` in `fedac/native/src/audio.c`. The math is
  already pure float; should be a straight translation.
- [ ] Mirror as a `marimba` voice in
  `system/public/aesthetic.computer/lib/sound/synth.mjs` so AC pieces
  can `sound.synth({type: "marimba", ...})` directly.
- [ ] First marimba.np track using the synth in a song context (this
  lane's investigation goal).

## listening references

For ear-calibration when tuning the presets:

- Keiko Abe — *The Wave Impressions* (rosewood, warm, long ring)
- Bogdan Bacanu — *Bach's Cello Suite No. 1* on marimba (mid-range,
  every mode audible in the high register)
- Steve Reich — *Marimba Phase* (lots of attack noise; mallets sound
  almost wooden)
- Robert van Sice — bass marimba on Pius Cheung tracks (mostly tube)
