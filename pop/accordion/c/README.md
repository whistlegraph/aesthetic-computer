# accordion — an air-powered physically-modeled free reed

A single-file C engine (like `pop/bell/c/`) whose sound comes from a **free
reed driven into self-oscillation by bellows pressure** — not an oscillator
with an envelope on it. Below a threshold pressure the reed is silent; above
it, airflow feeds the swing each cycle and the note *grows out of turbulence
noise*. The attack breathes because the air takes time to build **and** the
reed takes time to speak.

Zero dependencies beyond libm.

```
pop/accordion/
  c/accordion.c   the engine
  c/build.sh      cc -O2 -std=c11 -Wall -Wextra -o accordion accordion.c -lm
  out/            demo renders
```

## The model

**Reed** — a van-der-Pol-type nonlinear oscillator (the standard reduction of
the Fletcher/Rossing free-reed treatment):

```
x'' = -w(p)^2 (x - x_off(p))  +  w·drive·( p·(1 - x²/X(p)²) - ζ₀ )·x'  + noise(p)
```

- The **airflow damping term** goes negative when `p` exceeds `ζ₀` — energy
  flows *into* the swing and the oscillation grows exponentially from the
  turbulence seed until the amplitude-dependent factor saturates it. Growth
  rate scales with pressure: soft notes speak slower (measured ~145 ms to 90 %
  at `--vel 0.3`, ~125 ms at 0.8, on top of the bellows RC).
- The intrinsic loss `ζ₀` is kept **outside** the amplitude factor so it is
  always dissipative — folding it into one van-der-Pol `mu` explodes at
  release, when `mu < 0` and `|x| > X` flips the saturation into anti-damping.
- **Saturation amplitude `X(p)` grows with pressure**, so bellows dynamics are
  heard in level, not just in the flow factor: the tremor and the swell
  actually modulate loudness.
- `x_off(p)`: static pressure deflection — pumping harder skews the pulse and
  feeds the even harmonics.
- `w(p)`: reeds **flatten under pressure** — ~10 cents measured at full
  pumping (a ~7-cent programmed detent plus a real nonlinear-damping shift),
  which also turns the tremor into a faint coupled vibrato.

**Flow gate (the buzz)** — the radiated sound is the derivative of the volume
flow through the slot, `U = A(x)·√p`. The aperture `A` has a kink at the frame
plane and is *asymmetric*: above the plane the gap opens freely; below it the
tongue sits inside the slot and the gap barely reopens. One real closure per
cycle ⇒ dense odd+even comb with the fundamental on top (a symmetric `|x|`
fold puts the octave above the fundamental — measured, wrong, fixed).
Measured sustain spectra: 25–32 harmonics above −50 dB at A4, comb to Nyquist
at A2; bass reeds carry pitch on the comb (A2 fundamental ~4 dB under h2, like
the real thing). Output is leveled by `1/w0` so pitch doesn't change loudness.

**Bellows** — ONE pressure signal shared by every reed in the render:

- attack RC (air builds behind the pallets; pull opens slower than push),
- hand tremor: a drifting 4–6 Hz LFO plus a slow random-walk unsteadiness,
  depth = `--tremor` (measured ~12 % RMS level wobble at 0.8),
- shapes: `push` (firm, small sag), `pull` (gentle open, slight crescendo),
  `swell` (hairpin < > peaking ~55 % in), `shake` (fast ±strokes ~3.6 Hz —
  pressure dwells near zero at each turnaround so the reeds re-speak every
  stroke: measured 7.3 Hz throb, level dipping to ~0.2 of peak),
- release: the pallet closes, pressure bleeds off, and the residual air leaves
  as a little filtered-noise **exhale**.

**Musette** — `--voices 2|3` reeds per note at `±--detune` cents (2 = on +
sharp, 3 = flat/on/sharp), each with small random spreads in drive, gate and
flattening so they start and drift independently. A 12 ¢ pair at A4 measures
a 2.8–3 Hz amplitude beat at ~27 % depth — the wet shimmer.

**Body** — four fixed 2-pole resonances (box thump 215 Hz, wooden mid 540,
grille presence 1350, air 2900) over a 55 % dry path, DC blocker, grille-cloth
rolloff at 6.5 kHz. Plus a key-click + valve-thump transient at note start and
pressure-scaled breath noise throughout.

**Chord** — `--chord "A3,C#4,E4"` renders every note off the *same* bellows:
one attack, one tremor, one release for the whole handful of reeds. That
shared air is what makes it an accordion and not three reeds in a row.

Integration: semi-implicit Euler at 4× oversampling.

## CLI

```bash
./build.sh
./accordion --note A4 --dur 4 --out note.wav
./accordion --note D4 --voices 3 --detune 14 --bellows pull --out musette.wav
./accordion --chord "A3,C#4,E4" --bellows swell --dur 7 --out chord.wav
./accordion --note G4 --voices 2 --bellows shake --dur 5 --out shake.wav
```

| flag | meaning | default |
|---|---|---|
| `--note N` | note name (`A4`, `C#3`, `Db5`) or bare Hz | `A4` |
| `--hz F` | fundamental in Hz (overrides `--note`) | — |
| `--chord "A3,C#4,E4"` | several notes sharing one bellows | — |
| `--dur S` | note length, seconds (render adds a 0.5 s tail) | 4 |
| `--vel 0..1` | bellows force | 0.8 |
| `--voices 1\|2\|3` | reeds per note (musette) | 2 |
| `--detune C` | musette detune, cents | 12 |
| `--bellows` | `push` \| `pull` \| `swell` \| `shake` | push |
| `--tremor 0..1` | hand-tremor depth | 0.35 |
| `--sr HZ` | sample rate | 48000 |
| `--seed N` | RNG seed (reed spreads, tremor phase) | 1 |
| `--out FILE` | float32 stereo WAV | required |

## Demos (`out/`)

```bash
./accordion --note A4  --voices 1 --tremor 0.35 --dur 4 --out ../out/single-a4.wav
./accordion --note D4  --voices 3 --detune 14 --tremor 0.4 --bellows pull --dur 5 --out ../out/musette-d4.wav
./accordion --chord "A3,C#4,E4" --voices 2 --bellows swell --tremor 0.45 --dur 7 --out ../out/swell-chord-a.wav
./accordion --note G4  --voices 2 --bellows shake --dur 5 --out ../out/shake-g4.wav
```

## Notes / honest limits

- The reed is a *reduced* model (one bending mode + phenomenological flow
  gate), not a full aeroelastic simulation: torsional modes, reed-frame
  contact chatter and the two-reed push/pull switch of a real box are absent —
  push/pull here differ in gesture (attack, sag/crescendo), not in reed pair.
- Body resonances are a fixed EQ, not a measured accordion impulse response;
  swap the `body_init` table to chase a specific box.
- The flow-gate kink is not band-limited, so very high notes fold a little HF
  hash past Nyquist even at 4× reed oversampling (inaudible under the grille
  rolloff in practice, but it's there).
- `note_to_freq` and the WAV writer follow `pop/bell/c/bell.c` conventions.
