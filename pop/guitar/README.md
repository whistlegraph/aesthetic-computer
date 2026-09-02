# guitar — a physically-modeled strummed six-string

A single-file C engine (like `pop/accordion/c/` and `pop/bell/c/`) whose
sound comes from **six waveguide strings on one bridge, played by one hand**.
The string is an extended Karplus-Strong loop; the hand is a strum pattern
with down- and up-strokes that rake the strings at different speeds, catch
different strings, and damp each string for a moment before letting it go.
The cult track's `guitar()` had the string and not the hand; this is the
hand.

Zero dependencies beyond libm.

```
pop/guitar/
  c/strum.c     the engine
  c/build.sh    cc -O2 -std=c11 -Wall -Wextra -o strum strum.c -lm
  out/          demo bank (gitignored, like every pop out/)
```

## The model

**String** — a fractional-delay Karplus-Strong loop per string:

```
y  = allpass(line[idx])                 first-order allpass → exact pitch
lp = b·y + (1-b)·lp                     the string forgets its highs
line[idx] = g·lp + c·(bridge - lp) + exc
```

- `g` is a per-sample loss from a **T60 per string** (5.5 s on the low E
  down to 2.8 s on the high E, ×1.6 for electric), shortened a little per
  fret, and blended toward a 0.14 s muted T60 by `mute` — which is what a
  palm, a lifting fret hand, and a chuck all are.
- `b` is the loop brightness: wound strings 0.80–0.86, plain 0.90–0.95,
  every fret shaving 0.6 %.
- `exc` is a one-period noise burst **comb-filtered at the pick position**
  (`--pick`, default 18 % of the length — a pluck there cannot feed the
  harmonics with a node there) and **lowpassed by force** (900 Hz at a
  feather, ~8 kHz at full), with a 1.5 ms click for the pick leaving the
  string. It is *added* into whatever the string was already doing.
- **Pick contact**: 3 ms before each pluck the loop loss is multiplied by
  0.55 — the pick is resting on the string. A re-strum catches the old
  note; it does not stack a fresh burst on top of a ringing one.

**Bridge** — every string feeds `c·(mean − own)` back into its loop through
a 2.5 kHz lowpass (`c = 0.03`). The mixing matrix is `g·I + c·(J/n − I)`:
its common mode keeps gain `g`, every differential mode gets `g − c`, so the
coupling is passive by construction and energy leaks from a plucked string
into the others. A strum rings as one body of six, and a single plucked
string makes the others whisper.

**Body (acoustic)** — a dreadnought's three modal peaks plus presence over a
62 % dry path: Helmholtz air 101 Hz (Q 9), top plate 203 Hz (Q 7) and
398 Hz (Q 5.5), a broad 2.3 kHz bump. Left and right carry the modes skewed
±2.5 % so the box has a width; strings are panned low→left, high→right.
DC blocker, 9 kHz rolloff.

**Amp (electric)** — the strings sum at the pickup **then** the amp drives
them: the cult engine's two-stage asymmetric tanh (`tanh(v·k + bias) −
tanh(bias)`, then `tanh(·1.8)/2`) and its dark one-pole (0.16 @ 48 k),
into a 2nd-order 4.2 kHz cab with a 2.4 kHz presence peak. Driving the chord
rather than each string is the point: the grind *between* the notes is the
amp intermodulating, and six separately-driven strings never produce it.

**Hand** — a pattern is one bar, one character per step (16 chars = 16ths,
8 = 8ths, 12 = triplet 8ths), repeating:

| char | stroke |
|---|---|
| `D` / `d` | down, full / soft — rakes low→high across every sounding string |
| `U` / `u` | up, full / soft — rakes high→low across the top `--up` strings only, 0.6× the rake time |
| `x` | chuck — every string muted, then scraped near the bridge |
| `.` | rest |

Rake time is `--rake` at full force and *shortens with force* (a hard strum
is a fast strum); the wrist accelerates through the strings (`t ∝ k^0.85`);
later strings in a rake are hit slightly lighter; the top string of an
up-stroke slightly harder. `--human` puts a seeded ±4 ms / ±8 % / ±15 %
wobble on timing, force and rake.

**Fret hand** — `--chord "Bm|D|G|Em"` cycles one chord per bar. 30 ms
before the bar line the strings whose fret changes are choked; on the bar
line they retune (the ringing delay line is simply read at the new length,
which is what a hammer-on is); and if a wound string moved two or more
frets, a 70 ms band of noise around 2.2–3.1 kHz squeaks.

The default pattern is the cult track's `STRUM` table written in 16ths:
`D..d..u.u.D..u..` (beats 0, 0.75, 1.5, 2.0, 2.5, 3.25).

## CLI

```bash
./build.sh
./strum --chord Bm --out bm.wav
./strum --chord "Bm|D|G|Em" --bars 4 --electric --out cycle.wav
./strum --chord 47,54,59,62 --pattern "D.D.uu.D..u." --electric --out cult.wav
./strum --chord Bm --mute palm --pattern "DdDdDdDdDdDdDdDd" --electric --out chug.wav
./strum --chord "Bm9|Gmaj7|Em9|F#7sus4" --bars 4 --pattern "..D...u.d.U..u.." --rake 24 --force 0.55 --out flower.wav
```

| flag | meaning | default |
|---|---|---|
| `--chord C` | a name (`Bm`, `F#m7`, `Cadd9`, `Dsus4` …) or a midi list (`47,54,59,62`); `\|`-separated list cycles one per bar | `Bm` |
| `--pattern P` | one bar of `D d U u x .` | `D..d..u.u.D..u..` |
| `--bpm B` | tempo | 120 |
| `--bars N` | bars to play | 1 |
| `--electric` / `--acoustic` | amp chain vs body | acoustic |
| `--drive 0..1` | amp drive (electric) | 0.7 |
| `--rake MS` | full-force down-stroke, low to high | 16 |
| `--up N` | strings an up-stroke catches | 4 |
| `--force 0..1` | hand force | 0.8 |
| `--pick 0..1` | pick position along the string | 0.18 |
| `--mute palm\|open\|0..1` | fret-hand damping | open |
| `--damp 0..1` | shorter string sustain | 0.15 |
| `--human 0..1` | seeded timing/force/rake wobble | 0.5 |
| `--tail S` | ring past the last bar (the hand chokes the strings 220 ms before the end) | 1.0 |
| `--sr HZ` | sample rate | 48000 |
| `--seed N` | RNG seed | 1 |
| `--out FILE` | float32 stereo WAV, peak-normalized to 0.9 | required |

Chord names: 31 open-position shapes are tabled (E Em E7 Em7 A Am A7 Am7
Asus2 Asus4 D Dm D7 Dmaj7 Dsus2 Dsus4 G G7 C Cmaj7 Cadd9 C7 F Fmaj7 Bm B B7
Bm7 F#m F# F#m7); anything else becomes a barre — E-shape for roots E…B,
A-shape for C…D# — with suffixes `m 7 m7 maj7 sus2 sus4 7sus4 add9 9 m9 5`.
A midi list is placed top note first, each on the highest free string that
reaches it (the lowest fret), so `47,54,59,62` is `x A2+2 D3+4 G3+4 B3+3 x`
and the up-stroke's "top strings" are the right ones. The voicing is
printed on stderr.

Deterministic: one seeded xorshift, drawn in a fixed order. Same flags,
same bytes. A 16-bar electric render takes ~0.3 s.

## Demos (`out/`)

```bash
for ch in Bm D G Em; do
  ./strum --chord $ch --out ../out/$(echo $ch | tr A-Z a-z)-acoustic.wav
  ./strum --chord $ch --electric --out ../out/$(echo $ch | tr A-Z a-z)-electric.wav
done                                                          # 1 bar @ 120 = 3.00 s each
./strum --chord "Bm|D|G|Em" --bars 4 --out ../out/cycle-acoustic.wav
./strum --chord "Bm|D|G|Em" --bars 4 --electric --out ../out/cycle-electric.wav
./strum --chord 47,54,59,62 --pattern "D.D.uu.D..u." --electric --out ../out/cult-voicing-electric.wav
./strum --chord Bm --mute palm --pattern "DdDdDdDdDdDdDdDd" --electric --out ../out/bm-palm-electric.wav
./strum --chord "Bm9|Gmaj7|Em9|F#7sus4" --bars 4 --pattern "..D...u.d.U..u.." --rake 24 --force 0.55 --out ../out/flower-acoustic.wav
```

## Notes / honest limits

- The string is a lossy loop with a one-pole, not a stiff-string model: no
  inharmonicity, so the wound strings lack the slight "piano" stretch of
  real bronze, and there is no longitudinal / two-polarization coupling —
  the slow beating of a real open string is absent.
- The body is a fixed modal EQ, not a measured impulse response; swap the
  `body_init` table to chase a specific box.
- The pick is a noise burst: it carries position and force but not the
  pick's own stiffness, so a felt pick and a steel one are the same pick.
- The fret hand changes chords only on bar lines, and the squeak is a
  band of noise with no relation to which fret it slid from.
- Output is peak-normalized like the accordion, which means a soft render
  and a hard one land at the same peak — the dynamics live *between* the
  strokes, not between files. Score the level at the call site.
- `note`/WAV conventions follow `pop/bell/c/bell.c`.
