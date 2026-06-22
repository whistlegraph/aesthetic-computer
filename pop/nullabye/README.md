# nullabye

A lullaby with no instruments in it — every sound is carved out of
cancelled pink noise with EQ automation. Part of the *pixsies* body.

## The technique

Study after Andy Brewer's ["this song has no instruments in it"](https://youtu.be/_Rk-hmIMv6I):

1. Duplicate a pink-noise track and invert the phase of the copy.
   Summed, they null to **perfect silence**.
2. Put an EQ on the copy. Anywhere the EQ deviates from flat, the
   cancellation breaks — what you hear is the *difference signal*.
3. A narrow peaking bell at a note frequency therefore becomes a
   breathy pitched voice pulled out of the hiss. Automate 24 bells
   and you have a 24-voice "noise-sine" polysynth.

Two properties make this more than a parlor trick:

- **Pink noise + constant-Q bells = equal voice energy at any pitch**
  (pink is equal energy per octave; a constant-Q band covers a constant
  fraction of an octave). Bass notes and whistle notes come out at the
  same loudness for free.
- **High-Q bells ring** (ring time ≈ Q/πf), so the filters supply
  natural attack/release tails without any amplitude envelope on the
  audio path — the "instrument" has built-in articulation.

We honor the original's constraint: the renderer allocates from a pool
of **at most 24 EQ points** and throws if the score ever wants a 25th.
nullabye uses 15, peaking at 10 sounding simultaneously.

## Run

```bash
node pop/nullabye/bin/render-nullabye.mjs            # → out/nullabye.mp3
node pop/nullabye/bin/render-nullabye.mjs --proof    # flat EQ ⇒ bit-exact silence
```

`--proof` renders with every band forced flat and asserts the output is
bit-exact digital zero — the cancellation is real, not just quiet.

## nuellaby — the complexity-arch cut

Second cut in the lane (`bin/render-nuellaby.mjs` → `out/nuellaby.mp3`,
exactly 2:00 — 38 bars at 76 BPM is 120.0 s on the nose): the EQ rack
**doubles 2 → 4 → 8 → 16 → 32 → 64 → 128, then folds back down
64 → 32 → 16 → 8 → 4 → 2 and empties** — band count as the
musical-complexity arc itself. Up in 4-bar chord cycles (2 bars for the
sparse openers), down in quick 2-bar halvings (arch form; the ×128 bloom
is the keystone, with a 12-partial harmonic choir + 21-band diatonic halo).

The spectrum is carved into zones, each owned by its own layers:
sub (pitch-dropping 110→42 Hz kick, bass roots) · bass (drones, tom
fills, floor pad) · low-mid (chord pads, detuned doubles) · mid (lead
whistle, harmony, choir) · upper-mid (octave lead, arps, snare-shh) ·
presence (sparkles, rim clicks, risers) · air (closed/open hats, shaker
16ths, ticks, veils).

New engine idea: **lanes** — persistent monophonic EQ points that
consecutive notes reuse (the bell drags to a new frequency, clipping the
old ring), so each stage's band count is the honest number of points on
the rack. A per-stage cap audit throws if any stage exceeds its power of
two; measured arch: `2/2 4/4 8/8 16/16 30/32 51/64 92/128 49 29 15 8 4 2`.
`--proof` works here too (92 lanes flat ⇒ bit-exact silence).

## The song (nullabye)

76 BPM, 4/4, C major, 24 bars (≈ 1:19). Chord cycle C / Am9 / Fmaj9 / Gadd9.

- **breaths** (bars 0–3) — wide low-Q bells swell in and out of the
  null so the trick is audible before the song starts.
- **lullaby-a** (4–11) — Q 28 chord pads, Q 55 whistle lead (±0.12%
  L/R detune), heartbeat kick (Q 4.5 bell at 54 Hz) from bar 8.
- **lullaby-b** (12–19) — adds half-note bass roots, 8.2 kHz hat
  ticks, Q 70 high sparkles on offbeats.
- **veil-lifts** (20–23) — one wide bell sweeps 250 Hz → 6 kHz letting
  the raw noise wash through, a last lone whistle, then the EQ goes
  flat and the song ends in literal silence.

## nullabata — the sonata cut

Third cut in the lane (`bin/render-nullabata.mjs` → `out/nullabata.mp3`,
~4:21): *nullaby + sonata*. **Four movements run attacca — no silence
between them** — bound by one lullaby motif (semitone offsets from a
per-movement voice tonic) transformed in each:

```
I.   Andante   C maj    72 BPM   18 bars   the theme, lyrical whistle + warm pads
II.  Scherzo   A min   132 BPM   32 bars   theme shattered into a fast arp/twinkle cloud
III. Adagio    F maj    56 BPM   13 bars   theme sung slow over sustained pads + drones
IV.  Finale    C maj    84 BPM   30 bars   theme recapitulated: 2→bloom→2 accumulation
```

**Clarity is a Q decision.** In this engine every voice is a peaking-EQ
bell on noise: a *narrow* (high-Q) bell rings like a breathy tone, a *wide*
(low-Q) bell passes a wide band of raw noise (hiss). nullabata runs the
engine at its **tonal extreme** — high Q on every pitched voice (lead 110,
pads 55, drones 45, choir/halo 50+), **no wide "veil" wash** (the single
biggest static source in the lane), and percussion kept narrow (no shaker /
open-hat; hats are narrow pitched ticks). Master drops the treble boost and
adds a 15 kHz lowpass; energy above 12 kHz sits 36 dB below the full band.

Otherwise reuses the engine wholesale — lanes, the serial-dB
disjoint-frequency discipline, sorted mono events. The loudness ride is
**per-second, movement-aware** (each movement targets its own dBFS curve)
so the four very different tempos and densities land evenly; integrated
**−19.1 LUFS**. The Finale runs a compact accumulation arch (47-point
roster, 2 → bloom → 2). `--proof` passes with all 47 lanes flat (bit-exact
silence); the `--bake` → `c/run-c.mjs` C path renders the same score.

```bash
node pop/nullabye/bin/render-nullabata.mjs            # → out/nullabata.mp3
node pop/nullabye/bin/render-nullabata.mjs --proof    # flat ⇒ bit-exact silence
node pop/nullabye/bin/render-nullabata.mjs --bake out/nullabata.score.txt
```
