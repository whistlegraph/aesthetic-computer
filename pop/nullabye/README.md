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

## sineabye — the oscillator cut

`bin/render-sineabye.mjs` recasts the lullaby around sine voices, with a very
quiet bed of independently filtered noise hats and soft percussion. It uses no
samples, distortion, cancellation, or EQ-as-instrument technique.
It expands the original theme to exactly **2:00** (38 bars at 76 BPM), moving
through an immediate quiet opening verse, two increasingly full statements,
a consonant F-major bridge, homecoming, and a melody-only ending. Even the
heartbeat is a short downward-gliding sine; sparse modal gongs mark the form.

```bash
node pop/nullabye/bin/render-sineabye.mjs
# → out/sineabye.mp3 + out/sineabye.struct.json
```

### spatial-sineabye — gravitational room cut

`c/spatial-sineabye.c` is a small acoustic game engine: every musical voice is
a persistent body in a virtual room, and its live acoustic energy exerts
gravity plus a tangential “groove” force on a damped listener body. The moving
listener position compiles to distance, azimuth, elevation, Doppler, ear gain,
and room reflections. The same C simulation rasterizes an MP4 showing sources,
energy, attraction lines, listener position, and head direction in perspective
3D. After 1:18 its camera withdraws from the listener and eventually leaves the
whole room floating far away as a small source constellation.
Its listening law is intentionally selective: distance falls steeply after a
few room units, so wandering actively remixes the piece. Faint grey tethers in
the video show physical source distance; colored halos and lines show the
post-distance energy that actually reaches the stereo listener.
The opening starts near 30 rotations per second, completing 96 turns as it
decelerates to a stop. Sustained resonators make that HRTF/proximity motion fuse
into a spatial hum, but the track no longer waits inside atmosphere: the
lullaby melody, quarter-note low pulse, and alternating air answers begin in
bar one. From 0:50–1:18 the complete source constellation eases through two
more physical rotations, including an eight-turn centrifuge from 1:02–1:10.
The audio and gravity use the rotating coordinates, so these passages produce
real proximity, Doppler, and stereo movement rather than a camera spin.

In the first-person receiver view, emitters are twelve-slice spectrographic
volumes instead of small source dots. Frequency rises vertically, live band
energy controls each cross-section, and longitudinal ribs join the slices into
one body. Six historical post-HRTF measurements launch expanding three-plane
shells from each body's position at the moment its sound was emitted; captured
per-source waveform samples deform those shells instead of leaving them as
smooth decorative rings. The cockpit windshield is a 55 ms perspective surface
of the exact final stereo master decoded back into C. Source-colored waveform
particles strike it at the emitter's measured L/R position and spectral height,
producing impact ripples on the calculated sound plane.
The echo and air sources are follower bodies: they occupy the listener's
position 2.2 and 5.5 seconds in the past, respectively. Their sound and gravity
therefore chase the player; a fading breadcrumb in the video exposes the same
six-second positional memory.

```bash
sh pop/nullabye/c/build-spatial.sh
cd pop/nullabye/c
./spatial-sineabye --wav ../out/spatial-sineabye.wav \
  --mp3 ../out/spatial-sineabye.mp3 \
  --video ../out/spatial-sineabye.mp4 \
  --spatial-wet 0.58 --noise-level 0.55
```

`--spatial-wet 0..1` interpolates every voice between a restrained fixed-pan,
constant-distance studio mix and the complete moving-listener model (including
distance, azimuth, Doppler, and room motion). The pop default is 0.58; use 1 for
the fully experimental observation or 0 for the stable dry reference. The wet
bus is implemented by `c/ac_hrtf.h`, an allocation-free procedural binaural
core designed to compile unchanged to WebAssembly. It models fractional ITD,
far-ear head shadow, distance and elevation-dependent pinna notches. This is
portable directional DSP, not yet a personalized measured HRIR dataset.

`--noise-level 0.05..1` scales the hat, nose, and air voices without permitting
an unnaturally silent floor. Room, globe, and lattice renders default to the
composed 76 BPM / 2:00 form; cosmos stays at 104 BPM unless `--bpm` overrides it.

### Jeffrey choir release candidate

`bin/render-spatial-jeffrey.mjs` layers the existing project-owned, pitch-locked
Jeffrey `mm/oo/oh/ah/eh` takes over a room render. It leaves the opening spin
free of Jeffrey's choir, then stretches compact harmonies around the takes'
natural B2 register. During the 1:02–1:10 super-spin, the choir's orbit follows the exact
eight-turn C trajectory. The renderer makes no network or voice-generation API
calls and records every source and placement in a provenance sidecar.

```bash
node pop/nullabye/bin/render-spatial-jeffrey.mjs
# → out/review/spatial-room-spin-jeffrey-MASTER.wav (48 kHz / 24-bit)
# → out/review/spatial-room-spin-jeffrey-MASTER.mp3 (listening copy)

# Reconnect the accepted master to the engine-generated picture:
cd pop/nullabye/c
./spatial-sineabye --wav ../out/review/spatial-room-spin-clean.wav \
  --video ../out/review/spatial-room-spin-jeffrey.mp4 \
  --video-audio ../out/review/spatial-room-spin-jeffrey-MASTER.wav \
  --spatial-wet 0.58 --noise-level 0.55
```

Spatial-engine listening/video snapshot:

- `release/spatial-sineabye/spatial-sineabye.mp3` — 320 kbps listening copy.
- `release/spatial-sineabye/spatial-sineabye.mp4` — 720×720, 24 fps,
  Git-safe H.264 delivery encode with the calculated stereo windshield.

### Special Sign — locked release master

The release-length work grown from this engine is **Special Sign**. Its accepted
cut begins at source bar 6, runs 1:41.375, and keeps the lead-first assembly,
all-body hum, Jeffrey phoneme choir, listener-relative gravity/HRTF motion, and
the eight-turn super-spin. The final C-major cadence decelerates the spatial
world to rest without changing musical tempo or adding a global fade.

The 2026-07-24 release lock is
`release/special-sign/special-sign-MASTER.wav`: 48 kHz/24-bit stereo, −15.0
LUFS, 7.1 LU range, and −1.7 dBFS true peak. Its `diamond` master is linear—no
saturation, recursive audio feedback, or added master echo. A side-only parallel
return from the 68%-wet listener engine sits at 0.20 and rises smoothly to 0.85
through the eight-turn super-spin; that rotation is 4.1 dB stronger in the side
field than the previous master while remaining mono-safe. The paired 320 kbps
MP3 carries release tags and the 3000-square radial-score cover. The same 1,967
visible score events print as a synchronized 2160-square MP4: twelve physical
sound-body lanes, Jeffrey's vowel lane, chords, spatial rotation, kick gravity,
and the locked master's measured dynamic arc move through one fixed playhead.
The paired 720-square first-person 3D spatial graph shows the physical bodies,
wave shells, listener path, gravity field, and aligned mastered stereo receiver.
See `release/special-sign/README.md` and `release.json` for the exact files,
checksums, and QC receipt.

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
