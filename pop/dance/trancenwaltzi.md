# trancenwaltzi — musical + mastering spec

The chill sequel to **trancenwaltz**. Same `pop/dance/` engine, same
deterministic seed, but rendered in `--mode chill`: a continuous
study-vibes waltz bed — no drops, no screams, no roar, no vocal stem,
flatter dynamics, ~2× the runtime. This is the doc to master from
before it goes to DistroKid → Spotify.

> Status: **RENDER → MASTER**. The current render is **not** a release
> master yet (built without `--master`). Step 1 below fixes that.

---

## 1. Musical facts (authoritative — from `trancenwaltzi.assets/struct.json`)

| Field      | Value |
|------------|-------|
| Meter      | **3/4** (waltz lilt — `--meter 3`, `isWaltz`) |
| Tempo      | **124 BPM** (chill default — slower + more laid-back than the 137.143 trancenwaltz cut; `--bpm` overrides) |
| Bar length | 1.4516 s (3 beats × 60/124) — **plus progressive swing** (off-beats ramp from +0.06 → +0.48 of a beat across the track) |
| Key        | **A minor** (natural; `rootMidi 57` = A3, `scale minor`) |
| Length     | **135 bars ≈ 3:16.2** (`totalSec 196.22`, music starts at 0.25 s — chill has no boot prefix) |
| Mode       | `chill` — continuous minimal bed, no EDM drops/impacts, **wordless** |
| Seed       | `"trancewaltz"` (deterministic; meter-3 default — **not** a custom seed) |

The render is **fully deterministic**: same seed + same flags = byte-identical
audio every time. Differences from trancenwaltz come entirely from `--mode chill`.

### Section map

"drop"/"break" here are **density tiers**, not EDM drops — chill mode has
no impacts. The arc is a gentle swell and settle.

| Section | Bars | Time | Active layers |
|---------|------|------|---------------|
| intro   | 0–12   | 0:00.3 – 0:17.7 | hat, pad, bells |
| break1  | 12–36  | 0:17.7 – 0:52.5 | + kick, lead, mosquito-saws |
| build1  | 36–60  | 0:52.5 – 1:27.4 | + sub |
| drop1   | 60–90  | 1:27.4 – 2:10.9 | + piano (fullest) |
| break2  | 90–102 | 2:10.9 – 2:28.3 | sub drops out |
| drop2   | 102–126| 2:28.3 – 3:03.2 | full again (+ piano, + sub) |
| outro   | 126–135| 3:03.2 – 3:16.2 | kick, hat, pad, bells — winds down |

A **meditation gong** (high struck sine bell → long dissipating ring →
white-noise wave that zippers out) fires every 8 bars throughout.

### Instrument bus & default gains (`recap/bin/trance.mjs`)

No gain overrides in the build config, so these are the levels in the mix:

| Bus    | Gain | Notes |
|--------|------|-------|
| drums (kick/hat) | 0.95 | `--drum-gain` |
| lead / supersaw  | 0.55 | `--lead-gain` |
| pad              | 0.30 | `--pad-gain` |
| sub / bass       | 0.45 | `--bass-gain` |
| piano            | 0.45 | `--piano-gain` (drop1/drop2 only) |
| bells            | 0.32 | `--bells-gain` |

**Chill-specific arrangement** (vs the trancenwaltz narrative cut): no ghost
16ths, no hi-hat flams, no open-hat phrase accents, no `build2`, no whooshes,
no sung "booty" vocal, no greeting vocal, no dramatic openings. Bells are
routed to a dedicated bus, dropped two octaves, slow-swelled (0.45 s attack,
4× T60) and **flangered** into a sub-low harmonic wash. Arp octave cycles
`[0, −1, +1, 0]` every 8 bars. The result is a hypnotic, even, low-arousal
bed — master for *calm*, not impact.

**Chill arp = pure sine, one octave down** (changed 2026-05-18 —
`trance.mjs`, chill-gated): the arpeggio's bright square "intro
expression" (Phase 1) is removed in chill — `waveMix` pinned to sine and
`octShift` pinned to 12 for its whole life, so it's a soft low sine
throughout instead of a bright square that morphs down. The phase logic
still shapes the crescendo / note-elongation / fade. trancenwaltz keeps
the original square→sine morph.

**No startup/shutdown sonification** (changed 2026-05-18 — `trance.mjs`,
chill-gated, trancenwaltz untouched): the boot/startup melody and the
pre-roll "beep→hat" blend are skipped in chill, and the 2.7 s opening
prefix collapses to **0.25 s** so the track just begins on the bed. This
also removed an audible **doubled hi-hat** at the top — the pre-roll hat
layer used to overlap the intro's own in-bar hats from music-entry on.
Shutdown chime + "system dying" tail hats were already chill-gated. The
quiet mid-track "aesthetic dot computer" whisper at 0:58 is **kept** (it's
a brand ID, not a startup beep).

**Chill redesign — full pass** (2026-05-18, `trance.mjs`, all
chill-gated; trancenwaltz byte-identical, untouched):

- **BPM 137.143 → 124** — slower, more laid-back. Fixed bar count, so
  the track lengthens to ~3:16 (was ~2:57).
- **Progressive swing** — off-beat hats / sub / arp odd-steps / lead get
  pushed late by a fraction of a beat that ramps linearly `0.06 → 0.48`
  of a beat across the track. Felt as the groove loosening / "tempo
  breathing"; the deterministic bar grid (struct/sections/seed/video
  alignment) is untouched. (Chosen over true variable-tempo automation —
  that's a deeper engine change, deferred.)
- **Whistle ~0:12** — the staccato "sings-along-with-the-vocal" blip
  becomes a slow, octave-down, ~3.3 s legato sigh (chill has no vocal).
- **Hats + kicks** — per-hit random `pitchFactor` scatter (hat ±0.55,
  kick ±0.16) so the noise content shifts every strike.
- **Supersaw → mosquito triad** — no trance wall; instead a triad of
  high, buzzy, pitch-wandering sawtooth "drone-bird" voices that circle
  the stereo field and sit out ~45 % of bars (zip in and out). New
  helper `fireMosquito`. Event count drops 498 → ~189.
- **Meditation gong** — every 8 bars: `fireGong` = high struck sine
  bell, long dissipating ring, trailing a white-noise wave that swells
  and zippers out. ~16 placements (bells count 541 → ~557).
- **Lead + arp hold/skip** — ~half the notes per bar, ~2× longer holds,
  extra rests; both ride the swing pocket. Lead event count 635 → ~220.
- Screams pre-render now also `!isChill`-gated (mixing was already
  gated — this just drops a wasted cache pass / confusing log line).

---

## 2. Reproduce the audio

Current render (what's on disk, **non-mastered**, loop-declick ending):

```bash
node recap/bin/trance.mjs --mode chill --meter 3 --out trancenwaltzi.mp3
# (this is exactly what pop/dance/bin/build.mjs runs for trancenwaltzi)
```

Current file: `~/Documents/Working Desktop/twi-out/trancenwaltzi.mp3`
(3.31 MB, rendered 2026-05-18 00:50). Build history:
`~/Documents/Working Desktop/builds/trancenwaltzi/bNNN/` (manifests carry
the git sha + note per build). `~/Desktop` copies auto-clean — treat
`~/Documents/Working Desktop/` and the CDN as the durable homes.

### Release cut — re-render WITH `--master`

The on-disk mp3 uses the 6 ms **loop declick**, not a real ending. For a
streaming single you must re-render with `--master` and output `.wav`:

```bash
node recap/bin/trance.mjs --mode chill --meter 3 --master \
  --out ~/Documents/Working\ Desktop/twi-out/trancenwaltzi-MASTER-preBright.wav
```

`--master` restores a proper musical fade-in/out (a real single ending)
and, with a `.wav` path, encodes lossless 16-bit/44.1 kHz. Keep this
**pre-bright** wav — you A/B against it.

---

## 3. Mastering chain

`trance.mjs` renders **dark** by design — a sine-heavy synth mix with
~16 dB of roll-off into the highs. A streaming master needs a brightening
polish pass. This recipe was tuned on **trancenwaltz** (shipped 2026-05-17):

| Move | Freq | Gain |
|------|------|------|
| Mud trim (bell-low / 3/4 boom) | ~190 Hz | **−1 dB** |
| Presence (lead/arp intelligibility) | ~4.2 kHz | **+2 dB** (tw used +2.2) |
| Air shelf | ~8.5 kHz | **+4 dB** (high-shelf) |
| Sparkle shelf | ~12.5 kHz | **+2 dB** (tw used +1.8) |
| Loudness | — | **`loudnorm I=-15 TP=-1.2 LRA=15`** — the chosen final (see below) |

**FINAL output = the `I=-15` master.** Decided 2026-05-18 by ear: on this
flat continuous study bed the quieter / wider-LRA take preserves the calm
better than `I=-14`. The canonical file IS the `I=-15` render:

```bash
# FINAL — this writes the canonical trancenwaltzi-MASTER.wav
ffmpeg -i trancenwaltzi-MASTER-preBright.wav -af \
"equalizer=f=190:t=q:w=1.0:g=-1,\
equalizer=f=4200:t=q:w=1.2:g=2,\
highshelf=f=8500:g=4,\
highshelf=f=12500:g=2,\
loudnorm=I=-15:TP=-1.2:LRA=15" \
-ar 44100 -sample_fmt s16 trancenwaltzi-MASTER.wav
```

A louder `I=-14 / LRA=13` alt is kept as `trancenwaltzi-MASTER-I14.wav`
for reference only — **not** the ship file. Always keep the pre-bright
wav to A/B against; back the 12.5 k sparkle shelf off if the gong /
flangered sub-bells start to whistle.

---

## 4. Ship checklist (tonight)

- [x] Re-render with `--master` → `trancenwaltzi-MASTER-preBright.wav` (124 BPM, full redesign, 3:16)
- [x] Brightening polish → `trancenwaltzi-MASTER.wav` = the **I=-15 final** (I=-14 alt kept for ref)
- [ ] Confirm by ear: −1.2 dBTP ceiling, no clipping, clean fade-out ending, gong not harsh
- [ ] 320 k mp3 for CDN: `assets.aesthetic.computer/pop/trancenwaltzi.mp3`
- [ ] Cover 3000² jpg (regen-in-progress — butterfly-wings fix) →
      `assets.aesthetic.computer/pop/trancenwaltzi.jpg`
- [ ] Canvas already staged: `assets.aesthetic.computer/pop/trancenwaltzi-canvas.mp4`
      (upload to Spotify only **after** the track is live)
- [ ] DistroKid upload (new single, artist "Aesthetic Dot Computer")
- [ ] CDN: `aws s3 cp …` **then** `doctl compute cdn flush
      2ff25b29-db80-48e6-888e-eb8a2464d69b --files pop/<file>`, re-`curl -sI`
      to confirm `content-length` (CDN serves stale ≤ 1 h otherwise)
- [ ] Add the **trancenwaltzi — RELEASED** entry to `pop/RELEASES.md`

Spotify for Artists access for the artist profile is already requested —
see `pop/spotify-for-artists-claim-reply.md`.
