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
| Tempo      | **GAS-PEDAL** (chill): 150 hold → press ↗198 (WOOOOF) → coast ↘156 → bigger press ↗212 (faster beat) → ease → **dying out ↘110** into the fade. Distinct accel presses w/ eased release, NOT a sine coaster. Deterministic piecewise fn of bar fraction. `--bpm` sets the 150 base |
| Bar length | shrinks/grows per the gas-pedal curve (3 beats × 60/bpm(bar)); pure function of bar index |
| Key        | **A minor** (natural; `rootMidi 57` = A3, `scale minor`). Chord progression: chill walks an **8-progression pool, 3-bar chords** → loops far less + travels more harmonically |
| Length     | **200 bars ≈ 3:43.6** (`totalSec 223.62`, music starts at 0.25 s). Continuous bed → musical `--master` fade end, **no dead air** (tempo also winds down into the fade — cohesive ending) |
| Mode       | `chill` — continuous minimal bed, no EDM drops/impacts, **wordless** |
| Seed       | `"trancewaltz"` (deterministic; meter-3 default — **not** a custom seed) |

The render is **fully deterministic**: same seed + same flags = byte-identical
audio every time. Differences from trancenwaltz come entirely from `--mode chill`.

### Section map

"drop"/"break" here are **density tiers**, not EDM drops — chill mode has
no impacts. Section seconds are **non-uniform** (each bar is shorter than
the last — the accelerando); the `(Xs / N bars)` column shows it speeding up.

| Section | Bars | Time (Xs / N bars) | Active layers |
|---------|------|---------------------|---------------|
| intro  | 0–16    | 0:00.2 – 0:16.5 (16.2s/16) | hat, pad, bells — **fast cacophonous arp**, **crunch bass**, kittens |
| break1 | 16–52   | 0:16.5 – 0:56.2 (39.7s/36) | + kick, lead, mosquito-saws; bass → ocean lo-fi → swung; coaster + kittens (bars 6–22) |
| build1 | 52–88   | 0:56.2 – 1:35.9 (39.7s/36) | + sub (template sub takes over the bass) |
| drop1  | 88–132  | 1:35.9 – 2:22.3 (46.3s/44) | + piano (fullest) |
| break2 | 132–150 | 2:22.3 – 2:40.6 (18.3s/18) | sub drops out |
| drop2  | 150–186 | 2:40.6 – 3:16.2 (35.6s/36) | full again (+ piano, + sub) |
| outro  | 186–200 | 3:16.2 – 3:29.7 (13.5s/14) | kick, hat, pad, bells — winds down to the fade |

The track genuinely accelerates (each later bar shorter), with a hard
roller-coaster crest over bars 6–22.

> ⚠ **Video caveat:** `struct.json`'s single `bpm` field records only the
> **base** (150) — it does not express the per-bar ramp. The audio single
> is correct; but `cover-video.mjs` beat-grid alignment would drift on an
> accelerating track. Fine for the audio release tonight; the canvas/video
> needs the ramp taught to it before any trancenwaltzi *video* ships.

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

**Chill v4 — gas-pedal + cat choir + harmonic travel** (2026-05-18,
latest; supersedes the v2/v3 tempo + cat notes): tempo reworked from a
sine roller-coaster to a **gas-pedal** (presses ↗198 / ↗212 with eased
release, then dying out ↘110 — see Tempo row; ~3:43.6). Crunch bass is
now a higher **WOOOOF womp** (octave up, downward pitch-sweep, no
sub-octave — fixes the "super deep kick"). Normal kick walks a fixed
**A/B/C pitch pattern** + occasional **double/triple bursts**, all
humanized. Meows became a **long, granular-time-stretched, autotuned,
harmonized cat choir** (minor-triad ratios) spanning the press bars +
short kitten accents (`mixCatDrone` helper). Chord progression: chill
walks an **8-progression pool with 3-bar chords** so it loops far less
and travels more. Still all chill-gated; trancenwaltz byte-identical.

**Chill v3 — bass arc + length + kittens** (2026-05-18, latest; builds
on v2): chill sections lengthened **135 → 200 bars** so the fast
accelerando still lands a **3:29.7** single (3–4 min target). New
3-stage **bass arc** filling intro+break1 (which had no template sub):
punchy **bit-crushed crunch** (square stack + noise spit, first ~8 s) →
crossfade to **deep ocean lo-fi** sub (low sine + sub-octave + tape wow
+ faint hiss, by ~18 s) → **levels out on a swing** (off-beats pushed
late); template sub takes over at build1. The roller-coaster was made
**radical** (bars 6–22, +45 % crest then ease — shows the algorithmic
stretch). A litter of **9 high kitten mews** scattered through the
coaster window. Verified **no dead air** (continuous bed → musical
`--master` fade end). `fireChillArcBass` helper added. Still all
chill-gated; trancenwaltz byte-identical.

**Chill v2 — accelerando** (2026-05-18, later same day, supersedes the
BPM/swing bullets below): it's a dance track, so the laid-back 124 +
off-beat-delay swing were scrapped. Now: **base BPM 150 accelerating
linearly to 188** (`BPM_END`) — a true deterministic per-bar tempo ramp;
the track *actually speeds up* (length ~2:25). The **intro arp** is fast,
dense and **cacophonous** (2× steps, short notes, +detuned tritone-octave
clash) then settles into the held/sparse sine. The **"aesthetic.computer"
stamp** is stretched slow + low in chill (`PITCH 0.55`). Off-beat swing
offset retired (`swingSec = 0`). Still all chill-gated; trancenwaltz
byte-identical.

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
- [x] Cover 3000² jpg — **`~/Documents/Working Desktop/gens/trancenwaltzi-cover-3000.jpg`**
      (also mirrored to `~/Desktop/`). Source: outro illustration **v31**
      (`gens/trancenwaltzi-sections/outro/gens/v31.png`, native 1024²
      upscaled → 3000²; square, no crop needed unlike the portrait
      trancenwaltz cut). Concept: **in-aisle Trader Joe's party / "welcome
      to the talk show, come chill with us"** — jeffrey arms-wide hosting
      to camera, the whole pixie crew vibing through the aisle, upload
      sparkles rising, his green Neo + whistlegraph (both arms) low-front,
      pixie laptops PALS-only. No readable brand wordmark (shot indoors —
      avoids the trademark issue the exterior-storefront takes had).
      Built via `marketing/bin/gen-promo.mjs` from
      `gens/trancenwaltzi-sections/outro/cover-prompt.txt` (prior prompt =
      `cover-prompt.prev.txt`). Still **TODO**: upload to
      `assets.aesthetic.computer/pop/trancenwaltzi.jpg`.
      Note: the cover **video** (`build.mjs` trancenwaltzi square format)
      is still on the old `v16` set across all 8 sections — switching it to
      this concept means regenerating all 8, a separate larger job (cover
      still vs. video are independent here, same as trancenwaltz).
- [ ] Canvas already staged: `assets.aesthetic.computer/pop/trancenwaltzi-canvas.mp4`
      (upload to Spotify only **after** the track is live)
- [ ] DistroKid upload (new single, artist "Aesthetic Dot Computer")
- [ ] CDN: `aws s3 cp …` **then** `doctl compute cdn flush
      2ff25b29-db80-48e6-888e-eb8a2464d69b --files pop/<file>`, re-`curl -sI`
      to confirm `content-length` (CDN serves stale ≤ 1 h otherwise)
- [ ] Add the **trancenwaltzi — RELEASED** entry to `pop/RELEASES.md`

Spotify for Artists access for the artist profile is already requested —
see `pop/spotify-for-artists-claim-reply.md`.
