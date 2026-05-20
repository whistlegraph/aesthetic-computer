# Pop Releases

Progress tracker for finished `pop/` singles — what's shipped, what's in
flight, and where it lives. See [SCORE.md](SCORE.md) for the mill mission.

Status legend: **RELEASED** · **MASTERING** · **RENDER** · **WIP** · **IDEA**

---

## trancenwaltz — RELEASED

- **Lane:** `pop/dance/` · dark/emo/extreme war-arc trance-waltz, ~1:26
- **Released:** 2026-05-17 via DistroKid
- **Canonical:** https://distrokid.com/dashboard/album/?albumuuid=8FF25085-8F58-4A3A-986A52A21D638805
- **CDN assets** (canonical, public — `system/public/assets/pop/`, gitignored, synced via `npm run assets:sync:up`):
  - audio · https://assets.aesthetic.computer/pop/trancenwaltz.mp3 (320 k mp3 of the bright master, 86.62 s)
  - cover · https://assets.aesthetic.computer/pop/trancenwaltz.jpg (3000², outro art)
  - canvas · https://assets.aesthetic.computer/pop/trancenwaltz-canvas.mp4 (Spotify Canvas — 1080×1920, 6 s, **silent**, seamless loop, **chrome-free**, slit-scan glitch montage of all 8 section illys)
  - Reconstructable byte-faithfully any time: `trance.mjs --meter 3 --vocal-stem pop/dance/out/trance-hook-layered.mp3 --master` (deterministic seed `trancewaltz`) → brightening polish (high-shelf 8.5 k +4, presence 4.2 k +2.2, sparkle 12.5 k +1.8, 190 Hz −1, `loudnorm I=-14 TP=-1.2 LRA=13`) → 320 k mp3; cover = outro v15 `-gravity North -crop 1024x1024+0+96 → 3000²`. (Desktop copies are auto-cleaned — see [[feedback_desktop_autocleaned]]; CDN + repo `assets/pop/` are the durable home.)
- **Master:** `~/Desktop/trancenwaltz-MASTER.wav` (= `-FINAL-distrokid.wav`)
  — 16-bit/44.1 kHz stereo WAV, 86.62 s. **Bright master:** −13.5 LUFS,
  −1.2 dBTP, LRA 6.7, +5 dB air vs the original dark cut. Pre-bright
  (−15.2 LUFS, LRA 10.5) kept at `~/Desktop/trancenwaltz-MASTER-preBright.wav`.
  - Proper fade-out single ending (not the loop declick); no typing
    keyclicks (startup melody kept); randomly-pitched intro/outro hats;
    quiet "aesthetic dot computer" stamp at 58 s on the build2 climb.
- **Cover:** `~/Desktop/trancenwaltz-cover-3000.jpg` — 3000×3000, the
  **outro** illustration (last frame of the contact sheet). Alternate
  upload-concept cover: `~/Desktop/trancenwaltz-cover-upload.jpg` (v12 —
  consciousness-upload, glitched whistlegraph-butterfly tear, worried
  adult hacker pixies, glowing PALS laptops, red/white throat light).
- **Cover video (loop):** `~/Desktop/tw-vertical-iter.mp4` — 1080×1920,
  perfect-loop (audio declick + video cross-dissolve), de-striped
  slit/gnarl warp, section-gated decode karaoke, forced-aligned lyrics.
- **Source:** audio `recap/bin/trance.mjs --meter 3 --vocal-stem … --master`
  (deterministic seed `"trancewaltz"`); video `pop/dance/bin/cover-video.mjs`;
  build orchestrator `pop/dance/bin/build.mjs`.

### Spotify for Artists assets ("aesthetic computer")

Managed in **artists.spotify.com**, NOT DistroKid (DistroKid only sets
the release cover + audio). Claim the profile once trancenwaltz is live;
DistroKid has a "request Spotify for Artists" shortcut for new artists.

- avatar · https://assets.aesthetic.computer/pop/ac-spotify-avatar.jpg —
  3000², hyper-real **half-kiki/half-bouba green crystal** with digital
  glitch-cracks on near-black, a BIG PLUMP PALS mark bursting out
  through the cracks; pure black + Spotify-green; circular-safe (form
  inside the inscribed circle, corners empty so the round crop never
  clips). Campaign `~/Documents/Working Desktop/gens/ac-avatar/`
  (gen-promo `--no-jeffrey`, pals-logo.png ref), gen v9. Circular-crop
  QA: `magick AV circ-mask.png -compose Multiply -composite` (mask =
  `magick -size 3000x3000 xc:black -fill white -draw "circle 1500,1500 1500,0"`;
  the alpha/CopyOpacity path mis-composites to a flat π/8 gray disk —
  use Multiply).
- canvas (per-track, 9:16, silent, 3–8 s loop) — see each track's
  `canvas ·` CDN line. **Canvas is a first-class build format:**
  `pop/dance/bin/canvas-loop.mjs` (chrome-free by construction — no
  title/progress/timecode/pals/karaoke/lanes/string/disc), wired into
  `build.mjs` formats as `{ suffix:"canvas", canvas:true, dur:6 }`.
  Rapid slit-scan glitch montage through every section illy, seamless
  single-pass loop (no `reverse`/`concat` — safe on 8 GB). Encoded
  QuickTime-safe: silent stereo AAC track (Canvas must be inaudible,
  not track-less) + High@4.0 + CFR + 1 s GOP, else QuickTime freezes
  on frame 1.
- **CDN-overwrite gotcha:** `assets.aesthetic.computer` is fronted by
  the DO Spaces CDN (id `2ff25b29-db80-48e6-888e-eb8a2464d69b`, 1 h
  TTL). `s3 sync` skips same-size overwrites, and even after `s3 cp`
  the edge serves the **stale** object for up to an hour. Always:
  `aws s3 cp … && doctl compute cdn flush 2ff25b29-db80-48e6-888e-eb8a2464d69b --files pop/<file>`
  then re-`curl -sI` to confirm `content-length` matches local.
- header · https://assets.aesthetic.computer/pop/ac-spotify-header.jpg —
  2660×1140, **colored-pencil**, near-monochrome BLACK + Spotify-green:
  a Hieronymus-Bosch scatter of many tiny prancing jeffreys + the AC
  crew (citizens + tricky-tinkerbell pixies, PALS on their laptop
  backs) across rolling Grant-Wood green hills, a green sun radiating
  green radio-waves, music notes piping from the chartreuse Neos.
  Lower-left kept clear for Spotify's avatar+name overlay. Campaign
  `~/Documents/Working Desktop/gens/ac-header/` (jeffrey refs +
  whistlegraph-butterfly.png + pals-logo.png), gen v6.

## amazing grace — WIP (1:24 verse-1 single, cool-sine arrangement)

- **Lane:** `pop/big-pictures/` · 70 BPM · 3/4 hymn-pace · G major
  pentatonic · jeffrey-pvc sung lead, **cooler sine instrumentation**
  layered over the existing sinebells waltz bed + pad. The "New
  Britain" tune (William Walker, 1835), **verse 1 only** as a focused
  single. **1:24 / 84 s** total: ~12 s cool-sine intro pad → verse 1
  vocal (≈58 s) → ~14 s sine outro fade.
- **Status (2026-05-20):** 1:24 master + brightened mp3 done. Vertical
  IG cut rendered with placeholder rooftop illys. **Scene direction
  pivoted** to a small old church sanctuary, jeffrey in a pew with his
  green Neo open on his lap, singing while playing the notepat
  on-screen piano, congregation in pews around him. Prompts updated
  but illy regen is **BLOCKED** — OpenAI billing hard-limit was hit
  mid-set (verses 1–6 + the deprecated rooftop scene rendered, verse 7
  + cover failed).
- **Master:** `~/Documents/Working Desktop/amazing-grace/amazing-grace-MASTER.wav`
  (16-bit/44.1 kHz stereo, **84.00 s / 1:24**). Pre-bright copy at
  `amazing-grace-MASTER-preBright.wav`. 320 k mp3 +
  3000² cover embedded: `amazing-grace/amazing-grace.mp3` (4.6 MB).
  Mirrored to `~/Desktop/amazing-grace.mp3` +
  `~/Desktop/amazing-grace-MASTER.wav`. Desktop copies auto-clean
  ([[feedback_desktop_autocleaned]]) — the
  `~/Documents/Working Desktop/amazing-grace/` copy is durable.
- **Bake recipe (deterministic, byte-faithful):**
  1. `pop/big-pictures/amazing.np` and `amazing.txt` hold the **verse-1
     edit** (full 7-verse hymn preserved in `amazing.np.bak-7verse` /
     `amazing.txt.bak-7verse`).
  2. `node pop/big-pictures/cli.mjs amazing --force` (engine: jeffrey-pvc
     TTS → ElevenLabs `/with-timestamps` align → score-pitch WORLD f0
     replacement → score-stretch rubberband 8× ceiling, +60 ms onset →
     melody-bells pad → waltz.mjs sinebells bed → amix vox 2.5 / pad
     0.55 / bed 0.20 → ac signoff stamp → finalize ID3+cover) →
     `pop/big-pictures/out/amazing-final.mp3` (≈58 s).
  3. `node pop/big-pictures/bin/cool-sine-layer.mjs --bpm 70 --duration 84
     --out /tmp/amazing-cool-sine.wav` (three-voice all-sine layer: sub
     bass an octave below the chord root with chorus detune, 3-osc sine
     pad on root/5/oct with 0.18 Hz LFO + wide stereo, two-octave-up
     sparkle bell triggered on each chord downbeat between t=14..t=68,
     soft tanh saturation + peak-normalize -3 dB).
  4. Mix: `ffmpeg -i amazing-cool-sine.wav -i amazing-final.mp3
     -filter_complex "[1:a]adelay=12000|12000,apad=pad_dur=12,volume=1.4[v];
     [0:a]volume=0.9[c];[v][c]amix=inputs=2:duration=longest:normalize=0,
     atrim=duration=84,afade=t=out:st=82:d=2"` → 1:24 pre-bright master.
  5. Bright polish: `equalizer=f=190:g=-1, equalizer=f=4200:g=2.2,
     highshelf=f=8500:g=4, highshelf=f=12500:g=1.8,
     loudnorm=I=-14:TP=-1.2:LRA=13` → final master.
- **Cover (PENDING re-gen — billing limit):** the **planned** scene is
  a small old church sanctuary, late-afternoon honey light through
  stained glass, jeffrey in a pew with the chartreuse Neo open across
  his lap (whistlegraph-butterfly white-paper scrap on the lid),
  singing while pressing the on-screen notepat piano keys, AC freaks +
  pixsies in pews around him. Prompts at
  `~/Documents/Working Desktop/gens/amazing-grace-sections/{verse1..7,cover}/cover-prompt.txt`
  (church direction; rooftop-hymn-circle previous versions backed up
  alongside). Generator: `pop/big-pictures/bin/gen-amazing-prompts.mjs`
  + `pop/big-pictures/bin/gen-amazing-illys.sh`. The **placeholder**
  cover (rooftop verse6 square crop → 3000²) is currently at
  `~/Desktop/amazing-grace-cover-3000.jpg` and embedded in the
  release mp3. **Regen the church cover once OpenAI credits are topped
  up.**
- **Vertical IG cut:** 1080×1920, **1:24**, slow ken-burns drift
  across two placeholder illys (verse1 golden-hour rooftop → 6 s
  xfade → verse6 dusk rooftop), title PNG fades in over 0–3 s. Output:
  `~/Desktop/amazing-grace-vertical.mp4` (15 MB). brew ffmpeg 8.1 here
  ships **without libass/drawtext/subtitles filters** — title is
  pre-rendered via ImageMagick + overlay; lyric subtitles deferred to
  a follow-up pass (install a libass-enabled ffmpeg or render subtitle
  PNG sequence). **Re-render** the IG once the church illys land.
- **Source files (in repo):**
  - lyrics (verse 1 single) — `pop/big-pictures/amazing.txt` ·
    backup of 7-verse hymn at `amazing.txt.bak-7verse`
  - score (verse 1 single) — `pop/big-pictures/amazing.np` · backup
    of 7-verse hymn at `amazing.np.bak-7verse`
  - prompts (church scene) —
    `pop/big-pictures/bin/gen-amazing-prompts.mjs`
  - illys launcher — `pop/big-pictures/bin/gen-amazing-illys.sh`
  - cool-sine layer — `pop/big-pictures/bin/cool-sine-layer.mjs`
  - vertical IG renderer — `pop/big-pictures/bin/vertical-amazing.mjs`
- **Next (outward — manual / your call):**
  1. Top up OpenAI credits, then
     `bash pop/big-pictures/bin/gen-amazing-illys.sh v2` to regen the
     full church scene set (verses 1–7 + cover) with the updated
     prompts.
  2. After church illys land: re-crop verse1 (church hero) → 3000²
     cover via `bash pop/big-pictures/bin/finalize-amazing-cover.sh v2`
     and re-render the vertical IG via
     `node pop/big-pictures/bin/vertical-amazing.mjs`.
  3. Stage `system/public/assets/pop/amazing-grace.mp3` +
     `amazing-grace.jpg` + `amazing-grace-canvas.mp4`,
     `npm run assets:sync:up`, then `doctl compute cdn flush
     2ff25b29-db80-48e6-888e-eb8a2464d69b --files pop/amazing-grace.mp3
     --files pop/amazing-grace.jpg --files pop/amazing-grace-canvas.mp4`
     ([[project_cdn_overwrite_stale]]).
  4. DistroKid upload: `~/Desktop/amazing-grace-MASTER.wav` + cover
     `~/Desktop/amazing-grace-cover-3000.jpg`, artist **Aesthetic Dot
     Computer**, album **pixsies**, title **amazing grace**, writer
     credit **John Newton (lyrics, public domain)** + **William Walker
     (melody, public domain)** + arrangement **Aesthetic Dot Computer**.
  5. Flip this entry to **RELEASED** with the DistroKid album UUID.

## trancepenta — RELEASED

- **Lane:** `pop/dance/` · 5/4 · 126 BPM steady · dorian; chill-trance
  instrumental — hellsine gabber kick (tik/tok alternating pitch),
  galloping intro (real CC0 horse + neigh + thunder + train-whistle +
  late-third ocean bed), Odyssey theremin solo line (3-act dorian arc),
  jeffrey hum at 9 wandering pitches with per-entry FX (dancing with
  the theremin, never aligned), supersaw power-saw wall, extreme swing
  crescendoing to the 1:33 audio stamp, dice-roll click-clack tumbling
  in post-vortex. Spec: `pop/dance/trancepenta.md`.
- **Released:** 2026-05-20 via DistroKid
- **Canonical:** https://distrokid.com/dashboard/album/?albumuuid=860FA7AC-AE6E-4B0A-ABC6E1514D273054
- **CDN assets** (canonical, public — `system/public/assets/pop/`, gitignored, synced via `npm run assets:sync:up`):
  - audio · https://assets.aesthetic.computer/pop/trancepenta.mp3 (320 k mp3 of the radio-balanced master, 190.69 s)
  - cover · https://assets.aesthetic.computer/pop/trancepenta.jpg (3000², felt-character hero crop with multi-section lighting)
  - canvas/IG · https://assets.aesthetic.computer/pop/trancepenta-canvas.mp4 *(pending the other agent's render — see vertical-video session)*
  - Reconstructable byte-faithfully any time: `BAKE_FORCE=1 bash pop/dance/bin/bake-trancepenta.sh`
    → produces `~/Documents/Working Desktop/twi-out/trancepenta-MASTER.wav` + paired struct.json
    → cover at `~/Documents/Working Desktop/gens/trancepenta-cover-3000.jpg` (felt-character regen via `gens/trancepenta-cover-final/`)
- **Master:** `~/Desktop/trancepenta-DISTROKID/trancepenta-MASTER.wav`
  — 16-bit/44.1 kHz stereo WAV, 190.69 s. **Radio-balanced master:**
  −14 LUFS / −1.4 dBTP / LRA 4.5 LU (Spotify-ready, tight broadcast
  dynamic range). Master chain: highpass 28 Hz → aecho 4-tap space
  reverb (with stamp-window reverb-duck) → glue compressor (−16 / 3.0 /
  8 dB knee) → 4-band EQ (120 Hz −1, 250 Hz −1.2, 3.5 kHz +2.5,
  11 kHz +1.8) → loudnorm I=−14 LRA=6 → alimiter 0.94 → 18 s fade.
  Instrumental (no vocal phrase — only jeffrey-hum harmonics).
- **Cover:** 3000² `~/Desktop/trancepenta-DISTROKID/trancepenta-cover-3000.jpg`
  — felt-character + scary-tattered hero, jeffrey + young Mark
  Zuckerberg at the PALS laptop in an after-hours Trader Joe's, with
  prismatic multi-section ambient lighting refracting around the
  laptop's PALS lid back-glow. Felt characters with frayed/torn felt
  clothing (Wes Anderson / Aardman-grade craft realism inside a real
  photographic environment). Multi-section lighting bakes the song's
  8-section arc into the ambient fog colour. Generated via
  `gen-promo.mjs` campaign at `gens/trancepenta-cover-final/`.
- **DistroKid folder:** `~/Desktop/trancepenta-DISTROKID/` — README,
  CHART.md, trancepenta-CHART.png (annotated 4K multitrack chart of
  the bake, sections + events + lane breakdowns + open-TODOs).
- **Vertical IG / Spotify Canvas:** in progress (separate agent owns
  the final video render — `cover-video.mjs` with the dampened pan +
  Odyssey-theremin events visible on the lane visualization).
- **Open TODOs (for the next track):**
  · kick wider (Haas stereo, broader lows)
  · hi-hats shorter + sharper (decay envelope tuning in playPercussion `g`)
  · choral phoneme vocals (jeffrey-pvc, no words — hahaha / olololol /
    rerererere / babobebebebabo / ummy wummy / woo woo) layered at
    root + 3rd + 5th + oct with long reverb
  · vocal pitch-correction to the Odyssey theremin melody (`rubberband`
    per-slice once compiled into ffmpeg, or a new
    `place-penta-autotune.mjs`)

## hellsine — WIP (concept track, first cut)

- **Lane:** `pop/hellsine/` · the *all-sine* concept track — every voice
  is `Math.sin` (saturated-sine gabber kick, sine sub, additive-sine
  Williams brass/strings, sine-FM stab, saturated-sine hoover, sine-blip
  perc). The thesis: a distorted sine **is** the gabber kick.
- **Status:** first full cut rendered + mastered 2026-05-19. Awaiting
  @jeffrey's ears + creative direction (composition/arrangement pass
  expected before any release).
- **Form:** hardcore spine carrying a hand-composed John Williams arc —
  heroic D-minor leitmotif: overture (strings hint, no kick) → statement
  (brass + gabber kick) → study-calm bridge (B theme, half-time pulse) →
  development (theme fragmented + sequenced + hoover + scream riser) →
  climax (full kick + theme restated **+2 semitone key-lift** + FM
  stabs) → continuous loop-friendly coda. 182 BPM, 4/4, ~1:48.
  Built for **epic study music**: no dead air, immersive under the arc.
- **Master:** `~/Documents/Working Desktop/hellsine/hellsine-MASTER.wav`
  (+ `hellsine.mp3` 320 k) — 16-bit/44.1 kHz, 1:48.69, **−13.7 LUFS /
  −1.2 dBTP / LRA 4.1** (low LRA is genre-correct for hardcore). The
  pure-sine mix rendered very dark (lows ~31 dB over the air band); the
  cut ships a **gabber transient click** (HF at the source, still
  sine-derived) + a heavier brightening polish than the trance note
  (high-shelf 7.5 k +8, presence 4.2 k +4, sparkle 12 k +4, 200 Hz
  −2.5) → gap closed to ~19 dB.
- **Source (deterministic):** `node pop/hellsine/bin/bake.mjs`
  (engine `pop/hellsine/bin/hellsine.mjs` seed `"hellsine"` → optional
  `--scratch` post-FX via `pop/dance/bin/scratch-mix.mjs` → Spotify
  finalize). Theme/harmony are hand-shaped data, not random — same seed
  + flags = byte-identical. Tune: `bake.mjs <dir> -- --hell 14 --bpm 186`.
- **Next:** @jeffrey listen → arrangement notes → (cover / DistroKid only
  after the composition is locked).

## trancenwaltzi — RENDER (not yet released)

- **Lane:** `pop/dance/` · chill mix (`--mode chill --meter 3`), ~3:00,
  photographic **Trader Joe's** calm-shopping concept w/ pixie crew
- **Status:** v17 square cut + v18 vertical IG-story cut rendering
  (1500×1500 + 1080×1920); pixies = all ages incl. kids→elders,
  casual-cyberpunk; post grade = hue-shift + bright greens + sharpen +
  smooth fade ending; lightbox/stained-glass backlight (face+laptop).
- **Outputs:** `~/Desktop/trancenwaltzi-iter.mp4` (square),
  `~/Desktop/trancenwaltzi-vertical.mp4` (IG-story)
- **Canvas (staged early):** https://assets.aesthetic.computer/pop/trancenwaltzi-canvas.mp4
  — same `canvas-loop.mjs` model (v25p happy-arc portrait set). Not
  uploadable to Spotify until the track is released.
- **Cover (LOCKED 2026-05-18):** `~/Documents/Working Desktop/gens/trancenwaltzi-cover-3000.jpg`
  (3000², ~2.6 MB, mirrored to `~/Desktop/`). Concept = **in-aisle
  Trader Joe's party — "welcome to the talk show, come chill with us"**:
  jeffrey arms-wide hosting to camera, the pixie crew vibing through the
  aisle, AC upload-sparkles rising, his citrus-green Neo +
  whistlegraph-butterfly (both arms) low-front, pixie laptops PALS-only,
  no readable brand wordmark (shot indoors — sidesteps the Trader-Joe's
  trademark issue the exterior takes had). Source = outro illustration
  **v31** (`gens/trancenwaltzi-sections/outro/gens/v31.png`, native 1024²
  → 3000², square so no crop). Generated by `marketing/bin/gen-promo.mjs`
  from `gens/trancenwaltzi-sections/outro/cover-prompt.txt` (which now
  carries the trancenwaltz-cover formal composition lineage + the chill
  TJ's photographic skin; prior prompt at `cover-prompt.prev.txt`).
  Iteration arc v26→v31: medium TJ's → bigger head → ultra-close →
  manger tableau → in-aisle talk-show party (final). Cover **still** is
  independent of the cover **video**: `build.mjs` trancenwaltzi square
  format is still on the old `v16` 8-section set — moving the video to
  this concept = regenerate all 8 sections (separate larger job).
- **Next:** upload cover+audio to CDN
  (`assets.aesthetic.computer/pop/trancenwaltzi.jpg` / `.mp3`) → master
  ear-check → DistroKid (artist "Aesthetic Dot Computer") → add the
  **trancenwaltzi — RELEASED** entry here.

---

### Mastering note (learned on trancenwaltz)

The trance.mjs master chain renders **dark** (~16 dB roll-off into the
highs) — sine-heavy synth mix. Ship-ready masters need a brightening
polish pass: high-shelf air @ ~8.5 k (+4), presence @ ~4.2 k (+2),
sparkle @ ~12.5 k (+2), slight 190 Hz mud trim, `loudnorm I=-14
TP=-1.2 LRA=13`. Brightness costs LRA (10.5 → ~6.7) — acceptable for a
loud modern dance master; offer a −15 LUFS / wider-LRA variant if more
dynamics are wanted. Always A/B against the pre-bright take.
