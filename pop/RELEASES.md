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
