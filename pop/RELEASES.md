# Pop Releases

Progress tracker for finished `pop/` singles — what's shipped, what's in
flight, and where it lives. See [SCORE.md](SCORE.md) for the mill mission.

Status legend: **RELEASED** · **SUBMITTED** · **MASTERING** · **RENDER** · **WIP** · **IDEA**

---

## YouTube state — pixsies singles (snapshot 2026-05-29)

All AC YouTube cuts are **1920×1080 landscape only** (vertical = Short, metadata doesn't surface). Titles are **Title Case**; descriptions are **hashtag-only**. Chrome family for all `*-yt.mjs` forks: trancepenta-yt side stamps — pals + rotated-90° title chars climbing UP beside each stamp (movie-poster spine). Pre-render title chars with `shadowColor: null` so the per-frame tint works.

| Track | Video | Chrome / Build |
|---|---|---|
| Trancepenta | [Em_lvTYET7M](https://youtu.be/Em_lvTYET7M) | `preview-score-trancepenta-yt.mjs` (canonical side-stamps) |
| Trancenwaltz | [RBG3k_XsfLA](https://youtu.be/RBG3k_XsfLA) | `cover-video.mjs --size 1920x1080` w/ v10b landscape illys |
| Marimbaba (v3) | [jXdnJjdV_kY](https://youtu.be/jXdnJjdV_kY) | `preview-score-marimbaba-yt.mjs` — vignette removed (mirrors trancepenta-yt fix); replaces v2 `X63Ni-Lb_Kc` (deleted 2026-05-26, edges were crushed by the leftover contrast vignette) |
| Helpabeach (v3) | [KVynj0RAwg8](https://youtu.be/KVynj0RAwg8) | `preview-score-helpabeach-yt.mjs` — multiply vignette half of `drawBacklight` dropped (warm sunset screen-glow kept); replaces v2 `WKdMYawwDPY` (deleted 2026-05-29 for the same side-darkening complaint) |
| Hellsine (v2) | [IjMGmPDvO4I](https://youtu.be/IjMGmPDvO4I) | `preview-score-hellsine-yt.mjs` — `drawVignette(c, v_i)` call removed from `paintSectionPanel` (function kept as dead code, mirrors marimbaba v3 / trancepenta-yt fix); replaces v1 `9KPUr6mA5e8` (deleted 2026-05-29, side-darkening crushed the felt-puppet panel edges); promoted to public 2026-05-29 |

YouTube API quota: **6 video_insert / day / project** (`defaultVideoInsertPerDayPerProject`); deletes count toward the same window. On 2026-05-29 the de-vignette pass burned 5 of 6 (hellsine v1 upload at 02:04Z + delete hellsine v1 + delete helpabeach v2 + upload hellsine v2 + upload helpabeach v3) — next YT churn must wait for the 24h rolling window to clear.

Open follow-ups (queued, not blockers):
- Re-run `node pop/chillwave/bin/gen-illy.mjs --slug helpabeach --landscape --sections --validate-butterfly` once OpenAI gpt-4o-mini quota clears — the validator wraps each gen with a vision check against `pop/chillwave/assets/wg-scrap.png` and regens FAILs (drift-1 visibly fails: tree-person not butterfly).
- `helpabeach-yt-forms.json` not authored — backlight/zoom layer disabled on landscape until face-detected forms land (`pop/dance/bin/detect-face.py` is the precedent).

---

## marimbaba — RELEASED

- **Lane:** `pop/marimba/` · lullaby for synthesized marimba, 1:24 ·
  56 BPM · F major · slow 3/4 · 24 bars. Fully instrumental — rosewood
  marimba lead, half-bar bass "rocking chair" pulse, off-beat kalimba
  twinkles, motorless vibraphone dream-haze pad. A syllabic lyric
  (hush-hush / twin-kle / wow-wow / ba-ba-bap / sleep-now) is written
  into the score but stays unspoken; the marimba hums it. Part of the
  *pixsies* body.
- **Artist:** Aesthetic Dot Computer
- **Status:** RELEASED — delivered via DistroKid 2026-05-22, live in
  stores. Assets published to the AC assets CDN 2026-05-22.
- **Listen (canonical):** https://open.spotify.com/track/1gopbVPw6LoinIpOANOnEG
- **Assets (AC CDN):**
  - audio — https://assets.aesthetic.computer/pop/marimbaba.mp3
  - cover — https://assets.aesthetic.computer/pop/marimbaba.jpg
  - visualizer — https://assets.aesthetic.computer/pop/marimbaba.mp4
  - youtube · https://youtu.be/jXdnJjdV_kY (1920×1080 landscape visualizer — uploaded 2026-05-26 as v3, REPLACED v2 `X63Ni-Lb_Kc` which was deleted same day because the contrast vignette layer (mistakenly carried over from the trancepenta-yt code that was later cleaned up) crushed the panel edges; the v3 render removes `drawVignette(c, v_i)` from `paintSectionPanel` and keeps just the transmitted backlight + leaded contrast layers (the figures already pop without the periphery darkening). Built from the `-yt` landscape illy set (`gen-sections.mjs --landscape` → 11 panels at 1536×1024 with the LANDSCAPE_NOTE re-framing the late-night study for 16:9) and the landscape fork `pop/marimba/bin/preview-score-marimbaba-yt.mjs`. Side-stamps chrome (pals + rotated-90° "marimbaba" climbing up beside each stamp) matches trancepenta-yt. v1 was `qntoYeAZSmM` (unused-`drawTitle` side stacks), v2 was `X63Ni-Lb_Kc` (correct chrome but vignetted). Title is "Marimbaba"; description is hashtag-only.)
- **DistroKid dashboard:** https://distrokid.com/dashboard/album/?albumuuid=772E43F5-D367-44A9-A7B4A1FA4E57FBD9
  (admin-only)
- **Master:** `~/Documents/Shelf/marimbaba-DISTROKID/marimbaba-MASTER.wav`
  — 44.1 kHz / 16-bit stereo WAV, 83.6 s. ≈ −14.5 LUFS, −1.5 dBTP.
  Master chain: highpass 30 → soft glue comp → +1 dB air @ 7.5 k →
  alimiter 0.95 → loudnorm I=−14 TP=−1.5 LRA=11. (Durable home is
  `~/Documents/Shelf/` — Desktop auto-cleans,
  [[feedback_desktop_autocleaned]].)
- **Cover:** `…/marimbaba-DISTROKID/marimbaba-cover-3000.jpg` — 3000²,
  a colored-pencil + gouache drawing: a tight head-and-shoulders crop
  of jeffrey + Bill Gates side by side, quiet and somber, their hands
  typing on an IBM Model M keyboard along the bottom edge, no screen in
  frame. Generated by `gen-illy.mjs` from
  `pop/marimba/marimbaba.illy.txt` (jeffrey identity refs);
  1024² source at `pop/marimba/out/marimbaba-cover.png`, upscaled to
  3000². (Earlier concept: a photographic `recovery-tears` library
  scene — superseded.)
- **DistroKid folder:** `~/Documents/Shelf/marimbaba-DISTROKID/`
  — MASTER.wav, cover-3000.jpg, README.md (submission-form fields).
- **Reconstructable** ($0, deterministic): `node pop/marimba/bin/render-marimbaba.mjs
  --wav <path> --no-open` → master chain above.
- **Storyline visualizer (9:16 insta-story):**
  `pop/marimba/out/marimbaba-preview-score-portrait-insta-story.mp4`
  — 1080×1920, 1:24, the marimbaba lullaby as one quiet story: jeffrey's
  late-night **computer help call** with Bill Gates. **10 colored-pencil
  + gouache panels** (two per `.np` section) with a changing-emotion
  arc — approaches the house → doorway hello → crossing → settles in →
  points it out → it clicks → Gates tries it → hands-on → it worked →
  drowsy close (the album cover). Built on the shared engine: a
  verlet-physics **playhead string** with the marimba's note events
  riding it as pitch-coloured blocks (4 voice lanes); a **3-layer
  backlight** (warm glow from behind + contrast vignette + per-figure
  halo); a **face zoom** easing wide↔face twice per section; and **6
  transitions** (iris / blinds / push / zoom-punch / pixel /
  diagonal-wipe) cycled across the 9 boundaries.
- **Visualizer pipeline:** `node pop/marimba/bin/render-marimbaba.mjs
  --no-open` (emits `out/marimbaba.struct.json` — sections + note
  events) → `node pop/marimba/bin/gen-sections.mjs` (gpt-image-2 →
  portrait cover + 10 panels, cached) → `node
  pop/marimba/bin/preview-score.mjs` → the mp4. Figure bboxes for the
  zoom + backlight in `marimbaba-forms.json`.

---

## helpabeach — RELEASED

- **Lane:** `pop/chillwave/` · chillwave / ambient instrumental, 2:31 ·
  84 BPM · A minor pentatonic. Freesound CC0 calm-ocean bed, all-sine
  intro rollers, sub-octave sine bells, pad, in-render formant
  computer-voice on wordless nonsense phonemes (from ~0:47). No chimes,
  all-sine (no bitcrush/flange). Part of the *pixsies* body.
  (Formerly `undabeach`.)
- **Artist:** Aesthetic Dot Computer
- **Released:** 2026-05-21 via DistroKid
- **Listen (canonical):** https://open.spotify.com/track/3jzlAylJQLSsNIXjnEY1e8
- **DistroKid dashboard:** https://distrokid.com/dashboard/album/?albumuuid=B1F5253A-11FA-40EF-83A89866A157829A (admin-only)
- **CDN assets** (canonical, public — `system/public/assets/pop/`, gitignored, synced via `npm run pop:assets:up`):
  - audio · https://assets.aesthetic.computer/pop/helpabeach.mp3 (320 k mp3 of the master, 151.1 s)
  - cover · https://assets.aesthetic.computer/pop/helpabeach.jpg (3000², Rhizome Health clinic tableau)
  - video · https://assets.aesthetic.computer/pop/helpabeach.mp4 (1080×1920 vertical IG-story, 9 clinic panels)
  - story cut · https://assets.aesthetic.computer/pop/helpabeach-short.mp4 (1:17 narrated cut)
  - youtube · https://youtu.be/KVynj0RAwg8 (1920×1080 landscape visualizer — uploaded 2026-05-29 as v3, REPLACES v2 `WKdMYawwDPY` (deleted 2026-05-29) which had a multiply-vignette darkening the periphery inside `drawBacklight`; the v3 render keeps the warm sunset screen-glow but drops the multiply layer so the clinic tableau reads to the edges. Earlier v1 `NG55RkBI7N0` (deleted 2026-05-24) had the wrong horizontal-top-title chrome. The v3 mp4 at `pop/chillwave/out/helpabeach-preview-score-yt.mp4` (126 MB) carries the side-stamps chrome matching trancepenta-yt: pals stamps snug against rotated-90° "helpabeach" climbing up beside each stamp; no top horizontal title.)
- **Master:** `~/Documents/Shelf/helpabeach-DISTROKID/helpabeach-MASTER.wav`
  — 44.1 kHz / 16-bit stereo WAV, 151.1 s. ≈ −13.4 LUFS, −1.5 dBTP.
  Master chain: highpass 24 → treble +1.8 dB @ 9.5 k → loudnorm
  I=−14 TP=−1.5 LRA=11 → alimiter 0.95. (Durable home is
  `~/Documents/Shelf/` — Desktop auto-cleans, [[feedback_desktop_autocleaned]].)
- **Cover:** `…/helpabeach-DISTROKID/helpabeach-cover-3000.jpg` — 3000²,
  the Rhizome Health clinic tableau (gpt-image-2, colored-pencil +
  gouache; `pop/chillwave/out/helpabeach-cover.png` upscaled lanczos).
- **DistroKid folder:** `~/Documents/Shelf/helpabeach-DISTROKID/`
  — MASTER.wav, cover-3000.jpg, README.md (submission-form fields).
- **Reconstructable** ($0, deterministic): `node pop/chillwave/bin/render.mjs
  --slug helpabeach --no-chimes --no-waves --no-sweeps --no-highmel
  --voice-gain 0.22 --voice-start 47 --wav` → master chain above.
- **Story cut:** `helpabeach-short` — a 1:17 narrated IG-story cut
  (jeffrey-pvc first-person narration over the Rhizome Health clinic
  panels). Separate from this single; see `pop/chillwave/helpabeach-short.np`.

---

## trancenwaltz — RELEASED

- **Lane:** `pop/dance/` · dark/emo/extreme war-arc trance-waltz, ~1:26
- **Released:** 2026-05-17 via DistroKid
- **Listen (canonical):** https://open.spotify.com/track/3PIPwPqptVlWy71rCEhQum
- **DistroKid dashboard:** https://distrokid.com/dashboard/album/?albumuuid=8FF25085-8F58-4A3A-986A52A21D638805 (admin-only)
- **CDN assets** (canonical, public — `system/public/assets/pop/`, gitignored, synced via `npm run assets:sync:up`):
  - audio · https://assets.aesthetic.computer/pop/trancenwaltz.mp3 (320 k mp3 of the bright master, 86.62 s)
  - cover · https://assets.aesthetic.computer/pop/trancenwaltz.jpg (3000², outro art)
  - canvas · https://assets.aesthetic.computer/pop/trancenwaltz-canvas.mp4 (Spotify Canvas — 1080×1920, 6 s, **silent**, seamless loop, **chrome-free**, slit-scan glitch montage of all 8 section illys)
  - youtube · https://youtu.be/RBG3k_XsfLA (1920×1080 full-chrome visualizer — title / multi-lane piano-roll / karaoke / progress reframed landscape; uses the **v10b** native-landscape illy set, regenerated from `cover-prompt-landscape.txt` so the canonical whistlegraph-butterfly lid scrap lands correctly; ZOOM_DAMP halves ken-burns amplitude on 16:9 so the illys breathe instead of pumping)
  - Reconstructable byte-faithfully any time: `trance.mjs --meter 3 --vocal-stem pop/dance/out/trance-hook-layered.mp3 --master` (deterministic seed `trancewaltz`) → brightening polish (high-shelf 8.5 k +4, presence 4.2 k +2.2, sparkle 12.5 k +1.8, 190 Hz −1, `loudnorm I=-14 TP=-1.2 LRA=13`) → 320 k mp3; cover = outro v15 `-gravity North -crop 1024x1024+0+96 → 3000²`. (Desktop copies are auto-cleaned — see [[feedback_desktop_autocleaned]]; CDN + repo `assets/pop/` are the durable home.)
- **YouTube visualizer build recipe** (deterministic from the bright master + v10b illys):
  1. `node marketing/bin/gen-promo.mjs <secdir> --variant v10b --size 1536x1024 --prompt-file cover-prompt-landscape.txt --force --no-mirror` per section (regen-only; the `cover-prompt-landscape.txt` is the portrait prompt with PORTRAIT framing language swapped for LANDSCAPE 3:2 / 16:9 strip).
  2. `recap/.venv/bin/python3 pop/dance/bin/detect-face.py <secdir>/gens/v10b.png` per section (writes `.face.json` sidecars the backlight emitters need).
  3. `node pop/dance/bin/cover-video.mjs --track <master.mp3> --illustrations intro=...,break1=...,... --size 1920x1080 --prelude <intro-prelude>/gens/v10.png --title trancenwaltz --bpm 137.143 --out <out.mp4>` (the **build.mjs** `youtube` format wires this).
  4. `node toolchain/youtube/yt.mjs upload <out.mp4> --title "Trancenwaltz" --description-file pop/dance/trancenwaltz.youtube.txt --tags "trance,waltz,electronic,visualizer,aesthetic computer,pixsies,music" --privacy public --category 10`. (YouTube title convention is Title Case; description is hashtags only — see 2026-05-24 metadata refresh.)
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
  clips). Campaign `~/Documents/Shelf/gens/ac-avatar/`
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
  `~/Documents/Shelf/gens/ac-header/` (jeffrey refs +
  whistlegraph-butterfly.png + pals-logo.png), gen v6.

## amaythingra (amazing grace, dance) — SUBMITTED · v2 FINAL master (4:33 vowel-extraction remix)

- **Lane:** `pop/big-pictures/c/` (C-engine bed) · 120 BPM 4/4 · G major
  · Loukeman-style deep-house remix of Amazing Grace.
- **Artist:** Aesthetic Dot Computer · **Album:** *Pixsies*
- **Status:** **v2 is the FINAL master (2026-06-03).** v1 was uploaded to
  DistroKid 2026-06-02 (awaiting go-live) — to ship v2, replace/re-upload the
  DistroKid file from `~/Desktop/Amaythingra.wav` (next time it surfaces, fill
  in the canonical URL + flip to RELEASED + publish CDN assets, like marimbaba).
- **The cut:** a 4:33 **vowel-extraction** remix — the hymn vocals stripped to
  OPEN/ROUNDED vowels only (ah/aw/uh — "ashhs and oooh", no consonants), their
  sustain loops **beat-aligned to the kick grid**, softened with per-voice
  vibrato + amplitude scatter (jazzy, breathing), and ridden **deep in the mix**
  (vox 0.07 + heavy reverb wash) so they ghost in faint from ~2:00 under the bed.
- **v2 break rebuild (2026-06-03, @jeffrey):** the ~1:36-2:08 break is now a
  proper trance-style **build** (per `pop/dance/STUDY.md` — "tension that pays
  off on the drop downbeat"). Bars 48-51 still state + resolve; **bars 52-55**
  (~1:44-1:52) flip to an **ascending G-major bell climb that defers
  resolution** (each step higher/louder/longer); **bars 56-63** keep the dense
  swirling **bell vortex** but now arc **upward in register** and lean onto the
  **dominant (D)** in the last bar. Real **turntable scratches** are woven
  through the vortex (`scratch_render` in `c/amaythingra.c` — a vinyl "record"
  tone with forward-push / backward-pull groove phase, surface hiss tracking
  the hand speed, crossfader chop on the pull-back), growing 1→2 per bar, with
  one decisive **D-pitched scratch dragging back onto the downbeat** as drop 2
  lands on the tonic **G** at bar 64 (2:08). Plus: final-90s 808 backbeat
  **ducked** + occasional **reverse-snare→snare fills**, and the 808 **fades in
  slowly** when the beat returns after the break (subtle most of the track).
- **Master (v2):** **-14.0 LUFS / -1.23 dBTP**, 44.1 kHz/16-bit, 0 clipping.
  Deliverables: `~/Desktop/Amaythingra.wav` + `~/Desktop/Amaythingra.mp3`
  (Title-Case tags) + `~/Desktop/Amaythingra-v2.wav` (48k/24-bit master) and the
  packaged set in `~/Desktop/amaythingra-DistroKid/` (tagged WAV +
  cover-embedded mp3 + cover-3000.png). Source masters live in
  `~/Documents/Shelf/amaythingra/`.
- **Pipeline** (supersedes the bake-c-dance.mjs recipe below):
  1. `node pop/big-pictures/bin/extract-vowels.mjs` → ah/ooh vowel drone stem
     (gated to open+rounded vowels, tight grain window, beat-aligned loops,
     per-voice vibrato + amplitude variation)
  2. `node pop/big-pictures/bin/bake.mjs --vox 0.07 --reverb 0.32 --vox-delay 120 --vox-fadein 70 --vox-last 0 --package`
     (C-engine bed → vowels sidechained to kick → FX → master → DistroKid package)
  - Bed (`c/amaythingra.c`): reverse-snare→snare @3:30, stochastic per-phrase
    808 shaping (tight/punchy/smooshy-washed), distinct longer woops at
    2:56/3:00/3:10, powersaws lift an octave by ~3:45, build-dip into the 0:32
    drop, centered fundamentals for stereo balance.
- **Historical (2026-05-29, "amazing grace dance" 4:58 version):** 4:58 master
  at -8.8 LUFS / -0.4 dBTP. DistroKid cut at -8.9 LUFS / -1.0 dBTP (24-bit/44.1
  kHz, 75 MB) in `~/Documents/Shelf/amazing-grace-dance/distrokid/`. Mono-bass
  widening via acrossover (200 Hz / 4th-order LR). Recipe below is from this
  version.
- **Bake recipe:**
  1. `pop/big-pictures/c/build-dance.sh` (C engine compiles → bed.wav)
  2. `node pop/big-pictures/c/bake-c-dance.mjs` (mixes 153+ harmony
     files, 50 syllable variants, atmosphere layers, pachinko + whale
     beds + scratches + master EQ/compression/wider/limiter)
  3. Final DistroKid WAV via `alimiter=limit=0.82` for -1 dBTP ceiling
- **Master:** `~/Documents/Shelf/amazing-grace-dance/amazing-grace-dance-c-MASTER.wav`
  (16-bit/44.1 kHz, 298 s) + `amazing-grace-dance-c.mp3` (320k).
  DistroKid: `~/Documents/Shelf/amazing-grace-dance/distrokid/`.
- **Sample sources (all CC0/PD, commercially safe):**
  - Whales — Wikimedia Commons (blue whale, humpback-sfx, seagulls)
    in `pop/samples/whales-wikimedia/` + `pop/samples/ocean-ia-cc0/`
  - Pachinko — Designer's Choice UCS via Internet Archive (CC0),
    `pop/samples/pachinko-ia-cc0/pachinko-floor-300s.wav` (looped from
    `pachinko-machine-playing.wav` — naturally in G major, bells ring
    G6/C6/F6 matching the vocal key)
  - Field bells — `pop/dance/out/.bell-0[1-3].wav` (trancepenta bells)
- **DistroKid submit:**
  `node pop/bin/distrokid-submit.mjs ~/Documents/Shelf/amazing-grace-dance/distrokid`
  (dry-run validated 2026-05-29)
- **Track diagram:** `~/Desktop/amazing-grace-dance-layers.pdf`
  (gantt of all 38 layers grouped by category, may need regenerate
  for current 300 s structure)

---

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
- **Master:** `~/Documents/Shelf/amazing-grace/amazing-grace-MASTER.wav`
  (16-bit/44.1 kHz stereo, **84.00 s / 1:24**). Pre-bright copy at
  `amazing-grace-MASTER-preBright.wav`. 320 k mp3 +
  3000² cover embedded: `amazing-grace/amazing-grace.mp3` (4.6 MB).
  Mirrored to `~/Desktop/amazing-grace.mp3` +
  `~/Desktop/amazing-grace-MASTER.wav`. Desktop copies auto-clean
  ([[feedback_desktop_autocleaned]]) — the
  `~/Documents/Shelf/amazing-grace/` copy is durable.
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
  `~/Documents/Shelf/gens/amazing-grace-sections/{verse1..7,cover}/cover-prompt.txt`
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
- **Listen (canonical):** https://open.spotify.com/track/4SVH80CTkq2BihSlSyiJhG
- **DistroKid dashboard:** https://distrokid.com/dashboard/album/?albumuuid=860FA7AC-AE6E-4B0A-ABC6E1514D273054 (admin-only)
- **CDN assets** (canonical, public — `system/public/assets/pop/`, gitignored, synced via `npm run assets:sync:up`):
  - audio · https://assets.aesthetic.computer/pop/trancepenta.mp3 (320 k mp3 of the radio-balanced master, 190.69 s)
  - cover · https://assets.aesthetic.computer/pop/trancepenta.jpg (3000², felt-character hero crop with multi-section lighting)
  - canvas/IG · https://assets.aesthetic.computer/pop/trancepenta-canvas.mp4 *(pending the other agent's render — see vertical-video session)*
  - youtube · https://youtu.be/Em_lvTYET7M (1920×1080 full-chrome visualizer — uploaded 2026-05-24. The native-landscape 16-panel YT illy set + cover at `pop/dance/out/trancepenta-yt-{sec-*,cover}.png` were regenerated 2026-05-24 from felt-puppet prompts (Aardman / Isle-of-Dogs craft language, tattered felt clothes, bearded jeffrey, felt mark + felt pixsies with prominent LED-bead tells) so the YT visualizer matches the album cover's felt aesthetic. The first re-render had a heavy contrast vignette crushing the panel edges; that layer was removed from `preview-score-trancepenta-yt.mjs` (function kept as dead code) and the file re-rendered with bright edges. Photoreal originals stashed under `pop/dance/out/_yt-photoreal-backup/`.)
- **YouTube visualizer build recipe** (deterministic from the felt panel set + bright master):
  1. `node pop/dance/bin/gen-trancepenta-sections.mjs --force` (regen 16 felt landscape panels + cover at 1536×1024 via the felt-puppet constants in the script; concurrency 3; ~15–20 min wall time; ~$5 OpenAI).
  2. `node pop/dance/bin/preview-score-trancepenta-yt.mjs` (default I/O — reads `~/Documents/Shelf/twi-out/trancepenta.mp3` + struct, writes `pop/dance/out/trancepenta-preview-score-yt.mp4`; ~35 min for the full 190 s @ 30 fps).
  3. `node toolchain/youtube/yt.mjs upload pop/dance/out/trancepenta-preview-score-yt.mp4 --title "Trancepenta" --description-file pop/dance/trancepenta.youtube.txt --tags "trance,chilltrance,electronic,visualizer,aesthetic computer,pixsies,music,instrumental,5/4" --privacy public --category 10 --thumbnail /tmp/yt-thumbs/trancepenta-thumb.jpg` (the thumbnail must be ≤ 2 MB; the 3000² CDN cover is too big — use the 1280-wide ffmpeg downscale or call `yt.mjs thumbnail <videoId> <image>` after the fact).
  - Reconstructable byte-faithfully any time: `BAKE_FORCE=1 bash pop/dance/bin/bake-trancepenta.sh`
    → produces `~/Documents/Shelf/twi-out/trancepenta-MASTER.wav` + paired struct.json
    → cover at `~/Documents/Shelf/gens/trancepenta-cover-3000.jpg` (felt-character regen via `gens/trancepenta-cover-final/`)
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

## solafiya — MASTERING

- **Lane:** `pop/jungle/` · spicy latina jungle (ragga sub-format), 1:40
  · 176 BPM felt half-time (~88) · key A · sunlit minor-pentatonic-with-
  major-third warmth. Hybrid AC-kit break (chopped/shuffled
  `percussion.mjs` web + synthesized break-stab burst at phrase ends —
  no Winstons, no Amen sample), deep dub sub on offbeat push, warm
  golden sinepower pad, reggae skank stab, sparse dub sirens / airhorns,
  AC bell + ding + meow + marimba + sineloop + shrill + throat
  ornamentation. Three sequential verses, no overlap. Part of the
  *pixsies* body. First **fía** solo lead — her live ragga toast forced-
  aligned through the WORLD pipeline into vocal / vocalAd / vocalDuet /
  vocalH / throat lanes, baked into the mix.
- **Artist:** fía (TBC — could be `fía & Aesthetic Dot Computer` for
  store-page coherence with the other pixsies singles; ask @jeffrey)
- **Status:** MASTERING — final master baked 2026-05-23; awaiting
  @jeffrey's ears + creative direction before DistroKid upload. Cover +
  README in place.
- **Master:** `~/Documents/Shelf/solafiya-DISTROKID/solafiya-MASTER.wav`
  — 16-bit/44.1 kHz stereo WAV, 100.18 s. **−13.5 LUFS · −1.0 dBTP ·
  LRA 2.3** (jungle wants tight; source mix is already heavily
  compressed at −19.4 LUFS / LRA 2.2 — no extra dynamic range to
  reclaim). Master chain: highpass 28 → 250 Hz mud trim → 4.2 k
  presence +2 → high-shelf 8.5 k +5 (air restore — render high-cuts at
  ~8.6 k for HC vibe) → high-shelf 12.5 k +2.5 → loudnorm I=−14
  TP=−1.5 LRA=8 → alimiter 0.95. Pre-bright A/B copy at
  `solafiya-MASTER-preBright.wav`. (Durable home is
  `~/Documents/Shelf/` — Desktop auto-cleans,
  [[feedback_desktop_autocleaned]].)
- **Cover:** `…/solafiya-DISTROKID/solafiya-cover-3000.jpg` — 3000²,
  Pixar-style fía + 7 different-breed kittens on a golden-hour beach,
  matcha + closed laptop + white W210 Mercedes + bounding dog +
  polychrome sparkles. Lanczos upscale of `pop/jungle/out/solafiya.illy.png`
  (1024² gpt-image-2 source, prompt at `pop/jungle/solafiya.illy.txt`).
- **DistroKid folder:** `~/Documents/Shelf/solafiya-DISTROKID/`
  — MASTER.wav, MASTER-preBright.wav, cover-3000.jpg, mp3 (320 k +
  embedded cover), README.md (submission-form fields + open questions).
- **Reconstructable** ($0, deterministic): `node pop/jungle/bin/render.mjs
  --slug solafiya` (cached) then `bash pop/jungle/bin/bake-solafiya.sh`
  → master chain above. `BAKE_FORCE=1` to re-bake from a fresh render.
- **Open before submitting:** (1) **artist credit** — `fía` solo vs
  `fía & Aesthetic Dot Computer`; (2) **lyrics** — only
  forced-aligned word JSON exists (`solafiya-fia-words.json`), no
  canonical `.txt`; (3) **canvas / vertical video** — 19 lane raw
  buffers + 6 section illys are baked, so `preview-score.mjs` /
  `preview-spin.mjs` can produce a 9:16 IG cut + Spotify Canvas loop
  (not yet built).

---

## hellsine — RELEASED

- **Lane:** `pop/hellsine/` · the *all-sine* concept track — every voice
  is `Math.sin` (saturated-sine gabber kick, sine sub, additive-sine
  Williams brass/strings, sine-FM stab, saturated-sine hoover, sine-blip
  perc). The thesis: a distorted sine **is** the gabber kick. Vocal
  layer is the 12-word jeffrey-pvc mantra **"i hope that we get all of
  the (money | honey | bunnies) that we want"** cycling across the
  three variants. Part of the *pixsies* body.
- **Artist:** Aesthetic Dot Computer
- **Released:** 2026-05-26 via DistroKid
- **DistroKid dashboard:** https://distrokid.com/dashboard/album/?albumuuid=5BAE15FB-BDDF-4C1D-8EBFB2E68D7D4A9C (admin-only)
- **Listen (canonical):** _Spotify URL pending — fill in once the store
  page is live._
- **Form:** hardcore spine carrying a hand-composed John Williams arc —
  heroic D-minor leitmotif: overture (typewriter keys + sine strings
  hint, no kick) → statement (brass + gabber kick + first vocal pass) →
  study-calm bridge (B theme, half-time pulse, vocal stack thickens) →
  development (theme fragmented + sequenced + hoover + crowd-roar
  D-minor arpeggio + pitched crowd chops) → AC-stamp drop → climax
  (full kick + theme restated **+2 semitone key-lift** + FM stabs, vocal
  drone) → coda (typewriter callback + outro gallop). 182 BPM (atempo
  ×1.06 from 110.77 s onward), 4/4, **2:41.93**.
- **Master:** `~/Desktop/hellsine-MASTER.wav` (+ `~/Desktop/hellsine.mp3`
  320 k) — 16-bit/44.1 kHz stereo PCM, 161.93 s, **−14.1 LUFS / −2.3
  dBTP / LRA 6.7**. Max sample peak −2.47 dBFS, zero clipped samples.
  Master chain (`pop/hellsine/c/bake-c.mjs` legacy chain): C engine
  render → atempo 1.06 from 110.77 s onward → **crescendo-into-drop**
  dynEnv (0.75 → 1.55 at 110.77, then 1.55 → 1.10 sustain → 0.40 coda)
  → highshelf 9 k +4 → 6 k presence +1.5 → 4 k +2 → highshelf 12 k +1.5
  → 150 Hz −1 → 320 Hz −2 → acompressor −24/3:1 → loudnorm
  I=−14 TP=−1.5 LRA=11 → alimiter 0.85 → −1.2 dB → afade-out 159.7 s
  for 2.3 s → truncate 162.0 s. (Desktop copies auto-clean
  [[feedback_desktop_autocleaned]] — durable home is
  `~/Documents/Shelf/hellsine/`.)
- **Cover:** `pop/hellsine/out/hellsine-cover.png` (1024², generated by
  `pop/hellsine/bin/gen-illy.mjs` from `pop/hellsine/hellsine.illy.txt`).
  CDN + 3000² upload pending.
- **YouTube:** https://youtu.be/IjMGmPDvO4I (1920×1080 felt-puppet
  storyline visualizer — uploaded 2026-05-29 as **v2**, promoted to
  public 2026-05-29. REPLACES v1 `9KPUr6mA5e8` (deleted 2026-05-29) —
  the v1 render had
  the `drawVignette(c, v_i)` multiply pass darkening the panel edges,
  same complaint that drove the marimbaba v3 + trancepenta-yt cleanups.
  v2 removes that call from `paintSectionPanel` in
  `pop/hellsine/bin/preview-score-hellsine-yt.mjs` (function kept as
  dead code) and re-renders with bright edges — the transmitted
  backlight + leaded contrast layers already shape the panel without
  the periphery darken. The v2 mp4 at
  `pop/hellsine/out/hellsine-preview-score-landscape.mp4` (162 MB) is
  built from the 18 section panels + 162 s C-engine master. Title is
  "Hellsine" (Title Case per the YT-snapshot convention); description
  is hashtag-only in `pop/hellsine/hellsine.youtube.txt`. (v1's
  long-form description + lowercase title — "hellsine — Aesthetic Dot
  Computer" — were carried over from the older AC upload style; both
  were retitled / re-described on 2026-05-29 alongside the trancepenta
  title-case fix, after jeffrey reasserted the rule for the whole
  channel.)
- **Reconstructable** ($0, deterministic): `node pop/hellsine/c/bake-c.mjs`
  (builds the C engine if stale; engine output deterministic at fixed
  BPM). For A/B with the JS engine: `node pop/hellsine/bin/bake.mjs`.
- **Final-mix decisions (locked 2026-05-26):**
  - **Choir voicing — girly + soft:** `unisonVoiceIdx = {bells,
    whisper, boing, wobble, good-news, bells, boing, whisper}` (all
    bright, all have `.words.txt` sidecars — bubbles + bahh
    honey/money are MISSING sidecars and silently skip → never
    select). All choir intervals upward (+5/+7/+12).
  - **Soft progression `vocalRamp`** drifts 0.30 → 0.85 → 0.65 → 0.35
    across passes 0..17 (no SPOTLIGHT at j=12 — was the "scream").
  - **Vocals under drums:** `variantGain = {1.55, 1.40, 1.30}` (was
    {2.35, 2.15, 2.00}).
  - **robotAtt:** j=12 → 0.55 (kills the chipmunked "i" at 1:19),
    j=16 → 0.55, j=17 → 0.35 (drone tail).
  - **80 ms vocal onset fade-in** on lead WSOLA + backings so the
    Hann-window grain-0 click is gone.
  - **Tom break at 1:12 REMOVED** entirely.
  - **Held piano chords at 126-133 s REMOVED** + matching instrument
    dip.
  - **Chaotic snare rush at 122-128 s REMOVED** (originally tried as
    gallop-rhythm snare-glitches; both versions cut).
  - **Mid-track gallop at 130 s REMOVED**; **outro gallop at 158.65 s
    kept** + encore scratch (4 crow chops + 5 screwed tail) kept.
  - **BOOwub reverse-kick:** post-AC-stamp 0.30 (full dub tail),
    pre-stamp dropped to 0.12 (subtle).
  - **Climax kicks consistent 2:06-2:11:** removed the 125.5-131 s
    wipe so the halftime pattern carries through.
  - **Swung hi-hats** (triplet offbeats via `swing_offbeat(0.85)`) +
    `humEager` lead (pushes ahead of grid, not behind).
  - **AC stamp ~1.5× louder** (main 0.68, 5th 0.42, octave 0.27, body
    0.33).
  - **Flyby warble** sine layer under the vocals (15.8 → 110 s, ±2 st
    5 Hz wobble, 6 s pan sweep, 0.045 gain).
  - **Key 5 typewriter reverse-kick swell** (320 ms, 48 → 78 Hz,
    squared envelope) — only intentional reverse in the opening.
- **Next:** Spotify URL drop-in once live · 3000² cover + CDN sync
  (`assets.aesthetic.computer/pop/hellsine.{mp3,jpg}`) · canvas/IG cut
  via `pop/hellsine/bin/preview-score.mjs` (not yet built).

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
- **Cover (LOCKED 2026-05-18):** `~/Documents/Shelf/gens/trancenwaltzi-cover-3000.jpg`
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
