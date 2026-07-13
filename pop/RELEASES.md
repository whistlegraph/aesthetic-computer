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
| Marimbaba (v4) | [EqX7Lkr5aRs](https://youtu.be/EqX7Lkr5aRs) | `motion-score-marimbaba-yt.mjs` — FIRST MOTION CUT: Seedance 2.0 image-to-video shots (gen-motion-marimbaba.mjs, fal.ai) as the background, full preview-score chrome on top, capital-M side badges; replaces v3 `jXdnJjdV_kY` (panel Ken-Burns build, deleted 2026-06-11) which replaced v2 `X63Ni-Lb_Kc` (deleted 2026-05-26, vignette edge-crush) |
| Helpabeach (v3) | [KVynj0RAwg8](https://youtu.be/KVynj0RAwg8) | `preview-score-helpabeach-yt.mjs` — multiply vignette half of `drawBacklight` dropped (warm sunset screen-glow kept); replaces v2 `WKdMYawwDPY` (deleted 2026-05-29 for the same side-darkening complaint) |
| Amaythingra | [2FwOYCXS4UA](https://youtu.be/2FwOYCXS4UA) | `preview-score-yt.mjs` (big-pictures fork) — 32 cinematic beats, 3 render modes (photoreal office / glossy-grey metaverse / psychedelic AC-glitch degradation); chrome: per-slide tint, stained-glass colour-sep backlight, radial chromatic+blur edge-warp, glowy lanes, slide-blink bar, shimmer labels, kick beat-bump. Replaces `-at3b-chBbE`/`F-RhEL9OQ1g`/`KpK_MV8-178` (all deleted) |
| Hellsine (v2) | [IjMGmPDvO4I](https://youtu.be/IjMGmPDvO4I) | `preview-score-hellsine-yt.mjs` — `drawVignette(c, v_i)` call removed from `paintSectionPanel` (function kept as dead code, mirrors marimbaba v3 / trancepenta-yt fix); replaces v1 `9KPUr6mA5e8` (deleted 2026-05-29, side-darkening crushed the felt-puppet panel edges); promoted to public 2026-05-29 |
| Momabobasheep | [GCOhg5J-brw](https://youtu.be/GCOhg5J-brw) | **listener video** (not a preview-score cut) — 15 felt Seedance clips (`pop/momboba/video/board.json` + `build.mjs` driver, auditioned in ShotWizard) cycling so no clip repeats back-to-back, each under a fresh crop drift, smooth xfade dissolves between scenes, trimmed jumpy tails + slo-mo + stop-motion fps, gentle wavy displacement + film grain + unsharp, staggered pals badges (left low / right high, pals leading each rotated title column). `bin/assemble-listener.mjs` (cycling/crossfade plate) + `bin/chrome-listener.mjs`. Public 2026-06-17 |

YouTube API quota: **6 video_insert / day / project** (`defaultVideoInsertPerDayPerProject`); deletes count toward the same window. On 2026-05-29 the de-vignette pass burned 5 of 6 (hellsine v1 upload at 02:04Z + delete hellsine v1 + delete helpabeach v2 + upload hellsine v2 + upload helpabeach v3) — next YT churn must wait for the 24h rolling window to clear.

Open follow-ups (queued, not blockers):
- Re-run `node pop/chillwave/bin/gen-illy.mjs --slug helpabeach --landscape --sections --validate-butterfly` once OpenAI gpt-4o-mini quota clears — the validator wraps each gen with a vision check against `pop/chillwave/assets/wg-scrap.png` and regens FAILs (drift-1 visibly fails: tree-person not butterfly).
- `helpabeach-yt-forms.json` not authored — backlight/zoom layer disabled on landscape until face-detected forms land (`pop/dance/bin/detect-face.py` is the precedent).

---

## americomputadora — RELEASED 2026-07-04 (DistroKid → Spotify, the 4th-of-July cut)

- **Lane:** `pop/americomputadora/` · bubblegum-bachiamatrixian · D major · 112 BPM ·
  2:29 · the three-syllable hook (america / computer / dora) with jeffrey's
  ElevenLabs chant threaded through as choir, ornament, zoom-by, and departure.
- **Engine:** `c/americomputadora.c` (C, stereo) — July-4th session rebuilt the
  whole back end: TRUE STEREO mixdown (constant-power bus pans, mono-safe bed
  wideners, dual decorrelated Schroeder rooms), printed on the **tape substrate**
  (`pop/lib/substrate.mjs` sat print + master chain) with the print LIFTING to
  hi-fi for 8 bars at the drop. Spatial story: near-mono choral cloud →
  condenses to a solo point → detonates wide at the drop → corridor bridge →
  accelerando outro (last 30 s speed up continuously — train leaving the
  station — while folding back toward mono).
- **Vocals:** hook samples +2 st; america↔computer vowels BRIDGED (0.65 s blend,
  0.30 s pre-roll, computer at 1.35× holding 0.45 under dora) so the junction
  reads "americawwwwmputer"; intro = ghost-bed choral→solo arc (5 detuned
  duplicate jeffreys thinning to one wild surging solo); sung two-note ornaments
  litter the first 90 s; skip-a-boo zoom-bys thread 4 sections (0:38 / verse 2 /
  crunch zone / pre-finale, more stretched each return); outro jeffreys return
  octaves up, sine-flanging, rocking L↔R. "aesthetic dot computer" stamp
  freshly synthesized via /api/say (jeffrey-pvc), glitch chops + telephone-EQ
  bloom, drums blacked out for the bar before the finale shock rifle.
- **Percussion:** classic kit pitched down (kick 110, serious 150, snare 165 Hz)
  + crunch hats that EVOLVE (pitch/attack/drive ride song position, 16ths +
  open hats in hot hooks); novelizer voices (memkick/cavikick/gransnare) ported
  and kept behind `--novel-drums`; spacey one-bar drum break at 1:24; ~1:45
  crunch zone = bitcrush + stereo-throb flange + the track SCRATCHING ITSELF
  (moving-tap scrub of the mix's own history) + sampled scratch cluster.
- **FEM bells:** `pop/bell` stone bells toll "o say can you see" across the
  bridge, echo two bars later (anthem echoes, ≤E5 per the bell law).
- **Cover:** `cover-android/gens/v9-crayon-patriot.png` — soft waxy crayon
  three-headed robot (jeffrey/singer/explorer) in red-white-blue star panels,
  PALS patch left breast + whistlegraph butterfly right, sailor-pixies tugging.
  Chosen over v7 pencil and v10 Soutine oil-goop (both kept in gens/).
- **Canvas:** `americomputadora-canvas.mp4` — 8 s 9:16 Seedance loop of v9
  (pixies tug, heads sway out of phase, fireworks re-bloom).
- **Master:** −9.0 LUFS · −1.8 dBTP · stereo 16-bit/44.1k · ID3 album `pixsies`.
- **Bundle:** `~/Documents/Shelf/americomputadora/` (master wav, tagged mp3,
  3000×3000 cover, canvas mp4). Uploaded to DistroKid and LIVE same day,
  2026-07-04. **On the AC CDN (2026-07-12):** `assets.aesthetic.computer/pop/americomputadora.mp3`
  + `.jpg` cover + `-canvas.mp4` (the 8 s Seedance loop). Remaining: attach the
  Canvas in Spotify for Artists + paste the Spotify link here once it surfaces
  in search/S4A.

---

## boombaboom — WIP

- **Lane:** `pop/boombaboom/` · sine-techno · D minor · ~3:19 (accelerando) ·
  @jeffrey's "boom-ba-boom" vocal chant (`~/Downloads/boombaboom.MP4` — a 32.5s
  melodic whistlegraph-drawing take around C#4 with a recurring **E-E-F** hook)
  re-cast as a hypnotic sine-techno track, rendered ENTIRELY in C.
- **Vocal:** autotuned to D minor (`bin/autotune.py`, note-mode) → **beat-aligned**
  ('boom' plosives → beats, 'ma' → bars, `bin/grid-warp.py` rubberband timemap,
  R3 smooth) → **stretched 1.3× for epic sustained vowels**. Sits deep + soft +
  whistle-dark, with a deep octave-down "throat" under it, fifth/third choral
  harmonies, and high "angel" octave-up layers on the back half.
- **Bed (sine-only law + a few samples):** `c/boombaboom.c` — sine kick
  (tick-tock D/A), sine bells (long flanged decay), a healing **432 Hz
  sine-swarm that IS his voice** (energy-following deep echo), deep chord pad,
  power sines, a dribbly filtered **square lead** under the vocal, tight dark
  hat. Samples: faint deep **rainforest rain** + a close **LA helicopter** flyby
  (both Freesound CC0, see `sources/*.CREDITS.txt`).
- **Form:** lost-vocal prelude → build → strong drop on the first boom →
  3 passes / breakdowns (whistle-feature breakdown) → octave-lower "true ending".
  Whole mix **gradually accelerates +9%**. Humanized timing throughout.
- **Pipeline:** `c/render.sh` (autotune → grid-warp → stretch → harmonies →
  osc/boom/ma cuts → C engine → pop master → accelerando). Graphic score:
  `bin/gen-score.mjs`. Felt cover (jeffrey + the whistlegraph drawing):
  `bin/gen-illy.mjs` → `illy/boombaboom-cover.png`.
- **Status:** WIP — rendered + cover embedded (ID3 album "pixsies"). Committed
  `boombaboom.mp3` to the lane. Master in `~/Documents/Shelf/boombaboom/`.

---

## Fluttabap360 — RELEASED 2026-06-30 (DistroKid → Spotify)

- **Lane:** `pop/marimba/` · the fat, fast, six-minute butterfly-park banger ·
  **exactly 6:00.000** (187 bars, 4/4 @ ~124.7 BPM, back-solved + hard-clamped
  to the boundary so a player reads "6:00", never "6:01") · a buttery,
  poppin' cousin of the flutterbap material (butterfly / pal-of-mine /
  mommy-wow / slinky / fly / land hooks) re-cast over four escalating passes.
  **C-CANONICAL:** the JS file (`bin/render-fluttabap360.mjs`) composes + bakes
  the score; the **C engine** (`c/fluttabap360.c`) does ALL the DSP, mixdown,
  reverb-scene automation, finalize + master. The JS audio path is deprecated
  (kept for reference). Part of the *pixsies* body.
- **Instrument roster (all synthesized / physically-modeled — no samples):**
  **dynabell** (additive bells whose waveform morphs sine → triangle → square,
  pitch-bendy, variable tail) · **fembell** (FEM shell-model bells from
  `pop/bell` — singing-bowl / church / handbell accents) · **gong** (gnarly
  long-blooming gongs — FEM steel/glass 24-mode spectrum + shimmer bloom +
  crash, ~15 s wash) · **flute** (the AC Cook jet-waveguide from `gm_synth.c` —
  ambient dawn tones, the hush melody, the fly soars: the "flutta") · **vortex**
  shoopy turntable warbles · **boowoop** reverse-bell flourishes · squeak-rides ·
  fat bass-perc + butter sub + thick sub-octave · snare drive in the climaxes.
- **Substrate:** printed on the **`tape`** medium (`pop/lib/substrate.mjs` —
  tube-glue compression + wow/flutter + saturation + hiss); a **`vinyl`** print
  also exists (`--substrate vinyl`). The "substrate" is the reusable print-medium
  abstraction (per-sample sat baked into the score + the ffmpeg master chain).
- **Build (one command):** `pop/marimba/bin/render-c.fish` (bake → C album →
  seamless loop) or `node pop/bin/pop.mjs render fluttabap360 --loop`. The
  unified `pop` CLI (`pop/bin/pop.mjs`) drives every song via a `*.song.json`.
- **Key-change journey:** modulates **C → D → E → (home C) → F** — Pass A in C,
  Pass B lifts to D, Pass C is the brightest in E, the late cave breakdown
  drops home to C for a breath, then a euphoric jump up to F for the final
  chorus (resolves home through progression → land → button). Every
  modulation blooms a **held tonic-triad pad swell**, with a long held chord
  under each `mommy-wow` hush.
- **Squeak-rides:** short melodic scratch+squeak duets that **swell in and out**
  of the fight (hump envelope) — the scratch SINGS a pitched call (C→E→G), the
  squeak ANSWERS up top (G→C→E), both transposed with the key and blooming in
  a shared space-reverb send so they sit BACK in the mix. Fights halved from
  8 → 4 bars (the old ~2:00 grind is now ~7.8 s, melodic).
- **Space:** a shared room-reverb SEND deepens the whole field (melodic + squeak
  + pad voices); kick + sub stay DRY so it's still fat and punchy.
- **Artist:** Aesthetic Dot Computer
- **Status:** **RELEASED 2026-06-30** via DistroKid (single, album field
  *pixsies*) — live on Spotify. Release packet (with legal-name metadata, kept
  OUT of this public repo): `~/Documents/Shelf/Fluttabap360-DISTROKID/`
  (MASTER.wav, cover-3000.jpg, CANVAS.mp4, release.json, README).
- **Cover:** `pop/marimba/out/fluttabap360-cover.png` (1024², embedded in the
  mp3) — a colored-pencil + gouache drawing: the butterfly cosplayer (jeffrey
  identity refs) **dancing ballet** — relevé + low arabesque + port de bras —
  with four or five monarchs circling him as dance partners. His real clothes
  (pale-yellow button-down + jeans), hand-made monarch wings, antenna
  headband; NO tutu. Generated by `gen-illy.mjs` from
  `pop/marimba/fluttabap360.illy.txt`. For DistroKid, upscale to 3000².
- **Outputs:** `pop/marimba/out/fluttabap360.mp3` (butter-mastered, cover
  embedded) · `fluttabap360.distrokid.wav` (44.1 kHz / 16-bit, −14 LUFS /
  −1.5 dBTP, **360.000 s**) · `fluttabap360.struct.json` (section map +
  per-section `keyOffset`).
- **Reconstructable** ($0, deterministic): `node pop/marimba/bin/render-fluttabap360.mjs`
  (`--out <path>` to redirect; `--loop` for the seam-perfect loop). Re-embed
  the cover with `node pop/marimba/bin/gen-illy.mjs --embed-only --cover
  out/fluttabap360-cover.png --mp3 out/fluttabap360.mp3`.
- **DistroKid metadata (as submitted):**
  - Track title: **Fluttabap360** · Artist: **Aesthetic Dot Computer**
  - Single · album field **pixsies** · Genre **Electronic** / **Pop**
  - Instrumental: **Yes** · Explicit: **No**
  - Songwriter: @jeffrey (legal name on the DistroKid form — not in this repo) ·
    bylined @jeffrey / aesthetic.computer (never Studio Zollo)
  - ISRC: auto (DistroKid) · Cover: 3000² of the ballet illy
- **Spotify Canvas:** `pop/marimba/out/fluttabap360-photo-canvas.mp4` — a
  seamless 9:16 loop of the cosplayer twirling, **photographic** (fal nano-banana
  photo-illy off the real jeffrey portrait refs → fal Seedance image-to-video).
  An illustrated montage Canvas (`fluttabap360-canvas.mp4`) is the alt.
- **Motion assets:** `pop/marimba/out/motion/fluttabap360-{intro,fly,button,
  photo-twirl}.mp4` (fal Seedance from the illys).

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
- **Apple Music:** https://music.apple.com/us/album/marimbaba/6772460819?i=6772460820
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
- **Apple Music:** https://music.apple.com/us/album/helpabeach/6772056808?i=6772057057
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
- **Apple Music:** https://music.apple.com/us/album/trancenwaltz/6773318345?i=6773318348
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

## amaythingra (amazing grace, dance) — RELEASED · v2 FINAL master (4:33 vowel-extraction remix)

- **Lane:** `pop/big-pictures/c/` (C-engine bed) · 120 BPM 4/4 · G major
  · Loukeman-style deep-house remix of Amazing Grace.
- **Artist:** Aesthetic Dot Computer · **Album:** *Pixsies*
- **Status:** **RELEASED — v2 FINAL master (2026-06-03).** v2 re-uploaded to
  DistroKid/stores by @jeffrey; CDN assets published. (v1 went to DistroKid
  2026-06-02, superseded by v2.)
- **Listen (canonical):** https://open.spotify.com/track/4hVFWMISa235ybIEfOYuwt
- **Apple Music:** https://music.apple.com/us/album/amaythingra/6776359148?i=6776359150
- **Canonical assets** (DO Spaces CDN, `pop/` flat path like marimbaba):
  - audio (wav) — https://assets.aesthetic.computer/pop/amaythingra.wav
  - audio (mp3) — https://assets.aesthetic.computer/pop/amaythingra.mp3
  - cover — https://assets.aesthetic.computer/pop/amaythingra.jpg
- **Visualizers (storyline cut, 2026-06-04):** a 32-beat illustrated story —
  **office → metaverse → office** — built like the marimbaba set.
  - **youtube** · https://youtu.be/2FwOYCXS4UA (1920×1080 widescreen; the
    CINEMATIC v2 cut — three render modes (real-world office = photoreal/textured ·
    metaverse beach = glossy grey · degradation/spawn/dissolve = super-psychedelic
    AC rainbow glitch), shot variety per beat (wide / close-up / ECU / first-person
    POV / OTS / angles), and the full chrome stack: per-slide colour tint, stained-
    glass colour-separation backlight, radial chromatic+blur edge-warp (periphery),
    lanes clearish-in → bright+glowy-out, slide-change bar blink, shimmer side-
    labels, kick beat-bump, dampened timecode. Replaced (all deleted): `-at3b-chBbE`
    (smoothed-cam, pre-cinematic), `F-RhEL9OQ1g` (32-beat shaky), `KpK_MV8-178`
    (16-beat). Title Case "Amaythingra"; hashtag-only desc
    `#deephouse #amazinggrace #remix #visualizer #aestheticcomputer #pixsies`.
  - **insta-story** (9:16) · `~/Documents/Shelf/amaythingra/amaythingra-insta-story.mp4`
  - **canvas** (Spotify Canvas, 9:16, 8 s silent loop) ·
    `~/Documents/Shelf/amaythingra/canvas/amaythingra-canvas-sequence.mp4`
    (+ `~/Desktop/amaythingra-canvas.mp4`); 8-beat "a"-spine via
    `pop/dance/bin/canvas-loop.mjs`.
  - **Story (8 sections × 4 beats = 32, ~8.5 s each):** jeffrey bored in a drab
    grey tech OFFICE staring at a WOMAN IN GREY (surfboard on the wall); MARK
    ZUCKERBERG onboards him through a portal into a metaverse game; the office is
    REPLICATED + warped inside it (the one woman → an army of grey selfie-girl
    avatars, the wall surfboard → his wakeboard); DRONE-WARS erupt on the
    horizon; he carves the maze, MARK grabs him in a tug-of-war, he wrenches free
    and flies; the GAG (girls topple mid-selfie, never reacting); he breaks away
    to the warm GROUP PIXSIES round-table (the human antidote); the world
    dissolves and he's spat back into the office, discombobulated — then sees the
    woman anew. THREE render modes (`build()` in gen-sections auto-routes):
    real-world office beats = **photoreal/textured** (R_ blocks); metaverse-beach
    beats = **glossy desaturated-grey** Horizon-Worlds render (`cover-prompt.v2.txt`
    law); degradation beats (spawn / portal-void / dissolve — `PSYCH_SET`) =
    **super-psychedelic AC rainbow glitch**. No laptop, no matrix code.
  - **Pipeline** (auto-detects panel count per section, splits each section
    evenly): `node pop/big-pictures/bin/gen-sections.mjs` (portrait `-p` set) ·
    `… --landscape` (`-yt` set, 32 panels each) → `preview-score.mjs` (9:16
    insta-story) · `preview-score-yt.mjs` (16:9 YouTube) · `canvas-loop.mjs`
    (Canvas). Panels conditioned on jeffrey shoot/IG refs via gpt-image-2 edits.
    NOTE: never run two `gen-sections` processes concurrently — parallel calls to
    the OpenAI images/edits endpoint trip spurious 401s (sequential is reliable;
    401 is now retried as transient). Upload via
    `node toolchain/youtube/yt.mjs upload … --privacy public --category 10`.
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
- **Apple Music:** _not yet distributed to Apple Music (only 5 of the 6 pixsies singles are on the artist's Apple catalog as of 2026-06-07); backfill once it propagates._
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
- **Listen (canonical):** https://open.spotify.com/track/1yauGyuJPMzDadtu8S5llk
- **Apple Music:** https://music.apple.com/us/album/hellsine/6773624140?i=6773624141
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
- **Next:** 3000² cover + CDN sync
  (`assets.aesthetic.computer/pop/hellsine.{mp3,jpg}`) · canvas/IG cut
  via `pop/hellsine/bin/preview-score.mjs` (not yet built). (Spotify +
  Apple Music URLs now live — see Listen line above.)

## cutezenwaltzi — WIP (né trancenwaltzi, renamed 2026-07-02)

- **RENAME + NEW DIRECTION (2026-07-02, @jeffrey):** "slow it down, make
  it cuter" → **cutezenwaltzi**. New cut: steady **104 BPM** (gas-pedal
  accel dropped), **128 bars / 3:42**, today's chill defaults (**G major**,
  root G3), `--chill-hats low`, `--master`; PLUS the **nullabye
  carved-noise engine as a voice** — `pop/dance/bin/cutezenwaltzi-nullvoice.mjs`
  re-voices the struct.json lead as a breathy Q-55 noise-whistle (+1 oct)
  + Q-70 sparkles on the bells (+2 oct), baked through
  `pop/nullabye/c/nullnoise.c` (`run-c.mjs --master lullaby`), mixed 0.35
  under the bed. Draft: `pop/dance/out/cutezenwaltzi.mp3` (bed + voice
  stem + score alongside). Spec: `pop/dance/cutezenwaltzi.md`. Awaiting
  @jeffrey ear-check; voice level / register / Q are the tuning knobs.
- Everything below is the **old trancenwaltzi render** (A minor,
  gas-pedal 150→212, ~3:44) — kept as lineage; its TJ's cover + canvas
  were made for that cut and may carry forward or get re-cut cuter.
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

## momabobasheep — RELEASED 2026-06-12 (10-minute Bachian remix of marimbaba)

- **Lane:** `pop/momboba/` · a 10:00 **remix of marimbaba**, the twin of
  marimbaba the way `pop/sleephellsine/` is the twin of `pop/hellsine/` —
  but it keeps the **actual marimba synth** (`pop/marimba/synths/marimba.mjs`),
  which is what makes it a marimbaba *remix* and not a sleephellsine clone.
  Began as a calm sleep drone; evolved (2026-06-08) into a slow, **Bachian,
  rolling, ever-transforming** piece that travels through keys over its 10
  minutes. **THE CONCEPT (jas, 2026-06-11): MoMA (the art museum) + boba
  tea + sheep — "jeffrey goes to the MoMA", drinks a boba, and falls
  asleep counting sheep in the galleries.** Name history: "moronbobasleep"
  → "mombobasleep" (2026-06-10) → **momabobasheep** (2026-06-11). NOTE:
  the render seed stays `"mombobasleep"` — it keys this exact composed
  walk. Part of the *pixsies* body.
- **Artist:** Aesthetic Dot Computer
- **Status:** RELEASED — uploaded to DistroKid by @jeffrey 2026-06-12:
  <https://distrokid.com/dashboard/album/?albumuuid=2456D650-9AEC-4C1A-831BCE628047AF6C>.
  Master clip-tested (−16.0 LUFS integrated / −2.8 dBTP, no clipping).
  MASTERING CALL: −16 LUFS is deliberate for a sleep mix — louder would
  sacrifice the baked dynamic arc, and every platform normalizes playback
  anyway; no brightening pass (warm/dark is the genre, unlike the trance
  lane's note below). mp3 tagged (`title=momabobasheep ·
  artist=Aesthetic Dot Computer · album=pixsies · date=2026`) with the
  cover embedded; upload assets staged in `~/Documents/Shelf/momboba/`
  (MASTER.wav + momabobasheep.mp3 + momabobasheep-cover-3000.png). **On the
  AC CDN (2026-07-12):** `assets.aesthetic.computer/pop/momabobasheep.mp3` +
  `.jpg` cover + `.mp4` (bunny-reel) + `-canvas.mp4`. TODO: Spotify Canvas
  upload via Spotify for Artists once the track goes live (`momabobasheep-canvas.mp4`
  now on the CDN, ready to attach in S4A).
- **Promo (2026-06-12):** full-motion 9:16 set via the Seedance lane —
  8 felt story beats (`pop/momboba/reel/`, gpt-image-2) animated through
  `bin/gen-motion-momabobasheep-reel.mjs` (door-morph entrance,
  drink-down boba continuity, spilled-cup ASCENSION morph into the dream)
  + `bin/chrome-reel.mjs` two-variant chrome over one 1080×1920@30 base:
  `out/momabobasheep-story.mp4` (pals + segmented progress bar +
  timecode) and `out/momabobasheep-reel.mp4` (pals only — marimbaba's
  --reel split for the Reels UI). Canvas: `bin/build-canvas.mjs` —
  8 s silent ping-pong of the dream take. Audio bed = the track from
  5:20, swelling into THE DREAM's golden-section climax.
- **YouTube (2026-06-17):** listener video PUBLIC →
  <https://youtu.be/GCOhg5J-brw> (1920×1080, 10:00, ~1.07 GB). NOT a
  preview-score cut — a new **listener** pipeline: 15 felt Seedance clips
  (the 9 movement scenes + 6 added vignettes: boba-macro, sheep-sky,
  pond-koi, color-field, night-moon, and a closing **sketchbook** shot that
  feeds the day's prior stills forward as gpt-image-2 refs so the pages
  relive the day) **cycled** so no clip repeats back-to-back, each under a
  fresh crop drift, **xfade dissolves** between every scene, trimmed jumpy
  tails + slo-mo + stop-motion fps, gentle wavy displacement + film grain +
  unsharp, staggered pals side-badges (left low / right high, pals leading
  each rotated title column). Storyboarded/auditioned in **ShotWizard**
  (`pop/momboba/video/board.json` + `build.mjs` driver: `--shot <id>` gens
  still+clip, `--assemble` cuts the full video). Built by
  `bin/assemble-listener.mjs` (cycling/crossfade plate) →
  `bin/chrome-listener.mjs` (chrome + post). Title "Momabobasheep",
  hashtag-only description. Thumbnail = the lily-pad dream frame.
- **Engine:** `pop/momboba/bin/render-momabobasheep.mjs` — a **generative**
  renderer (deterministic per `--seed`; the default seed string remains
  `mombobasleep`, the PRNG key for this exact walk). **C ENGINE
  (2026-06-11, house pattern like hellsine/americomputadora):**
  `pop/momboba/c/` — composition stays in JS (score-extract + bake.mjs
  fan-out with an rng parity gate), `momabobasheep.c` executes the DSP
  ~30× faster (610 s in ~20 s) and matches the JS reference to ≤0.01 dB
  on every compare metric (`c/compare.mjs` verdict: all PASS). Run:
  `node pop/momboba/c/render-c.mjs`. 100%
  synthesized — the CC0 rain sample + brownian/air hiss beds were REMOVED
  (2026-06-10: too repetitive as a loop, environmental noise out of the track
  completely); it opens on the tonic pad + drone alone, then builds a
  contrapuntal baroque texture over a fixed **functional progression**
  (ii–V–I / plagal / modal Eb) in F major:
  - **FIBONACCI-ARCH NIGHT NARRATIVE** — 9 movements (bars 3·5·8·13·21·34·55·34·13)
    that are chapters of a night (drift → settle → sink → deepening → dreaming
    → deep dream → THE DREAM → strange REM → dawn). Bar-counts grow to a LATE
    peak so the climax lands near the **golden section** (~0.6 of the runtime),
    then recede; each movement has its own dynamic **level**, key (gentle
    journey, home at the ends), whistle **register**, and density — a real
    arc, not a loop. A baked dynamic curve + a gentle (non-levelling) master
    keep it (verified: per-minute loudness swells ≈ −21 → −17 → −21 dB);
  - **per-chapter PROGRESSIONS** — each movement walks its OWN functional
    progression (calm tonic → ii–V–I → circle-ish turns → modal Eb in REM →
    plagal homecoming), so the harmony itself journeys, not just the dynamics.
    NO chord ever repeats bar-to-bar (2026-06-11, incl. cycle wraps + chapter
    seams — verified 0/186; resty chapters alternate F with Dm/Bb, the piece
    ends Dm → F-tail at the loop seam);
  - **broken-chord arpeggio** figuration (Prelude-in-C style), felt-soft, drawn
    from an **8-pattern library** the renderer walks through bar to bar (range
    widening from 5→7 tones into the dream) so it keeps reshaping;
  - a **WHISTLE** top melody (MenuBand instrument-79 vibe — pure sine +
    faint partials, fade-in vibrato + pitch scoop; its breath-noise whisper
    was removed 2026-06-11 as the last "airy" texture), voice-led +
    harmonised a third below;
  - a **walking bass**, a **quaternary (alto)** inner voice in the bigger
    movements (four-part harmony), a chord sparkle on a **Euclidean E(5,8)**
    rhythm, **triplet** flourishes deep in;
  - fractal **pterodactyl swoops** — gnarly self-similar up-then-dive screech
    runs clustering at the climax;
  - underneath: the **felted** 5-voice drone (warbly, pure tone — stereo
    chorus only) + a fixed-filter deep **sub**, **reverso sine bells** that
    peak on chord arrivals, and a lush Schroeder reverb. ALL remaining filter
    sweeps + texture were removed 2026-06-11 (the drone's 4-stage phaser
    sweep, its felting noise-fizz, and the sub's LFO-swept lowpass "wub") —
    the bed is now sweep-free pitched tone, moved only by chorus +
    per-voice vibrato/drift;
  - **sidechain tempering** — the arpeggio ducks under the whistle. (The
    bass→bed pump was REMOVED 2026-06-11: its 45% dip-and-recover on every
    bass onset read as a kick drum. Same day the bass mallet went from the
    hardest strike (5 ms attack) to the softest (35 ms) — the walking bass
    swells now, it never thumps);
  - **SOFT FELT MALLETS** throughout (long mallet contact + slow attack).
  Every render self-runs a **clip test** (sample/true peak + LUFS).
- **Master:** `~/Documents/Shelf/momboba/momabobasheep-MASTER.wav`
  (44.1 kHz / 24-bit) + `momabobasheep.mp3` (320k). **Sleep** chain (kept
  gentle so the dynamic arc lives): highpass 22 → +3 dB low shelf @ 85 →
  −3.5 dB shelf @ 3.2 k → ONE soft slow glue comp (2:1, no leveller) →
  2 s fade-in / 5 s fade-out guarding the loop seam → **measured LINEAR gain
  to −16 LUFS / −1 dBTP** (loudnorm as meter only), truncate to 10:00. (Durable
  home is `~/Documents/Shelf/` — Desktop auto-cleans, [[feedback_desktop_autocleaned]].)
- **Cover:** `pop/momboba/out/momabobasheep-cover.png` (1024²,
  gpt-image-2 via `bin/gen-illy.mjs` + `momabobasheep.illy.txt`) — FINAL
  (2026-06-11, soft-felt return): **the whole concept as a NEEDLE-FELTED
  WOOL diorama** — felt jeffrey-doll (real outfit in felt, red felt
  glasses folded by his hand) asleep on a felt gallery bench, the
  water-lily room as a monumental FELTED TAPESTRY filling the wall, a
  tiny felted boba with wool pearls on the bench, and a flock of
  needle-felt sheep grazing the felt floor (wool playing itself). No
  text/wordmarks/screens. (Concept lineage: felted couch-doll →
  photographic MoMA scene → this felt MoMA scene.) Embedded in BOTH
  engine mp3s; DistroKid upscale at
  `~/Documents/Shelf/momboba/momabobasheep-cover-3000.png` + Desktop
  mirrors of cover + all four masters.
- **Reel (FMV-ready):** `pop/momboba/reel/` — 8 vertical (1024×1536)
  felt story beats via `bin/gen-reel.mjs` (PREAMBLE + per-beat prompts):
  arrival-with-boba → the sip → galleries → pond-room awe → heavy eyes →
  first sheep → asleep among the flock → the dream on the lily pad.
  Outputs `reel/out/NN-*.png` + Desktop mirrors, for a ~30 s IG reel cut.
- **Graphic score:** `pop/momboba/out/momabobasheep-score.png` (3200×1400,
  also on ~/Desktop) — the whole 10:00 visualized: 9 Fibonacci movement
  bands, the 186-chord walk, per-voice lanes (whistle/bells/sparkle/arp/
  alto/pad/drone+sub ribbons), the baked dynamic arc, golden-section climax
  at 6:11, loop-seam note. Built by `bin/score-extract.mjs` (verbatim
  composition copy, verified bit-identical to struct.json) +
  `bin/build-score.mjs` → chrome-shot. Watchable as a **scorodeon**
  (`pop/bin/scorodeon.mjs` — NEW shared infra for ALL pop lanes, named by
  @jeffrey, score + ὠδεῖον): a scrolling dynamic canvas on the
  preview-score machinery (node-canvas → `spawnFFmpegEncode`) — the
  track's event data flies past a FIXED CENTER LINE, events light up +
  pop as they sound, chord rail, movement banners, drone ribbon, and a
  whole-piece minimap with the dynamic arc. Any `--size` aspect (default
  1080×1920 story-vertical), `--zoom` = seconds visible per screen.
  Generic JSON contract; lane adapter `bin/scorodeon-data.mjs` →
  `out/momabobasheep.scorodeon.json` → `out/momabobasheep-scorodeon.mp4`.
- **Done:** master ear-checked → DistroKid (RELEASED 2026-06-12) → media on
  the AC CDN (`assets.aesthetic.computer/pop/momabobasheep.{mp3,jpg,mp4}`,
  uploaded 2026-07-12).

---

## nullabye — WIP (lullaby carved from cancelled noise)

- **Lane:** `pop/nullabye/` · 1:19 · 76 BPM · C major 4/4 · 24 bars.
  Technique study after Andy Brewer's "this song has no instruments in it"
  ([_Rk-hmIMv6I](https://youtu.be/_Rk-hmIMv6I)): two copies of pink noise,
  one phase-inverted (sum = perfect silence), notes carved by automating
  peaking-EQ bells on one copy — what you hear is the cancellation
  *breaking*. A 24-band pool (matching the video's 24 EQ points, enforced
  with a throw) acts as a breathy noise-sine polysynth; pink noise +
  constant-Q bells give equal voice energy at any pitch. nullabye uses
  15 bands, peak 10 sounding. Part of the *pixsies* body.
- **Artist:** Aesthetic Dot Computer (intended)
- **Engine:** `pop/nullabye/bin/render-nullabye.mjs` — `--proof` renders
  all bands flat and asserts bit-exact digital silence (verified). Sections:
  breaths (wide-bell swells that demo the trick) → lullaby-a (pads + Q 55
  whistle lead + heartbeat 54 Hz kick) → lullaby-b (+ bass roots, 8.2 kHz
  hats, Q 70 sparkles) → veil-lifts (250 Hz → 6 kHz wide sweep, last lone
  whistle, EQ goes flat — ends in literal silence). Lead pitches
  Goertzel-verified (E5/D5 exact). −21.4 LUFS integrated, LRA 11.2 —
  deliberately hushed; revisit level if it ships.
- **Cut 2: nuellaby** (`bin/render-nuellaby.mjs` → `out/nuellaby.mp3`,
  exactly 2:00 — 38 bars @ 76 BPM = 120.0 s, −16.2 LUFS) — the
  complexity-arch cut, named by @jeffrey (nullabyte → noellabye →
  **nuellaby**): the EQ rack doubles 2→4→8→16→32→64→128, then folds
  back down 64→2 (2 bars per halving) and empties — band count as the
  musical-complexity arc. Full spectral-zone carve per @jeffrey: REAL
  kick (110→42 Hz pitch-drop bell) + closed/open hats + snare-shh +
  rim + shaker 16ths + tom fills + ticks + risers, plus drones / floor
  pad / octave pads / arps / twinkle clouds / detunes; ×128 keystone =
  12-partial harmonic choir + 21-band diatonic halo. Engine adds
  **lanes** (persistent monophonic EQ points; consecutive notes drag the
  bell to a new frequency — per-lane events must be inserted in time
  order or the mono-clip silences the earlier note) + a per-stage cap
  audit that throws past each power of two; measured arch
  `2/2 4/4 8/8 16/16 30/32 51/64 92/128 49 29 15 8 4 2`, silence proof
  passes with all 92 lanes flat.
- **C engine (2026-06-11):** `pop/nullabye/c/nullnoise.c` — generic
  cancelled-noise renderer (hellsine-style split: JS composes + bakes a
  score, C does all DSP). `render-nuellaby.mjs --bake` → score.txt →
  `c/run-c.mjs <score> --out x.mp3 --master lullaby|techno`. Verified vs
  the JS reference by `c/compare.mjs`: diff −145 dB below signal
  (libm-ulp territory). `--proof` works in C too. nuellaby itself
  evolved to the **accumulation form** (no stages: 85 points join ~1 per
  beat, bloom, shed ~2/beat LIFO; veil bed runs throughout; key journey
  C→Eb→F→C; per-bar loudness ride). CRITICAL DSP LESSON: the EQ chain is
  SERIAL, so two bells at the same frequency ADD their dB — detuned pad
  doubles (+19+13+13=+45) and a root-partial choir both blew up the mix
  this way; layers must own disjoint frequencies.
- **Cut 3: nullabata** (`bin/render-nullabata.mjs` → `out/nullabata.mp3`,
  ~4:21, −19.1 LUFS) — the **sonata cut** (*nullaby + sonata*): four
  carved-noise movements running **attacca (no silence between)**, bound by
  one lullaby motif (semitone offsets from a per-movement voice tonic)
  transformed per movement: **I. Andante** C maj 72 BPM (theme + warm pads
  + heartbeat kick) · **II. Scherzo** A min 132 BPM (theme shattered into a
  fast arp + twinkle cloud) · **III. Adagio** F maj 56 BPM (theme in long
  tones over sustained pads + drones + spice-partial choir, no perc) ·
  **IV. Finale** C maj 84 BPM (theme recapitulated over a compact 47-point
  accumulation arch: 2→bloom→2, nuellaby's Boléro logic gathering the suite
  home). **CLARITY = Q:** first pass sounded like radio static — every
  bell-on-noise voice is breathy, and a wide bell passes a wide band of
  hiss. Fix was to run the engine at its TONAL extreme: high Q on all
  pitched voices (lead 110 / pads 55 / drones 45 / choir+halo 50+), **kill
  the wide veil bed entirely** (Q 0.55 = the worst static source),
  narrow/strip the noise perc (no shaker/open-hat; hats → narrow pitched
  ticks), and master with a 15 kHz lowpass + no treble boost — energy
  >12 kHz now sits 36 dB below full band. New engine piece: a **per-second,
  movement-aware loudness ride** (`rideTargetAt(t)`) so four very different
  tempos/densities land evenly. 47 lanes, `--proof` passes flat;
  `--bake` → `c/run-c.mjs` renders the same score.
- **Next:** @jeffrey ear-check the three cuts → decide if one grows into a
  full single (more sections, maybe formant-vowel bells for a "choir" à
  la [[feedback_chillwave_formant_voice]]) or they stay studies.

---

## teknull — WIP (fast techno, 32 EQ points, C engine)

- **Lane:** `pop/teknull/` · 2:00 exactly (70 bars @ 140 BPM) · A minor ·
  carved-noise techno on a strict **32-EQ-point budget** (uses 27/32).
  Composition in `bin/render-teknull.mjs`, ALL DSP through the shared C
  engine (`pop/nullabye/c/nullnoise.c`) with the `techno` master preset.
  The technique is home turf here: the **clap is literally the noise**
  (wide 2.3k bell on 2+4), the **acid line is one monophonic EQ point**
  (303 slides = freqEnd glides, accents = gain, note-off = mono-cut),
  kick = 115→44 Hz pitch-drop bell, offbeat 55 Hz sub "oontz".
  Form: intro → build → acid → drop A (+stabs, lead hook) → breakdown
  (kick out, chord swells, rising noise-wash, subdrop boom) → drop B
  (+ride 16ths, acid echo 8va) → outro. −14.8 LUFS integrated.
- **Artist:** Aesthetic Dot Computer (intended)
- **Groove + bass + harmony pass (2026-06-11, per @jeffrey):** soft kicks
  from bar 0 (+22 dB slow-attack → full thump by bar 8); deep bass =
  longer kick tail (115→38, 320 ms) + stacked kickSub body (52→40) +
  45 Hz master bell; swing on odd 16ths (SW=0.08) + eager hats / dragged
  claps + rests in the acid line; lead riff rides whole drops, lifts a
  5th in back halves. NEVER-SILENT floor: veil/drone/air beds as 8-BAR
  events — per-bar bed events notch −35 dB at every bar seam via the
  mono-cut (verified zero sub-−30 LUFS momentary windows; floor then
  trimmed ~3.5 dB per "too loud"). **Harmony channel:** engine `group`
  support = second independent noise pair, residues sum in PARALLEL (no
  serial dB collision) — voice-led Am/F/C/G pads + top-voice double,
  exactly 32/32 points.
- **Next:** @jeffrey ear-check → tune acid pattern/drops by ear.

---

### Mastering note (learned on trancenwaltz)

The trance.mjs master chain renders **dark** (~16 dB roll-off into the
highs) — sine-heavy synth mix. Ship-ready masters need a brightening
polish pass: high-shelf air @ ~8.5 k (+4), presence @ ~4.2 k (+2),
sparkle @ ~12.5 k (+2), slight 190 Hz mud trim, `loudnorm I=-14
TP=-1.2 LRA=13`. Brightness costs LRA (10.5 → ~6.7) — acceptable for a
loud modern dance master; offer a −15 LUFS / wider-LRA variant if more
dynamics are wanted. Always A/B against the pre-bright take.
