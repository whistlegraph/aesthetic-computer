# momabobasheep — YouTube Listener Video (10:00 landscape)

**Status:** SPEC (proposed 2026-06-15) · **Target:** one 1920×1080 / 30fps
landscape video for the full 10:00 master, uploaded to the AC YouTube channel
as the canonical "play the whole track" page.

> **DECIDED (@jeffrey, 2026-06-15):**
> - **Build path:** *dry run first* → then the **LUSH pass** (9 distinct
>   movement scenes, motion throughout). Both done — see §3.5 and §3.6.
> - **Score ribbon:** quiet ambient, **and NO labels on the tracks** — the
>   scrolling score is purely abstract felt-tinted notes gliding past the
>   playhead, with *zero per-note text and no chord glyphs riding on the
>   ribbon*. Text is confined to the sparse corner chrome only (title /
>   movement name / progress), and even that stays minimal.
> - **Scene approach:** 9 distinct felt dioramas, one per movement, cross-cut
>   on the music's movement boundaries (not the single evolving bench scene).

This is the **landscape twin** of the released 9:16 promo set
(`momabobasheep-story.mp4` / `-reel.mp4`). Those are 56-second vertical
highlight reels for Reels/TikTok. This is the long-form *listener* video: the
whole sleep mix, designed to be left running, that earns its watch-time from a
**tasteful, quiet score overlay** rather than a wall of generated motion.

The core constraint @jeffrey set: *the track is 10 minutes and video gen is
expensive, so overlay the track data tastefully with few labels, build out
still images, and reserve Seedance for where it actually counts — don't break
the bank.*

---

## 1. The aesthetic (carry the track's concept)

The track already has a narrative: **MoMA + boba + sheep** — jeffrey goes to the
museum, drinks a boba, and falls asleep counting sheep in the galleries —
told as a **9-movement Fibonacci-arch night** (drift → settle → sink →
deepening → dreaming → deep dream → **THE DREAM** (climax, ~golden section) →
strange REM → dawn). Medium is **needle-felt wool** (`mediums.mjs →
NEEDLE_FELT_WOOL`), the same felted-diorama look as the cover and reel.

The listener video should feel like **watching that felt night breathe for ten
minutes**, with the score gliding by underneath like the tape of a music box.
Warm/dark, never bright (matches the −16 LUFS sleep master and the
"no brightening pass" mastering call).

---

## 2. Composition — three quiet layers

A listener video is not a motion reel. It is **mostly still, gently alive**.
Three stacked layers, in back-to-front order:

### Layer A — Background: 9 felt movement stills (FREE, the workhorse)
One still per movement (felt diorama in the cover's style), drifting under a
slow **Ken Burns** (`preview-shared.mjs → drawCoverKenBurns`: baseScale ~1.15,
18s breath, multi-freq wobble) and **cross-dissolving** at each movement
boundary. This alone carries ~8 of the 10 minutes with zero video-gen spend.

Stills are new landscape gpt-image-2 gens (1536×1024) via a `gen-stills.mjs`
adapted from `bin/gen-illy.mjs` — the existing 8 reel panels are 9:16 and
center-cropping them to 16:9 loses the diorama, so generate fresh landscape
beats. Story beats map cleanly to movements:

| # | Movement (night)      | Still (MoMA-boba-sheep beat)                  | Motion budget |
|---|-----------------------|-----------------------------------------------|---------------|
| 1 | drift                 | arrival — felt jeffrey at the museum doors    | still + KB    |
| 2 | settle                | boba sip in the lobby                         | still + KB    |
| 3 | sink                  | drifting the galleries, eyes heavy            | still + KB    |
| 4 | deepening             | waterlily room, awe                           | **loop**      |
| 5 | dreaming              | first sheep grazing between the paintings     | **loop**      |
| 6 | deep dream            | the flock, asleep on a felt bench             | still + KB    |
| 7 | **THE DREAM** (climax)| spilled-boba ASCENSION — the dream itself     | **hero shot** |
| 8 | strange REM           | melting galleries, sheep adrift               | **loop**      |
| 9 | dawn                  | morning light, the cup empty, resolve home    | still + KB    |

### Layer B — Motion accents: short Seedance loops (BUDGET-CAPPED)
Reserve Seedance for the **4 marked rows** above. Each is a short clip
**ping-pong looped** (`build-canvas.mjs` already does this for the 8s Canvas) to
fill its whole movement seamlessly — a 5s felt scene ping-ponged = a 10s
breathing loop that tiles under any movement length, and slow looping reads as
*intentional* at sleep tempo, not as a GIF. The climax (row 7) gets one longer
**hero shot** that plays once, un-looped, as the visual payoff. Generated
through the existing `pop/lib/motion-pipeline.mjs` + `fal.mjs` (fast
tier, 720p) and assembled the same way as the reel.

### Layer C — The score overlay ("the video score", tasteful, few labels)
This is your scorodeon idea, re-pitched for landscape and **dialed way down**.
`bin/scorodeon-data.mjs` already turns `events.json` → `scorodeon.json`, and
`pop/bin/scorodeon.mjs` renders the score flying past a centerline. For a
listener video the score should be **ambient, not the star**:

- A **thin horizontal score ribbon** (notes scrolling right→left past a fixed
  playhead) across the lower third, at low opacity (~35–45%), tinted to the
  current movement's felt palette. Landscape orientation = horizontal scroll,
  unlike the vertical 4K scorodeon.
- **Few labels, set sparse and small** (YWFT, via `preview-shared.mjs`
  typography helpers):
  - top-left: **momabobasheep** title (static, small)
  - top-right: **current movement name** (cross-fades at boundaries — "the
    dream", "dawn"…)
  - the **current chord** glyph near the playhead (F / Dm / Bb / C / Gm / Eb),
    pulled from `struct.json` chords
  - a **slim segmented progress bar** + timecode along the very bottom edge
    (reuse the story-variant chrome)
- Optional: a faint **figure backlight** (`drawFormBacklight`) pulsing on the
  audio RMS envelope so the felt scene "breathes" with the marimba.

That's the whole label set — four text elements + chord + progress. Nothing
karaoke, nothing busy. The data is *present and legible* but stays a quiet
instrument-panel under the felt.

---

## 3. Budget — three tiers (the whole point)

Seedance 720p: **fast $0.2419/s · standard $0.3024/s** (`fal.mjs`
`RATE_PER_SEC`). gpt-image-2 stills are cents each. Pick a tier:

| Tier | Seedance | Billed sec | Seedance $ | Stills | All-in | Feel |
|------|----------|-----------:|-----------:|-------:|-------:|------|
| **A — minimal** | none (Ken Burns over 9 stills + reuse paid reel shots as accents) | 0 | $0 | ~9–12 imgs | **~$3** | calm, still, score-led |
| **B — recommended** | 3 loops ×5s + 1 hero ×10s, fast | 25s | **~$6** | ~12–15 imgs | **~$9–10** | felt breathes at 4 key beats, climax pays off |
| **C — lush** | loop per movement (8×5s) + hero ×12s, fast | 52s | **~$13** | ~15 imgs | **~$17** | motion throughout, still loop-cheap |

For reference, a naïve "generate all 600s" approach would be **~$145 (fast) /
~$181 (standard)** — so even Tier C is ~10× cheaper by leaning on stills +
ping-pong loops + the score overlay for perceived motion.

**Recommendation: Tier B.** Ken Burns + the scrolling score carry the runtime;
Seedance lights up only the waterlily, first-sheep, REM-melt, and the dream
climax. ~$10 all-in for a 10-minute video.

---

## 3.5 Dry-run result (2026-06-15)

Built `bin/chrome-listener.mjs` (the landscape twin of `chrome-reel.mjs`) and
rendered a Tier-A dry pass — **$0 spend**: the *existing* `momabobasheep-cover.png`
as the background still, the *existing* master audio, and the *existing*
`momabobasheep.scorodeon.json` score data. No new stills, no Seedance.

It works, and the cover already nails the concept: jeffrey asleep on a museum
bench, Monet waterlilies behind, boba + glasses + felt sheep in the foreground.
- Background felt diorama on a slow ken-burns drift (preview-shared).
- Quiet **label-free** score band in the lower third — every melodic lane
  merged into one felt-tinted note ribbon (pitch→Y, lane→color, gain→height)
  gliding past an amber centerline that breathes on the RMS. No per-note text,
  no chord glyphs, no lane chips — exactly as decided.
- Sparse corner chrome only: title (top-left), current movement w/ 1.2s
  cross-fade at boundaries (top-right), progress bar + clock (bottom).

Outputs: `out/listener-frame.png` (frame @360s, the "vivid" climax) and
`out/momabobasheep-listener-dryrun.mp4` (30s, 432–462s, crossing vivid→morning).
Both copied to `~/Desktop`. Render config: 1920×1080@30, `--ss 1` for a fast
dry pass (final bumps to `--ss 2`).

**Commands:**
```bash
node pop/momboba/bin/chrome-listener.mjs --frame 360            # one PNG
node pop/momboba/bin/chrome-listener.mjs --window 432-462 --dryrun  # 30s clip
node pop/momboba/bin/chrome-listener.mjs --ss 2                 # full 10:00 (final)
```

**Open look-notes for @jeffrey:** the ribbon currently overlaps the foreground
felt sheep/bench and reads a touch muddy there — easy knobs: lower `--ribbon`
opacity, drop the band lower, or thin it. Tune after you watch.

## 3.6 Lush pass — BUILT (2026-06-15)

9 distinct felt movement scenes, motion throughout. Total fal spend **~$12.6**
(8 movement loops × 5s + the-dream hero × 12s = 52s billed @ fast 720p).

Pipeline, all on this lane:
- **`pano/`** — `PREAMBLE.txt` + 9 beat prompts (`01-arrival` … `09-resolve`),
  one per movement, landscape framing.
- **`bin/gen-panels-listener.mjs`** — 9 stills @ 1536×1024 (gpt-image-2, jeffrey
  refs) → `pano/out/NN-*.png`. ~2.7 min each on the 8GB machine.
- **`bin/gen-motion-listener.mjs`** — Seedance 2.0 (fal) over the 9 stills, 16:9
  720p, all cuts. SHORT generation struct (5s / hero 12s) decouples paid clip
  length from the long movement playback. → `out/motion/…-shot-N-name.mp4`.
- **`bin/assemble-listener.mjs`** — ping-pong + tile each clip to its real
  movement length (from the scorodeon movement boundaries), hard-cut at
  boundaries → `out/listener-plate.mp4` (600.0s, 1920×1080, silent).
- **`bin/chrome-listener.mjs --plate <plate>`** — reads plate frames via an
  ffmpeg RGBA pipe as the moving background, draws the quiet no-label score +
  corner chrome, muxes the master → `out/momabobasheep-listener.mp4`.

**Gotchas hit + fixed:**
- the-dream (close-up of jeffrey's felt face) tripped fal's
  `content_policy_violation` ("likenesses of real people"). Fix = re-gen that
  still **face turned away / tucked into his arm, seen from above-behind** (the
  faces-fail-moderation rule); it passed and looks better. After a re-roll,
  **delete the stale `…mp4.queue.json`** or the pipeline resumes the old failed
  job instead of submitting the new image.

**Final render command:**
```bash
node pop/momboba/bin/gen-panels-listener.mjs        # 9 stills
node pop/momboba/bin/gen-motion-listener.mjs        # Seedance (~$12.6)
node pop/momboba/bin/assemble-listener.mjs          # → listener-plate.mp4
node pop/momboba/bin/chrome-listener.mjs --plate pop/momboba/out/listener-plate.mp4 --desktop
```

## 4. Build pipeline (reuse what exists)

Almost everything is already in the lane or `pop/lib/`. New work is one stills
generator and one landscape-chrome compositor; the rest is configuration.

```
1. node pop/momboba/bin/gen-stills.mjs          # NEW (fork gen-illy.mjs)
   → out/stills/momabobasheep-mov-{1..9}.png     # 1536×1024 landscape dioramas

2. node pop/momboba/bin/scorodeon-data.mjs       # EXISTS
   → out/momabobasheep.scorodeon.json            # already generated

3. node pop/momboba/bin/gen-motion-listener.mjs  # NEW (fork gen-motion-…-reel.mjs)
   → out/motion/listener/<movement>-shot.mp4      # Tier-B: 4 shots, fast/720p
   (uses pop/lib/motion-pipeline.mjs + fal.mjs; takes board via
    pop/bin/audition-motion.mjs to pick the good roll before assembling)

4. node pop/momboba/bin/chrome-listener.mjs       # NEW (fork chrome-reel.mjs)
   --variant listener                              # landscape 1920×1080@30
   → out/momabobasheep-listener.mp4                # full 10:00, audio muxed
   (Ken Burns stills + ping-pong motion loops + horizontal score ribbon +
    sparse YWFT labels + progress/timecode + RMS backlight, via preview-shared.mjs;
    encodes through spawnFFmpegEncode, BGRA stdin, x264 -crf 20)

5. node toolchain/youtube/yt.mjs upload \          # EXISTS
     pop/momboba/out/momabobasheep-listener.mp4 \
     --title "Momabobasheep" \
     --description-file pop/momboba/out/momabobasheep.youtube.txt \
     --tags "ambient,sleep,chillwave,marimba,aesthetic computer,pixsies" \
     --category 10 --privacy public \
     --thumbnail out/momabobasheep-cover-thumb.jpg
   → writes momabobasheep-listener.youtube.json receipt (videoId, watch URL)
```

**New scripts (3):** `gen-stills.mjs`, `gen-motion-listener.mjs`,
`chrome-listener.mjs` — each a fork of an existing sibling, so no new
infrastructure. Everything else (scorodeon data, Seedance client, motion
pipeline, preview-shared chrome primitives, ffmpeg encode, YouTube upload)
already exists and is proven on this lane.

---

## 5. YouTube metadata

Per the RELEASES.md convention (Title Case title, hashtag-only description):

- **Title:** `Momabobasheep`
- **Description file** (`out/momabobasheep.youtube.txt`): one line of hashtags —
  `#ambient #sleep #chillwave #marimba #aestheticcomputer #pixsies`
  (optionally a single line above it: *"a ten-minute sleep mix — jeffrey goes
  to the MoMA, drinks a boba, and falls asleep counting sheep."*)
- **Category:** 10 (Music) · **Privacy:** public · **Made for kids:** no
- **Thumbnail:** downscale the 3000² cover → ≤2MB jpg
- **Chapters** (nice freebie for a 10-min video — YouTube auto-detects
  `0:00 title` timestamps in the description): emit them from `struct.json`
  movement boundaries, e.g. `0:00 drift · 1:12 settle · … · 6:0x the dream`.
  This doubles as on-page navigation and reinforces the night arc.

---

## 6. Open questions for @jeffrey

1. **Tier?** Recommending **B** (~$10). A is ~$3 and pure stills; C is ~$17.
2. **Score ribbon prominence** — quiet ambient ribbon in the lower third (my
   default), or a touch more present (taller/brighter)? "Few labels" says keep
   it quiet.
3. **Stills count** — 9 (one per movement) is the clean mapping. Want fewer
   (e.g. 5 hero stills cross-dissolving) to save a couple gpt-image-2 cents and
   tighten the look?
4. **Privacy on first upload** — `unlisted` to review the 10-min render before
   going `public`? (Cheap insurance for a long render.)
5. **Chapters in description** — yes/no on the auto-chapter timestamps.

Nothing here is built yet — this is the plan. Say the tier and I'll scaffold the
three fork scripts and do a Tier-A dry run (stills + score, zero Seedance spend)
so you can see the look before we spend anything on motion.
