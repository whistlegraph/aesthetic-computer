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

## trancenwaltzi — RENDER (not yet released)

- **Lane:** `pop/dance/` · chill mix (`--mode chill --meter 3`), ~3:00,
  photographic **Trader Joe's** calm-shopping concept w/ pixie crew
- **Status:** v17 square cut + v18 vertical IG-story cut rendering
  (1500×1500 + 1080×1920); pixies = all ages incl. kids→elders,
  casual-cyberpunk; post grade = hue-shift + bright greens + sharpen +
  smooth fade ending; lightbox/stained-glass backlight (face+laptop).
- **Outputs:** `~/Desktop/trancenwaltzi-iter.mp4` (square),
  `~/Desktop/trancenwaltzi-vertical.mp4` (IG-story)
- **Next:** review cuts → pick cover → master (apply the same bright
  polish learned from trancenwaltz) → DistroKid.

---

### Mastering note (learned on trancenwaltz)

The trance.mjs master chain renders **dark** (~16 dB roll-off into the
highs) — sine-heavy synth mix. Ship-ready masters need a brightening
polish pass: high-shelf air @ ~8.5 k (+4), presence @ ~4.2 k (+2),
sparkle @ ~12.5 k (+2), slight 190 Hz mud trim, `loudnorm I=-14
TP=-1.2 LRA=13`. Brightness costs LRA (10.5 → ~6.7) — acceptable for a
loud modern dance master; offer a −15 LUFS / wider-LRA variant if more
dynamics are wanted. Always A/B against the pre-bright take.
