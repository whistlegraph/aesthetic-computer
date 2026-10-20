# yc-2018

@jeffrey's 2018-10-01 Y Combinator intro video, re-sung as a 120 BPM deep house
track with a lit room and beat captions. Not posted. Its working caption is `2018.10.01`.

Only the scripts live here. Every script runs from one work dir that holds `YC.mov`
(from ~/Library/Mobile Documents/com~apple~QuickTimePlayerX/Documents/) and
`jeffrey-sung.mp3` (blueberry, ~/Music/…/Unknown Album/), and writes there.

1. whisper (`ggml-small.bin`) on both takes → `spoken.json`, `sung.json`, `sung-words.json`
2. `node bin/regrid.mjs 120` → `vox-grid.wav`, `words-grid.json`, `grid2.json` (rubberband R3 timemap)
3. `node bin/voiced.mjs sung` / `spoken` → `words-voiced.json`, `spoken-voiced.json` (word edges tightened to the voice)
4. `node bin/deep.mjs`, then `node bin/real.mjs` → `deep/` stems (synths + FluidSynth GeneralUser GS)
5. `zsh bin/mix2.sh kick=-16 sub=-23.5 bass=-23 ep=-23` → `mix2/master-real.wav` (−10.7 LUFS)
6. `marketing/talking-head/bin/warp-to-sing.mjs YC.mov --spoken spoken-voiced.json --sung words-voiced.json
   --audio mix2/master-real.wav --source-sync 95 --mouth-lead 100 --continuous` (held words play forward only,
   since `--min-rate` boomerangs rock the head)
7. matte with `pop/sailor-song/bin/vision-matte.swift` (swiftc), then `bin/relight4.py warped.mp4 matte.mp4 lit.mp4`
8. `marketing/talking-head/bin/reel.mjs lit.mp4 --song … --words words-voiced.json --no-stamps --no-captions`
9. `NO_BALL=1 node bin/beatcaps.mjs nocap.mp4 caps.mp4`, then mux with `aac_at` (ffmpeg's native aac overshoots peaks)

The last cut was v5: forward-only holds, no bouncing ball.
