# pop/blackboard — build checkpoint

Working notes for resuming an interrupted build. The finished story lives in
README.md; this file is the scratch ledger.

## Stage ledger

- [x] `bin/score.mjs` — 72 BPM, E minor, 48 bars (~2:44), 14 sung line
  entries (12 unique TTS texts; the hook appears at bars 20/32/40).
- [x] `bin/render-bed.mjs` → `out/blackboard-bed.wav` (164.0 s, stereo f32).
  Section RMS: intro -32.6 / verse -23.9 / pre -22.5 / hook1 -18.0 /
  bridge -26.9 / hook2 -17.6 / outcome -22.6 / hook3 -17.5 / outro -30.6 dB.
- [x] `bin/sing.mjs` — vocal chain. CHUNKED: run
  `node pop/blackboard/bin/sing.mjs --prep 1` repeatedly (each run renders
  ONE uncached line, caches it under `out/sung/blackboard/words/
  line-<li>-qa.json` + sung/lead wavs, exits). When all 14 lines are
  cached, a plain `node pop/blackboard/bin/sing.mjs` reuses them all and
  runs assembly → stems → mix → master → QA sidecars.
- [x] `bin/texture.mjs` — Ingold whisper texture stem (NON-RELEASABLE) +
  alt mix. Source span ≈ 35.4–39.6 s of
  `papers/lines-platter/sources/ingold-blackboard-clip.wav`
  ("when you stand at the blackboard and you scrape a line").
- [x] README.md — full chain doc + rights note.
- Venv gotcha: `pop/.venv` lacked scipy (vocal_bus.py import) — installed
  via `uv pip install --python pop/.venv/bin/python scipy` (no pip in venv).

## Chain settings

- harmony lock 0.875 · stability 0.55 · QA passes 3 + clarity 2 ·
  WER gate 0.25 (logged, not a blocker) · continuity ≥ 0.95 ·
  f0 60–300 Hz · octave_opt on · choir on · seed 7+li.
- registers: verses/bridge/pre1-2 +0; p3 "I'm arguing for." +7 (ladder 0);
  o1 outcome +7 (ladder 0); hooks +12 (ladder 7, 0).
- master: measured 2-pass loudnorm linear → -14 LUFS / -1 dBTP
  (spinging SING_TARGET), premaster click-scanned by vocal_bus.py.

## Gotchas already hit

- Run everything serially (8 GB); chunk long renders; never silent-wait.
- "PowerPoint" TTS'd as one word, scored as "power"+"point" (presplitHeard
  handles the welded whisper token).
- Bed and vocal both 164.0 s exactly; mix apad+atrim pins to bed length.
