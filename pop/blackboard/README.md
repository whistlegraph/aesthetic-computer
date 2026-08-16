# blackboard

jeffrey sings Tim Ingold's blackboard/PowerPoint passage from "Thinking
through Making" (Institute for Northern Culture lecture, 07:28–08:28 — clip
and transcript in [`papers/lines-platter/`](../../papers/lines-platter/)).
72 BPM, E minor, 48 bars, 2:44. The hook — "that's why I like blackboards /
and don't like PowerPoint" — lands at bars 20, 32, and 40.

Built bottom-up: the bed is synthesized in `bin/render-bed.mjs` (chalk-scrape
percussion via the TrackDrum friction voice ported from `pop/cult`, patient
E-minor pads, section dynamics intro→hooks ≈ −32→−18 dB RMS); the vocal is
@jeffrey's ElevenLabs voice (jeffrey-pvc, stability 0.55) lifted to singing
through the aesthetivox chain (`spinging/` — continuous WORLD line synthesis,
guided phoneme alignment, self-choir gated to vowels). No external
music-generation services.

## Chain

`bin/score.mjs` (notation: 14 line entries, registers verses +0 / "arguing
for" +7 / hooks +12) → `bin/sing.mjs` (per-line: ElevenLabs TTS → guided
alignment → WORLD synthesis at harmony lock 0.875 (β = 0.125), f0 clamp
60–300 Hz, octave fit on, per-line QA: percentile conformance, click scan,
whisper WER; then assembly → stems → consonant-duck mix over the bed →
two-pass measured loudnorm) → `out/blackboard.wav` / `.mp3` at
**−14.17 LUFS / −1.48 dBTP** (target −14 / −1, SING_TARGET).

Chunked rendering: `node bin/sing.mjs --prep 1` renders one uncached line and
exits (resumable); a plain run reuses the cache and finishes the track.
`bin/texture.mjs` builds the optional Ingold whisper stem and alt mix.

## QA

`out/blackboard-sung-qa.json` + per-line sidecars under `out/sung/`. All 14
lines pass conformance and the click scan (one advisory each on p3/b1).
Whisper end-to-end WER (tracked, not a blocker — sung+choir audio is
intrinsically whisper-hostile): 4/14 lines ≤ 0.25 strict; mix smoke
transcript stays mostly legible. Best lines: v4 and b2 at WER 0.

## Rights

- Lyrics are Ingold's spoken words (short lecture passage), sung by
  @jeffrey's own cloned voice — no lecture audio in the canonical master.
- `stems/ingold-texture-NONRELEASABLE.wav` and
  `out/blackboard-alt-texture-NONRELEASABLE.wav` contain 4 s of the actual
  lecture recording ("when you stand at the blackboard…") as an intro
  whisper. Unlicensed source: these two files must never ship to DistroKid
  or any release channel.
