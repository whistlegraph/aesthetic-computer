# cornerfive

A /pop remix built around the whistlegraph **"five in the corner x2"**
([tiktok](https://www.tiktok.com/@whistlegraph/video/7651341048442047775)).
The whistle *is* the song — kick + a sine-bell "sister" line + the vocal,
nothing else.

## Source

- `src/whistle.wav` — the whistle stem (mono 44.1k, pulled via
  `toolchain/whistlegraph/grab.mjs`, gitignored)
- `src/whistle.analysis.json` — librosa readout (tempo, onsets, the
  observed melody with per-note pitch + cents)

## Pipeline

```bash
node bin/sync-whistle.mjs    # beat-align + shift the whistle → src/whistle-synced.wav
node bin/cornerfive.mjs      # kick + sister bells + looped vocal → out/cornerfive.mp3
```

**1. `sync-whistle.mjs`** — keeps the whistle's **own observed melody**
(no key-snapping, no WORLD resynth). Per note it uses rubberband
(pitch-preserving) to:
- **beat-align** — stretch each onset onto the 16th-note grid
- **shift** — nudge each note into tune by its detected cents

Writes `src/whistle-synced.wav` + `src/whistle-synced.notes.json` (the
melody the sister line follows).

**2. `cornerfive.mjs`** — renders `out/cornerfive.mp3`, **1:24** at
**73.2 BPM**:
- **kick** — four-on-the-floor (+ a push on the "and of 3"), whole track
- **sine bells** — pure-sine bell + soft octave-down sub, playing the
  vocal's exact melody like a "sister", read from the notes sidecar
- **vocal** — the synced whistle looped end-to-end through the whole
  track, high-passed + bright, light reverb, sidechain-ducked under the
  kick
- `--raw` falls back to the untouched original whistle recording

## Notes

`out/` and `src/*.wav` are media (gitignored, reproducible from the
whistlegraph URL + the scripts); the `*.json` melody/analysis sidecars and
the code are the source of truth.
