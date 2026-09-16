# Sound toolkit

The first Easel Sound lane composes short instrumental phrases and loops using
Pop's own sinepower voice, wobble/bitcrush effects, and Bjorklund rhythms.
No samples, account, network, external executable, or personal voice assets are
required. Source modules in `pop/` are verbatim copies; `provenance.json` records
the exact original paths and SHA-256 hashes. The Easel repository license applies.

`score.json` is the editable recipe:

```json
{
  "bpm": 100,
  "beats": 8,
  "instrument": "sinepower",
  "preset": "stab",
  "loop": false,
  "notes": [
    { "at": 0, "midi": 60, "duration": 1, "gain": 0.3 },
    { "at": 2, "midi": 67, "duration": 1, "gain": 0.3 }
  ],
  "effects": [{ "type": "wobble", "rate": 2, "depth": 0.4 }]
}
```

Times are beats (quarter notes). Presets are `lead`, `pad`, and `stab`.
`set_score` validates and renders a full recipe. `render` reads the saved recipe.
`rhythm` replaces notes using `{ "hits": 3, "pulses": 8, "midi": 60 }` and enables
looping. `analyze` measures the saved WAV without changing its audio.

Outputs are 24 kHz mono 16-bit PCM `sound.wav`, `analysis.json` (duration, actual
PCM RMS/peak, 256-bin min/max waveform), and render provenance recording recipe
and WAV hashes plus gain attenuation. Versions must preserve all four outputs.
The shared project layer owns revision history and rollback.

Rendering is deterministic. Clips are capped at 30 seconds, 128 notes, four
effects, and 180 seconds of accumulated voice windows. Peak attenuation limits
the mix to 0.95; quiet work is not automatically amplified. Non-loop phrases get
10 ms edge fades. Loop release tails wrap into the start; the loop length is
exactly the declared beat duration. Modulation rates may still cause a loop seam.
Playback must remain user-controlled. No auto-play occurs in this adapter.

This slice has no sample import, recording, multitrack editing, loudness mastering,
or hosted sound generation. RMS/peak are sample measurements, not LUFS/true peak.

To refresh bundled source, copy each `provenance.json` source to its recorded
`file`, regenerate its SHA-256, review upstream changes, then run:

```sh
node --test easel/test/sound.test.mjs
```
