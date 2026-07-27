# squeakysines

An adversarial test-case song for the squeak heard in the opening sine tones of
**Special Sign**. It uses the production procedural HRTF (`../../c/ac_hrtf.h`)
and turns the signal-chain ablation into the arrangement.

```sh
node pop/nullabye/variants/squeakysines/render.mjs
```

Outputs land in `pop/nullabye/out/review/variants/squeakysines/`:

- `squeakysines.wav` / `.mp3` — 48-second diagnostic composition.
- `squeakysines-residue-exploit.wav` / `.mp3` — 72 seconds of only the
  HRTF-minus-clean residue. Motion rises every eight seconds; octave layers
  enter in the final sixteen seconds.
- `probe-raw.wav` — static equal-power sine carrier.
- `probe-clean.wav` — moving clean pan and distance only.
- `probe-blend.wav` — the release's 76% clean / 24% HRTF spatial blend.
- `probe-hrtf.wav` — procedural HRTF at 100% for an exaggerated check.
- `probe-residue.wav` — HRTF minus clean field, with makeup gain.
- `probe-full.wav` — release blend plus moving propagation delay.

The phrase repeats the first six lead pitches and timing profile at the release
entrance. All motion curves are smooth; a bright chirp that appears only in the
HRTF or residue probes therefore points into the binaural filter rather than a
stepped oscillator or position control.
