# Special Sign

Locked master package for the release-length spatial-sine work.

- Artist: Aesthetic Dot Computer
- Album/body: `pixsies`
- Master locked: 2026-07-24
- Duration: 1:41.375
- Tempo/key: 76 BPM, C major
- Source lane: `pop/nullabye/`

## Files

- `special-sign-MASTER.wav` — canonical 48 kHz, 24-bit stereo master.
- `special-sign.mp3` — 320 kbps listening/distribution copy with ID3 metadata
  and the front cover embedded.
- `special-sign-cover-3000.jpg` — canonical 3000×3000 *Attic Gremlin*
  colored-pencil cover: Jeffrey live-coding beside the spatial sine globe.
- `special-sign.scorodeon.json` — the release-trimmed graphic score: 2,009
  engine events plus 35 Jeffrey-choir events across thirteen lanes.
- `special-sign-graphic-score.mp4` — synchronized 2160×2160, 30 fps moving
  score with the locked master.
- `special-sign-3d-spatial-graph.mp4` — synchronized first-person 3D graph of
  the twelve physical bodies, listener path, emitted waves, and mastered
  stereo receiver plane.
- `special-sign-circular-score.mp4` — the radial cover rotating beneath a
  fixed receiver needle, with the final motion-dependent wet field.
- `special-sign.automation.json` — sample-timeline receipt for the final
  speed-to-wet mapping and its deliberately dry motion pocket.
- `special-sign.provenance.json` — authored spatial, choir, and mastering
  decisions from the accepted render.
- `release.json` — release lock, measured QC, and SHA-256 receipts.
- `distrokid/` — clean upload folder containing only the delivery WAV, cover,
  and upload notes.

## Master lock

The accepted source is `Special-Sign-SLIGHTLY-DRIER-AUDITION`. It keeps one
intact direct master and adds only a filtered antisymmetric spatial field. The
authored spiral speed drives wetness through a 1.10 convex curve: ordinary
motion becomes clearer while the 97% super-speed crest remains 96.7% wet. The
low-velocity turn near 24 seconds is held to a 3.5% ceiling before the curve,
and the material-kick sidechain follows the same motion so that dry passages do
not acquire an artificial amplitude hole.

Fifty four-times-oversampled `material kick` strikes key the whole mix and are
returned dry and centered. Five reverse-kick intakes and five alternating wide
reverse bells begin only after the quiet turn. The lead score includes variable
velocity, soft attacks, longer decays, sparse doubles, grace notes, chord
extensions, and motion-dependent arpeggio decoration. The added spatial return
cancels in mono before the final linear loudness trim.

> The WAV in this directory is canonical. A fresh engine render is a new
> audition and must not silently replace it, even when made from the same score.

## QC

- Integrated loudness: −15.0 LUFS
- Loudness range: 4.9 LU
- True peak: −1.47 dBTP
- Mono fold-down: −18.8 LUFS / −4.2 dBTP
- Clipping, NaN, Inf, and denormal samples: none detected
- MP3: 48 kHz stereo, 320 kbps, cover and release tags embedded
- Graphic-score MP4: 2160×2160 H.264 + 48 kHz stereo AAC, exactly 1:41.375
- 3D spatial graph: 720×720 H.264 + 48 kHz stereo AAC, exactly 1:41.375
- Encoded frames inspected at opening, super-spin, and final run-down in both
  visual systems

The central 16-second, eight-turn rotation reaches a 96.7% wet crest. Outside
the speed peaks, the convex mapping keeps the direct sine bodies forward and
reduces the earlier wavy/hazy impression. The mono fold remains clear and
peak-safe because the spatial return is strictly antisymmetric; the mastering
kick and reverse gestures are intentional centered/stereo material.

## Graphic score

The release cover is the selected *Attic Gremlin* illustration, built from the
offline acoustic-glass system, Jeffrey identity references, and the physical
Neo reference in the project colored-pencil house style. The circular-score
art remains the basis of the synchronized score video rather than the release
cover.

`bin/special-sign-scorodeon-data.mjs` compiles the C score without rendering
audio, trims it to the locked master, adds the Jeffrey vowel events and actual
master RMS arc, and emits `special-sign.scorodeon.json`. The shared
`pop/bin/scorodeon.mjs` renderer moves those events through a fixed red
playhead. `bin/render-special-sign-score-cover.mjs` wraps the same complete
dataset clockwise into the circular motion piece.

`special-sign-3d-spatial-graph.mp4` is the engine's first-person receiver view
over the same release interval. The C renderer places the release-length master
back onto its full source timeline at bar 6, so the windshield waveforms and
source impacts remain sample-aligned while the authored 3D bodies continue to
drive their own motion, shells, distance, Doppler, HRTF, and gravity telemetry.
