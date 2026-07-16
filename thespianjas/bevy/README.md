# thespianjas · Bevy Metal studio

Native Rust presentation studio for the canonical Jeffrey digital twin.

```sh
cargo run --manifest-path thespianjas/bevy/Cargo.toml
```

- left drag: orbit
- right drag: pan
- scroll: zoom
- W/A/S/D: move camera
- Q/E: lower/raise camera
- Shift: move faster

On macOS Bevy's `wgpu` renderer selects the Metal backend.

The current performer pass plays the local talking-head voice track and drives
the model's `Head`, `neck`, and `Spine02` joints from a 30 Hz RMS speech
envelope. Meshy v001 has no jaw or viseme blendshapes. Version v002 must pass
`bin/inspect-face-rig.mjs` and a multi-pose contact sheet must pass
`bin/validate-visual.mjs` before it becomes the default performer.
