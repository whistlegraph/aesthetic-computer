# Narrator Wizard

A native macOS line reader and take recorder for replacing generated narration
with a human performance. It opens a JSON screenplay, shows one line and its
reference frame at a time, records 48 kHz mono WAV takes, and writes a resumable
`manifest.json` containing every take and the selected take for each line.

```bash
narrator-wizard/bin/narratorwizard path/to/narration-spec.json
```

Keys: `R` records, `S` stops, `P` plays, and Return keeps the current take and
advances. Closing the window is safe; the next launch resumes at the first line
without a kept take.

The input menu lists live CoreAudio capture devices. After hot-plugging an
interface such as a Focusrite, press **Inputs**, select it, and record. Optional
input monitoring is off by default and should be used with headphones. The
wizard restores the input that was selected before launch when it closes.

Appearance follows macOS Auto/Light/Dark and updates while the app is open.

When a screenplay supplies a `video` path, **Play current cut** opens that video
inside Narrator Wizard using native AVKit playback, scrubbing, volume, and
full-screen controls. This lets the narrator review the existing edit without
leaving the recording session.

The Social Software reel is already wired:

```bash
narrator-wizard/bin/narratorwizard marketing/sosoft-reel/narrator-spec.json
node marketing/sosoft-reel/use-human-narration.mjs
node marketing/sosoft-reel/render-realtime-spine.mjs
node marketing/sosoft-reel/caption-and-mix.mjs
node marketing/sosoft-reel/export-delivery.mjs
```
