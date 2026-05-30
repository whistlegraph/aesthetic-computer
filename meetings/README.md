# meetings

Phone / Mac call recordings → WhisperX transcripts → whistlepop-detected
section breaks and directives → arxiv-style LaTeX PDF.

## Pipeline

```
slab "Start Call"
  → slab-recorder (mic + system audio aggregate → WAV in ~/Documents/Shelf/meetings/)
slab "Stop Call"
  → meetings/cli.mjs ingest <wav>          → meetings/<slug>/audio.wav
  → meetings/cli.mjs transcribe <slug>     → transcript.json   (WhisperX)
  → meetings/cli.mjs detect <slug>         → whistlepops.json  (audio events)
  → meetings/cli.mjs parse <slug>          → directives.json   (DSL applied)
  → meetings/cli.mjs build <slug>          → meeting.pdf
```

Each step is incrementally cached by mtime, matching `papers/cli.mjs`.

## Whistlepop DSL

See `dsl.md` for the grammar. TL;DR — a single whistle is punctuation
(section break); two whistles bracket a spoken directive ("highlight",
"decision", "section budget", "action alex", …) that the renderer applies
to the surrounding conversation context.

## Status

- [x] Skeleton CLI (`cli.mjs`) with `new/ingest/list/transcribe/detect/parse/build/run/open`
- [x] Detector stub (`detect-whistlepops.mjs`)
- [x] DSL spec (`dsl.md`)
- [x] Whistlepop training corpus spec at `wave-wizard/samples/whistlepops/`
- [x] LaTeX template (`template/meeting.tex.tmpl` + `template/ac-meeting.sty`)
- [x] Build pipeline end-to-end (transcript+directives JSON → PDF), verified with synthetic data
- [x] Recorder (`slab/bin/slab-call-record`) + slab "Start Call" menubar item
- [ ] Capture training takes (jeffrey runs `swift run WaveWizard samples/whistlepops/spec.json`)
- [ ] Real `detect-whistlepops.mjs` (k-NN against the corpus)
- [ ] `transcribe` step (shell out to WhisperX, populate `transcript.json`)
- [ ] `parse-directives.mjs` (whistlepops + transcript → directives)

## Sample / reference output

`sample-jeffrey-x-scott/` — hand-crafted reference meeting that exercises
every directive type in the .sty (title block, key ideas card, speaker
turns, highlight, decision, action, quote, freeform, skipped, margin
note, whistle break, colophon). Compile with `xelatex meeting.tex` to
regenerate the reference PDF. Named without a timestamp prefix so
`cli.mjs list` skips it.

## Recording sources

Mac-side calls (Zoom, Meet, FaceTime, Discord) work via a BlackHole +
mic aggregate device — `slab-recorder` will detect whether one is wired
and warn if not. iPhone phone calls don't expose audio to macOS apps;
the workaround is speakerphone, and the recorder tags the resulting
meeting with `source: "speakerphone"` for awareness.
