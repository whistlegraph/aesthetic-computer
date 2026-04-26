# Recap

Generates narrated, captioned video recaps of monorepo activity for a chosen
audience (currently `fia`, jas's girlfriend; trivially extendable to others).
The audio is the source of truth — whisper word-level timestamps drive slide
durations, so visuals stay in sync with what the voice is actually saying.

The default voice is `jeffrey-pvc` (the same Professional Voice Clone used in
the `say` piece and the LACMA grant pitch video), called via `/api/say` on
production.

## Pipeline

```
audience/<name>.mjs            (narration + segment markers + slide HTML/queries + voice + transcriptFixes)
       │
       ▼  bin/tts.mjs
out/recap.mp3                  (jeffrey-pvc TTS via /api/say)
       │
       ▼  bin/transcribe.mjs   (whisper-cli, models/ggml-base.en.bin)
out/words.json                 ([{text, fromMs, toMs}, ...])
       │
       ▼  bin/align.mjs        (matches audience.segments[].marker)
out/segments.json              ([{name, startSec, endSec, durationSec}, ...])
       │
       ▼  bin/scout.mjs        (resolves per-slide content queries; pdftoppm for PDFs)
out/assets.json                (slide-name → {queryName: dataUrl|commits|paths})
       │
       ▼  bin/slides.mjs       (puppeteer + ywft-processing + purple-pals + scouted assets)
out/slides/*.png               (1080×1920 PNG per segment)
out/concat.txt                 (ffmpeg concat demuxer w/ real durations)
out/duration.txt               (total seconds, including trailing silence)
       │
       ▼  bin/subtitles.mjs    (chunks words.json, applies transcriptFixes, renders pill PNGs)
out/subs/*.png                 (1080×220 transparent subtitle pill per chunk)
out/subs.json                  ([{file, startSec, endSec, text}, ...])
       │
       ▼  bin/build-filter.mjs (emits filter graph: showwaves + drawbox + per-sub overlay)
       ▼  bin/compose.fish
out/recap.mp4                  (1080×1920, h264 + aac, faststart, baked subs)
```

Run end-to-end:

```fish
./pipeline.fish fia            # fresh tts + everything
./pipeline.fish fia --skip-tts # reuse existing out/recap.mp3 (re-align/re-render)
```

First run only — download the whisper model (~141 MB):

```fish
curl -L -o models/ggml-base.en.bin \
  https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin
```

## Architecture decisions

- **Audio is the source of truth.** Slide durations come from whisper word
  timestamps, not from hand-tuned guesses. Re-recording the audio (e.g. a
  re-edit of the narration) automatically retimes the visuals.
- **Markers are anchor phrases**, not paraphrases. Each `audience.segments[]`
  has a `marker` field that must appear in the narration verbatim (modulo
  whisper transcription quirks — match is case-insensitive and punctuation-
  stripped). `align.mjs` fails loud if any marker is missing.
- **End card sits in trailing silence.** The last segment uses a synthetic
  `__END__` marker; the audio is padded with `apad` so the silent end card
  has time to breathe without truncating the narration.
- **Slides are HTML rendered by Chrome.** Reuses the `oven/` puppeteer install
  to avoid taking on a new dep. ywft-processing-bold/regular fonts are
  inlined as base64; `unicode-range: U+0020-007E` constrains the AC font to
  ASCII so Chrome falls back to system fonts for `ñ`, `中文`, `日本語`,
  `·`, `×`, etc.
- **Progress bar is `drawbox` with `w='iw*t/$TOTAL'`.** This ffmpeg build
  lacks `drawtext` and `subtitles`, so visible captions live in the slide
  PNGs; only the bar (no text) is composited at runtime.

## Content queries (scout)

Slide bodies can be **functions** of resolved query results. `scout.mjs` runs
every query declared on a slide and writes data URLs / commit lists / file
paths into `out/assets.json`. The slide function then receives those values
and produces HTML.

Three query shapes are supported:

| Shape                                                                | Result                                                    |
| -------------------------------------------------------------------- | --------------------------------------------------------- |
| `{ glob: "<path>" }`                                                 | base64 data URL of the first matching image (PNG/JPG/WebP/SVG) |
| `{ glob: "<path>.pdf", pdfPage: 1, pdfWidth: 600 }`                  | base64 data URL of one PDF page rendered via pdftoppm     |
| `{ commits: "<git -E grep regex>", since: "48 hours ago", limit: 5 }` | `[{hash, subject}, ...]` from `git log --grep -E`         |
| `{ files: "<glob>", sinceHours: 48, limit: 60 }`                     | matching paths newer than N hours, sorted newest first    |

Globs are repo-relative or absolute. PDF rendering uses 150 DPI by default;
`pdfWidth` scales the longer side. Commit grep is POSIX extended (`|`
alternation works without escaping). Failed queries log a warning and skip
the value; the slide function should defensively handle missing results
(e.g. `${(commits || []).map(...)}`).

Example slide entry in an audience config:

```js
"02_notepat": {
  queries: {
    icon: { glob: "ac-electron/build/icon.png" },
    paper: { glob: "system/public/papers.aesthetic.computer/notepat-26-arxiv-cards.pdf",
             pdfPage: 1, pdfWidth: 600 },
    commits: { commits: "^notepat|^build-notepat", since: "48 hours ago", limit: 5 },
  },
  body: ({ icon, paper, commits }) => `
    <div class="frame">
      <img class="brand-icon" src="${icon}" />
      <img class="paper-thumb" src="${paper}" />
      ${(commits || []).map(c => `<div>${c.hash} ${c.subject}</div>`).join("")}
    </div>`,
},
```

A slide entry can also still be a plain HTML string when no scouting is
needed (see `01_title`, `03_arena`, etc. in `audience/fia.mjs`).

## Subtitle transcript fixes

Whisper renders dictionary-style — `notepat` becomes `Notepad`, `baktok`
becomes `Backtalk`, `menubar` becomes `menu bar`. Fix per-audience without
re-running whisper:

```js
transcriptFixes: {
  "Notepad": "notepat",
  "Backtalk": "baktok",
  "menu bar": "menubar",
}
```

Match is case-insensitive and applied to each subtitle chunk's joined text
(so multi-word fixes like `"laid on Linux": "late on Linux"` work).

## Adding a new audience

Drop `audience/<name>.mjs` exporting `audience` (and a `PALETTE` if you want
to deviate from fia's). Required shape:

```js
export const audience = {
  name: "<name>",
  handle: "<optional handle for the corner bug>",
  voice: { provider: "jeffrey", voice: "neutral:0" },
  narration: "<verbatim text POSTed to /api/say>",
  segments: [
    { name: "01_title",  marker: "<phrase from narration>" },
    { name: "02_topic1", marker: "<phrase from narration>" },
    // ...
    { name: "10_end",    marker: "__END__", trailingSilenceSec: 3 },
  ],
  slides: { "01_title": "<html body>", /* ...one per segment */ },
};
```

Then `./pipeline.fish <name>`.

## Files

| File                     | Role                                                          |
| ------------------------ | ------------------------------------------------------------- |
| `audience/fia.mjs`       | narration, markers, slide HTML/queries, palette, fixes        |
| `bin/tts.mjs`            | POST narration → `/api/say` → `out/recap.mp3`                 |
| `bin/transcribe.mjs`     | `whisper-cli` → `out/words.json`                              |
| `bin/align.mjs`          | match markers in transcript → `out/segments.json`             |
| `bin/scout.mjs`          | resolve per-slide content queries → `out/assets.json`         |
| `bin/slides.mjs`         | puppeteer-render slide PNGs (consume assets) + `concat.txt`   |
| `bin/subtitles.mjs`      | chunk words into pills (apply transcriptFixes) → `subs.json`  |
| `bin/build-filter.mjs`   | emit ffmpeg filter graph for compose (one overlay per sub)    |
| `bin/compose.fish`       | ffmpeg compose final mp4                                      |
| `pipeline.fish`          | runs all six stages                                           |
| `models/ggml-base.en.bin`| whisper model (gitignored, downloaded on first run)           |
| `out/`                   | all generated artifacts (gitignored)                          |

## Dependencies

- `whisper-cli` (homebrew `whisper-cpp`)
- `ffmpeg` with `libx264`, `aac`, `showwaves`, `drawbox`, `apad`, `movie`, `overlay` (homebrew default)
- `pdftoppm` (homebrew `poppler`) for PDF → PNG in scout
- `node` (uses `oven/node_modules/puppeteer` to avoid extra installs)
- Google Chrome at `/Applications/Google Chrome.app` (puppeteer driver)
- Network access to `aesthetic.computer/api/say` (jeffrey-pvc TTS)
