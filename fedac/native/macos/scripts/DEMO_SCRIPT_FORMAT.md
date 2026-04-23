# AC Native Demo Script Format

**Goal:** a single file that describes a scripted demo run — key
injection timeline, TTS narration, burn-in subtitles — so the macOS
host (and eventually Linux) can **play the demo AND record it** in one
pass, with no external stitching / ffmpeg post-processing.

Invoked like:

```sh
ac-native ../pieces/prompt.mjs --demo demos/intro.md
```

Two format proposals below. Markdown is the recommended one — human-
readable, diffable, comments survive, timestamps obvious.

---

## A) Markdown (preferred)

A single `.md` file. YAML-ish front-matter for run config, then one
event per line keyed on a `[M:SS.mmm]` timestamp. Lines that start
with `#` after the front-matter are comments.

```markdown
# title: AC Native — intro walkthrough
# voice: Samantha
# rate: 160
# handle: jeffrey
# city: Los Angeles
# hour: 13
# window: 1280x800
# subtitles: true          # burn into framebuffer
# narration: true          # run `say` into the audio mix

# ─── Opening ─────────────────────────────────────────────
[0:00.5] say   hi @jeffrey
[0:00.5] caption Welcome to AC Native

# ─── Type 'notepat' slowly; say each letter ──────────────
[0:02.0] say n
[0:02.0] key n
[0:03.0] say o
[0:03.0] key o
[0:04.0] say t
[0:04.0] key t
[0:05.0] say e
[0:05.0] key e
[0:06.0] say p
[0:06.0] key p
[0:07.0] say a
[0:07.0] key a
[0:08.0] say t
[0:08.0] key t
[0:09.5] key enter

# ─── In notepat: prompt user to press C; then press it ───
[0:12.5] say     Press the C key to play a C note
[0:12.5] caption Press 'C' to play a C note
[0:15.0] key     c

# ─── Back to the prompt via triple-escape ───────────────
[0:15.8] say Now let's go back to the prompt
[0:16.0] key escape
[0:16.2] key escape
[0:17.7] key escape

# ─── Type 'off' and let the shutdown animation fire ────
[0:18.2] say o
[0:18.2] key o
[0:19.2] say f
[0:19.2] key f
[0:20.2] say f
[0:20.2] key f
[0:20.5] key enter
# prompt.mjs calls system.poweroff() → bye @jeffrey animation plays
```

### Event grammar

```
event := '[' TIMESTAMP ']' SPACE KIND SPACE ARG
TIMESTAMP := <mm>:<ss>[.<ms>]
KIND := 'key' | 'say' | 'caption' | 'wait' | 'env'
ARG  := rest-of-line (trimmed)
```

- **`key`** — inject a key event at the timestamp (exactly what
  `AC_INJECT_SEQUENCE` does today, but with absolute offsets not
  cumulative deltas). Key names: single character, `enter`,
  `escape`, `pageup`, `pagedown`, `arrow{left,right,up,down}`, etc.
- **`say`** — pipe text through `say -v <voice> -r <rate>`. Mixed
  into the audio track via `aresample + async=1` so timing is
  sample-accurate.
- **`caption`** — render text as burn-in subtitle at bottom of the
  framebuffer. Persists until the next `caption` event or an
  explicit empty `caption` clears it. Styled via libass-equivalent
  font rules baked into the binary.
- **`wait`** — pure timeline anchor. Useful for grouping; doesn't
  fire anything but reserves a timestamp the parser can check.
- **`env`** — set an env var for the run (equivalent to prefixing
  the `ac-native` invocation with `VAR=value`). Handy for late-
  decision things like `handle` if you want to override front-matter.

---

## B) JSON (alternative, for programmatic generation)

```json
{
  "title": "AC Native — intro walkthrough",
  "voice": "Samantha",
  "rate": 160,
  "handle": "jeffrey",
  "city": "Los Angeles",
  "hour": 13,
  "window": { "w": 1280, "h": 800 },
  "subtitles": true,
  "narration": true,
  "events": [
    { "t": 0.5,  "kind": "say",     "text": "hi @jeffrey" },
    { "t": 0.5,  "kind": "caption", "text": "Welcome to AC Native" },
    { "t": 2.0,  "kind": "say",     "text": "n" },
    { "t": 2.0,  "kind": "key",     "key":  "n" },
    { "t": 3.0,  "kind": "say",     "text": "o" },
    { "t": 3.0,  "kind": "key",     "key":  "o" },
    ...
  ]
}
```

Same semantics, just machine-friendlier. Easier to generate from
`waltz-seq.py` style tools; harder to diff in a PR review. For
tooling (ffmpeg, analytics), both formats can share a compiled JSON
intermediate that the binary reads.

---

## Implementation notes (what needs to change in the mac host)

1. **New CLI flag** — `--demo <path>` in `main.c` alongside the
   piece path arg. When present:
   - Parse front-matter into env-var equivalents (`AC_SHOT_HANDLE`,
     etc.) so downstream code is unchanged.
   - Build an event list. Dispatch events from the main loop on
     each frame based on `SDL_GetTicks() - start_tick`.

2. **Subtitles** — render via `font_draw_matrix()` into a reserved
   bottom strip of the framebuffer. State lives in main.c (current
   caption + expiry time). Framebuffer is already the same surface
   that gets PNG-dumped + displayed + encoded to video, so no extra
   plumbing.

3. **TTS** — shell out to `say -v <voice> -r <rate> -o <tmpwav>
   "<text>"` at event time, then mix the wav into the audio tap.
   For recording, we can mix into `synth.wav` directly or emit a
   separate `narration.wav` that `demo.sh`-style post-encode joins
   at the end.

4. **Headless gating** — already supported via `AC_HEADLESS_MS`;
   demo scripts can set an explicit `duration:` in front-matter or
   infer from the latest event.

5. **Compatibility** — `AC_INJECT_SEQUENCE` stays for short one-
   off invocations; `--demo` is the higher-level equivalent that
   adds narration + captions.

---

## Pipeline consolidation

Once the binary supports `--demo`, the existing
`tools/vo-pipeline.mjs` (post-process ffmpeg pipeline for stitching
TTS + subtitles onto an already-rendered video) becomes optional —
only needed when you want to narrate something that wasn't scripted
to begin with (e.g. a hardware capture).

This makes the feedback loop:

```
edit demo.md → ac-native --demo demo.md → one .mkv, done
```

No external ffmpeg, no concat filter, no `?v=N` cachebuster on the
asset host. The recorded video IS the final video.
