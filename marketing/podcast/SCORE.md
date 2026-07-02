# marketing/podcast — essays → jeffrey-voiced readings

A production lane that turns written essays (`papers/essay-*/*.tex`, `opinion/*.md`)
into spoken-word podcast episodes in @jeffrey's ElevenLabs voice, framed like a
liturgical reading.

## The shape of an episode

```
[intro jingle]  →  "A reading of the essay: <Title>, by @jeffrey.
                    Approximately <N> minutes."  →  [the essay, read
                    paragraph by paragraph with breath between]  →
                    "Here ends the reading."  →  [outro jingle]
```

The framing is deliberately churchy — a *reading of the essay*, an announced
length, a fixed closing ("here ends the reading") — so the catalog feels like a
lectionary of AC writing rather than a talk show.

## Voice

ElevenLabs **jeffrey-pvc** via the production `/api/say` proxy — the same voice as
the pop lane and the 24h recap (`provider="jeffrey"`, `voice="neutral:0"`).
Stability held ≥ 0.5 to keep identity intact. Synthesis costs real money but is
content-hash cached (`out/cache/`), so re-runs are free.

## Pipeline (all in `bin/`)

1. **`essay-to-script.mjs`** — strips LaTeX/Markdown to clean spoken prose.
   Drops footnotes, section headings, colophon, URLs; keeps the argument. Emits
   `{ title, author, date, paragraphs[], wordCount }`.
2. **`jingle.mjs`** — synthesizes `intro.wav` / `outro.wav`: a short pentatonic
   bell motif (ascending in, resolving out). Deterministic, $0, no samples.
3. **`produce.mjs`** — the orchestrator. Narrates intro + each paragraph + outro
   via `/api/say`, measures the real body duration with ffprobe to fill in the
   announced length, then assembles jingle + VO + paragraph breaths with ffmpeg
   (loudnorm → mp3, ID3 tagged). Output: `out/<slug>.mp3`.

## Usage

```bash
cd marketing/podcast
node bin/produce.mjs ../../papers/essay-named-markets/named-markets.tex
node bin/produce.mjs ../../opinion/lotus-notes.md --open
```

Flags: `--open` (slab-afplay the result), `--force` (bypass say cache),
`--stability 0.55 --similarity 0.8 --speed 0.98` (voice tuning).

## Reused from /pop

- `/api/say` invocation pattern + content-hash caching — lifted from `pop/bin/say.mjs`.
- Jingle synthesis follows the pop DSP posture (phase-increment sines, exp-decay
  bells) but is self-contained here.
