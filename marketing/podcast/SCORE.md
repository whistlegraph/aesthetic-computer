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
3. **`cover.mjs`** — square cover art per episode via xelatex (same fonts as the
   essays): the pink drop-shadow YWFT title + AC color bar. Embedded into the mp3
   as ID3 album art and kept as `out/<slug>-cover.png`.
4. **`produce.mjs`** — the orchestrator. Narrates intro + each paragraph + outro
   via `/api/say`, measures the real body duration with ffprobe to fill in the
   announced length, assembles jingle + VO + paragraph breaths with ffmpeg
   (loudnorm → mp3), embeds the cover, and writes a metadata sidecar
   `out/<slug>.json`. Output: `out/<slug>.mp3`.
5. **`feed.mjs`** — aggregates the sidecars into `out/index.json` (catalog) +
   `out/feed.xml` (RSS 2.0 + iTunes), and renders the series cover `out/cover.png`.
6. **`publish.mjs`** — stages the public set into `publish/` and syncs it to the
   CDN. **Dry by default** (prints the command); `--push` actually uploads.

## Feed / hosting

Episodes + feed live on DO Spaces (`assets-aesthetic-computer`), served at
**`https://assets.aesthetic.computer/podcast/`** — same bucket as `/pop`. The
subscribable feed is `https://assets.aesthetic.computer/podcast/feed.xml`.
Publishing is a deliberate per-run choice (`publish.mjs --push`), never automatic —
a reading only goes public when you say so.

## Usage

```bash
cd marketing/podcast
node bin/produce.mjs ../../papers/essay-named-markets/named-markets.tex --open
node bin/feed.mjs                 # build index.json + feed.xml + series cover
node bin/publish.mjs              # dry run: stage publish/ + print the sync cmd
node bin/publish.mjs --push       # actually upload → feed goes live
```

Flags: `--open` (slab-afplay the result), `--force` (bypass say cache),
`--stability 0.55 --similarity 0.8 --speed 0.98` (voice tuning),
`--base <url>` on `feed.mjs` (override the asset host).

## Reused from /pop

- `/api/say` invocation pattern + content-hash caching — lifted from `pop/bin/say.mjs`.
- Jingle synthesis follows the pop DSP posture (phase-increment sines, exp-decay
  bells) but is self-contained here.
