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
   Filtered by the shared publish allowlist `lib/hosted.mjs`.
6. **`ship.mjs`** — the one-command publish: guardrail (allowlist) → Buzzsprout
   → CDN (mp3 + cover under the hosted name, which backs the papers podcast
   link) → feed regen → verify. `--papers` also runs papers deploy+index;
   `--private` stages on Buzzsprout. Stops before git (prints the finish block).
7. **`publish.mjs`** *(legacy)* — stages `publish/` + syncs the self-hosted CDN
   feed. Superseded by `ship.mjs` + Buzzsprout; kept for the retired RSS path.

**Allowlist:** `lib/hosted.mjs` maps each cleared `slug → siteName` and is the
single source of truth for what may publish. A slug absent from it never goes
public — `ship.mjs` refuses it and `feed.mjs` drops it.

## Feed / hosting

**Canonical podcast URL: `https://pod.prompt.ac`** — Buzzsprout custom domain
(CNAME `pod.prompt.ac → app.buzzsprout.com`, DNS-only, in the prompt.ac
Cloudflare zone). `https://podcast.aesthetic.computer` 301-redirects there
(proxied CNAME + redirect rule in the aesthetic.computer zone). The
subscribable RSS is Buzzsprout's: `https://feeds.buzzsprout.com/2628235.rss`.

Legacy self-hosted feed (pre-Buzzsprout): DO Spaces
(`assets-aesthetic-computer`) at `https://assets.aesthetic.computer/podcast/`,
feed at `/podcast/feed.xml`, pushed with `publish.mjs --push`. **Retired as a
subscription target** — it was never submitted to directories and shouldn't
be; Buzzsprout is the distribution path.

## Usage

```bash
cd marketing/podcast
node bin/produce.mjs ../../papers/essay-<slug>/<base>.tex --open   # → out/<slug>.mp3
node bin/ship.mjs <slug>          # publish: Buzzsprout + CDN + feed + verify
node bin/ship.mjs <slug> --papers # also deploy the papers PDF + index
# then commit the episode's files + `fish lith/deploy.fish` (ship prints the block)
```

Legacy self-hosted feed: `node bin/feed.mjs && node bin/publish.mjs --push`.

Flags: `--open` (slab-afplay the result), `--force` (bypass say cache),
`--stability 0.55 --similarity 0.8 --speed 0.98` (voice tuning),
`--base <url>` on `feed.mjs` (override the asset host).

## Reused from /pop

- `/api/say` invocation pattern + content-hash caching — lifted from `pop/bin/say.mjs`.
- Jingle synthesis follows the pop DSP posture (phase-increment sines, exp-decay
  bells) but is self-contained here.
