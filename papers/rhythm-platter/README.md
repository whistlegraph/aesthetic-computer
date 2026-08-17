# Rhythm — platter

A reading platter for the **geometry of musical rhythm**: Toussaint's corpus and
the wider field around it (maximal evenness, rhythmic oddity, syncopation
measures, distance geometry, tiling canons, entrainment, spoken solkattu,
timbre space). A sub-platter within
the [papers platter](../SCORE.md), parallel to
[jeffrey-platter](../jeffrey-platter/), [whistlegraph-platter](../whistlegraph-platter/),
and [corporate-graphics-platter](../corporate-graphics-platter/).

> The platter exists so that AC's rhythm tooling resolves "a Euclidean rhythm",
> "how even is this", or "how far is son from rumba" to a **cited definition and
> a verified test vector** — instead of to whichever formula seemed right the
> day an engine was written.

It has a specific consumer. [`pop/`](../../pop/) already runs this mathematics in
three places — `pop/minitek/c/hypnotek.c` (Bjorklund, precession, evenness,
vertex distance), `dubtek.c` (rhythmic oddity, chord-sum evenness, off-beatness),
and `acidtek.c` (off-beatness) — with each engine hand-rolling its own copy and
none of it in `pop/lib/`. This platter is the digest step that comes **before**
lifting that math into a shared library, and before the
[`pop/bracelet/`](../../pop/bracelet/) spatial-necklace lane is built on top of it.

## Posture

**Index and original restatement only.** No source text is reproduced in this
repo. Specifically:

- *The Geometry of Musical Rhythm* (CRC Press) is **cited by chapter and never
  mirrored** — no scans, no excerpts, no quoted passages, no PDF.
- Open-access papers are fetched into `sources/` by
  [`fetch-sources.mjs`](fetch-sources.mjs). **`sources/` is gitignored** — the
  fetch is reproducible, the mirror is not committed.
- [`timelines.json`](timelines.json) carries **structured facts** (k, n, onset
  sets, inter-onset intervals, necklace representatives, culture tags) with a
  locator back into the source. Attribution paragraphs stay in the papers.
- The `digest/` entries are written in AC's own words and exist to specify code,
  not to summarise reading.
- The one **video source** (`levin-2011-indian-rhythms`) follows the same rule:
  the video and captions are fetched into gitignored `sources/` by hand
  (yt-dlp), its on-screen overlays are OCR'd locally (macOS Vision) to
  `sources/*.ocr.txt`, and only the digest restatement is committed.
- `wessel-1979-timbre-space` is a **hand-placed** scan (shared by Sage Jenson,
  2026-08-15) — no open-access URL currently resolves, so `fetch-sources.mjs`
  skips it and the local copy stays out of git like everything else in
  `sources/`.

This mirrors the corporate-graphics-platter's "third-party — research reference
only" rule.

## Shelves

| Shelf | What it settles | Digest |
|---|---|---|
| **toussaint** | The spine: the Euclidean catalogue, the geometric programme | [01](digest/01-representation.md), [02](digest/02-evenness.md) |
| **evenness** | What "even" formally means — and that balance ≠ evenness | [02](digest/02-evenness.md) |
| **oddity-depth** | Rhythmic oddity, deepness, interval content, homometry | [03](digest/03-oddity-depth-interval.md) |
| **syncopation** | Syncopation and complexity measures | [04](digest/04-syncopation.md) |
| **distance** | Rhythmic similarity — the morphing levers | [05](digest/05-distance-similarity.md) |
| **canons** | Complementation and tiling rhythmic canons | [06](digest/06-complements-canons.md) |
| **perception** | Meter, entrainment, groove — where the grid model stops | [07](digest/07-perception-groove.md) |
| **spatial** | Spatial hearing: the limits the `bracelet` thesis rests on | [08](digest/08-spatial-appendix.md) |
| **solkattu** | Spoken rhythm: the konnakol syllable system as a representation | [09](digest/09-solkattu.md) |
| **timbre** | Timbre space: the platter's dissimilarity-to-geometry move, off-rhythm | [10](digest/10-timbre-space.md), [10a](digest/10a-timbre-space-in-ac.md) |

Full bibliography with fetch URLs and verification status:
[`sources.json`](sources.json) (33 entries; 9 open-access papers + 1 open
video).

## The catalogue

[`timelines.json`](timelines.json) — **48 Euclidean rhythms** extracted from
Toussaint (2005) §4 with culture tags, plus **9 named timelines** (the six
distinguished 5-onset/16-pulse claves, the two 8-pulse Cuban patterns, and the
12-pulse bell).

Every extracted box string is cross-checked against a local Bjorklund
implementation at build time. **All 48 agree.** That check is the acceptance
test for the future `pop/lib` implementation, and it already passes on the
reference version — so the algorithm is settled before the library is written.

## What the digest changed

Four findings that would have gone into code wrong:

1. **Son clave is not a Euclidean rhythm.** Its inter-onset intervals are
   (3,3,4,2,4); a maximally even five-in-sixteen is (3,3,3,3,4). Toussaint
   states plainly that E(5,16) "is also the Bossa-Nova rhythm necklace of
   Brazil." Of the six distinguished claves, **only bossa is Euclidean**.
   `pop/minitek/thesis/dubtek.md` currently says E(5,16) is "the canonical 2-3
   son rotation" — that is wrong and needs correcting.
2. **Compare necklaces, not strings.** The published catalogue prints one
   arbitrary rotation per entry, and many traditional rhythms are named as a
   *rotation* of the Euclidean form. Any equality test must compare
   lexicographically-least rotations, or it will report false negatives on
   half the catalogue.
3. **Off-beatness is a group-theoretic property, not a heuristic.** The off-beat
   positions of an n-cycle are exactly the **generators of C(n)** — the p with
   gcd(p, n) = 1. For n = 12 that is {1, 5, 7, 11}. The measure degenerates for
   prime n (every position is a generator) and the implementation must say so
   rather than return a meaningless number.
4. **Chronotonic distance wins.** Toussaint (2004) compares five dissimilarity
   measures and concludes the best overall is the **chronotonic distance,
   followed by swap distance in close second**. So `pop/lib` should default to
   chronotonic and offer swap — not default to Hamming because it is easiest.

## Regenerate

```bash
node papers/rhythm-platter/fetch-sources.mjs      # → sources/ (gitignored)
node papers/rhythm-platter/build-timelines.mjs    # → timelines.json, with cross-check

# the one video source — fetched by hand, then frame-OCR'd (macOS Vision):
yt-dlp -f "bv*[height<=720]+ba/b" --write-auto-subs --sub-langs en \
  -o "papers/rhythm-platter/sources/levin-2011-indian-rhythms.%(ext)s" \
  "https://www.youtube.com/watch?v=KsvKQhOeQjQ"
```

`build-timelines.mjs` exits non-zero if any extracted rhythm disagrees with
Bjorklund.

## Verification debts

Open, and deliberately visible rather than smoothed over:

- **19 of 33 sources carry `"checked": false`** — written from working knowledge,
  citation details not yet confirmed against a publisher record. None may be
  cited in a paper until checked.
- The 9 `named_timelines` rows carry `"checked": false`. The onset sets are the
  standard published forms and the Euclidean-membership results are computed,
  but the onset strings have not been re-read out of Toussaint (2002).
- `bjorklund-2003-sns` has no confirmed retrieval URL.
- The digest is written against the open-access papers plus working knowledge of
  the book. **Chapter-level citations to the 2013 book are approximate** and
  flagged inline where they carry weight.

---

*maintained by @jeffrey · feeds [`pop/lib`](../../pop/lib/) and [`pop/bracelet/`](../../pop/bracelet/)*
