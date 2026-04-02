# Making Getty's Scores Project Accessible on AC

*A report on how to expose all Getty score objects inside Aesthetic Computer*
*2026-04-02*

## Source Material

- Getty digital edition: https://www.getty.edu/publications/scores/
- Getty source repo: https://github.com/thegetty/scores
- Local text extract already on platter: `system/public/assets/papers/readings/text/Gallope-Harren-Hicks-The-Scores-Project-2025.txt`
- Local digest already on platter: `reports/2026-04-02-getty-scores-project.md`

## What "All Scores" Means Here

The Getty repo has:

- **11 score chapters** in `content/01/` through `content/11/`
- **609 object records** in `content/_data/objects.yaml`
- **610 object pages** in `content/object-index/*.md`
- **125 object records tagged `score`** via `type: [ score ]`

So "make all scores accessible on AC" should not mean "copy the entire 600-object archive into our repo as images." It should mean:

1. Every Getty chapter is discoverable from AC.
2. Every object whose metadata says `type: [ score ]` is searchable and openable from AC.
3. Every score object has at least a text-mode representation (title, maker, date, caption, alt text, credit, source link), and ideally a visual route to the original Getty page.

## Hard Constraints

### Rights

Getty's book text is CC BY-NC 4.0, but images are explicitly excluded from that grant. The repo README says many figure images live in a private `scores-images` submodule because third-party licenses restrict them to the Getty publication.

Implication: **do not mirror score images into AC by default.** AC can safely mirror metadata, commentary text, captions, and links with attribution, but score image binaries should stay on Getty unless we have item-by-item rights clearance or a public-domain-only subset.

### Technical embedding and fetch

HTTP checks on 2026-04-02 show:

- `https://www.getty.edu/publications/scores/` returns 200
- `https://www.getty.edu/publications/scores/01/` returns 200
- no `X-Frame-Options` header
- no `Content-Security-Policy: frame-ancestors ...` blocker in the checked responses
- `Access-Control-Allow-Origin: *`

Implication: **both iframe embedding and browser-side `fetch()` are plausible.** We do not have to proxy Getty HTML just to read it, though a local baked manifest is still safer than scraping live pages at runtime.

## Recommended Shape

Build a dedicated AC piece, probably `/scores`, that acts as a reader for the Getty score corpus.

### Mode A: Local text index + remote object viewer

This is the lowest-risk path.

- Build a local JSON manifest from `thegetty/scores` source metadata.
- In AC, render a searchable list of all 11 chapters and all 125 score objects.
- For each score object, show:
  - chapter
  - object id
  - maker
  - title
  - date
  - medium/location
  - `extended_caption`
  - `credit`
  - `alt`
  - links to the canonical Getty chapter and object page
- Add a "view original" button that opens `https://www.getty.edu/publications/scores/object-index/<id>/`.
- Optionally show the Getty object page in an iframe panel inside AC, with a fallback to `jump()` if embedding ever breaks.

This makes every score discoverable on AC without rehosting restricted images.

### Mode B: Text-only accessibility layer

Use the manifest as a proper accessibility surface, not just a link list.

- Add keyboard-first browsing: arrow keys to move through scores, enter to open, `/` to search.
- Add a "read caption" toggle that displays `extended_caption`, `alt`, and `credit` in large text.
- Add `say()` or browser speech synthesis for caption and alt text, so object cards can be listened to.
- Add filters by `maker`, `year_filter`, `chapter`, and `type`.
- Add a "copy citation/source URL" affordance for each object.

This is probably the most AC-native way to make Getty scores "accessible": not by duplicating the archive, but by turning the metadata into a playable index.

### Mode C: Permissioned or public-domain visual mirror

Only after Modes A/B work:

- Identify which score images are public domain or otherwise reusable outside Getty.
- Mirror only that whitelist into an AC-managed bucket.
- Render those images directly in `/scores`, while leaving restricted items as remote links/iframes.

This mode should not block the first implementation. It is a rights-clean upgrade path, not the starting point.

## Suggested Data Pipeline

Avoid scraping generated HTML if possible. The source repo already has structured data.

### 1. Add a build/import script

Create something like `papers/import-getty-scores.mjs` that:

- clones or updates `https://github.com/thegetty/scores` into a temp/cache dir
- reads each `content/NN/index.md`
- extracts chapter title, subtitle, commentary link/blurb, and each `score.objects[].id`
- reads `content/_data/objects.yaml`
- filters object records where `type` contains `score`
- joins score objects back to their chapter metadata when possible
- emits a local manifest such as `system/public/assets/papers/getty-scores-manifest.json`

That manifest should include one object per Getty score record and preserve the canonical Getty URLs rather than inventing our own IDs.

### 2. Add an AC reader piece

Create `system/public/aesthetic.computer/disks/scores.mjs` that:

- loads the local manifest
- renders chapter pills + a scrollable/searchable score list
- opens a selected score in a split view
- shows text metadata locally
- offers "open Getty page" and "embed Getty page" actions
- can route by object id, for example `scores:012` or `scores:kaprow`

This piece should work even if the iframe is disabled: the text manifest is the local fallback.

### 3. Add platter integration

Add a `/papers` platter entry for:

- the generated manifest
- the importer script
- the `/scores` piece source
- this report

This keeps the research and the implementation path connected in the same platter.

## Why This Should Be a Manifest, Not a Full Mirror

The score images are the visually seductive part, but they are also the legally constrained part. The metadata is the durable part we can own locally:

- chapter names and structure
- object IDs and types
- captions and alt text
- maker/date/location metadata
- canonical source links

That is enough to make all score objects searchable, citeable, and navigable from AC right now. The original Getty page can remain the canonical visual rendering until rights are solved item by item.

## A Minimal First Version

If we want the smallest useful implementation:

1. Generate `getty-scores-manifest.json` from the Getty repo.
2. Build `/scores` as a text-first browser over 11 chapters and 125 score objects.
3. Use direct links or an iframe to Getty object pages for visual inspection.
4. Preserve clear attribution and the CC BY-NC / image-rights boundary in the UI.

That gets "all scores accessible somehow on AC" done without pretending we can legally become Getty's image CDN.

## Notes on Exact Chapter Set

The 11 chapter pages currently are:

- 01 Morton Feldman - *Intersection 3* (1953)
- 02 John Cage - *Concert for Piano and Orchestra* (1958)
- 03 Sylvano Bussotti - *Five Piano Pieces for David Tudor* (1959)
- 04 Benjamin Patterson - *Paper Piece* (1960)
- 05 La Monte Young, ed. - *An Anthology of Chance Operations* (1962-63)
- 06 George Brecht - *Drip Music (Drip Event)* (1959-62), from *Water Yam* (1963)
- 07 Jackson Mac Low - *Three Social Projects* (1963)
- 08 Yvonne Rainer - *We Shall Run* (1963)
- 09 Alison Knowles - *The Identical Lunch* (late 1960s-early '70s)
- 10 Mieko Shiomi - *Spatial Poem* (1965-75)
- 11 Allan Kaprow - *Routine* (1973-75)

## Bottom Line

Yes, we can make all Getty score objects accessible from AC. The clean path is **local metadata + local text accessibility + remote canonical visuals**.

Do not start by copying images. Start by building a manifest and an AC reader piece that treats Getty's source metadata as a scorebook index, then lets each object open its canonical Getty page.
