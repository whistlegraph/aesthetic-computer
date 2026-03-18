# Thomas Lawson Structured Catalog — Plan

## Current State

The scraper (v2) uses the **WP REST API** (`/wp-json/wp/v2/pages/{id}`) to fetch
rendered Elementor page content rather than scraping raw HTML. It handles two
Elementor caption patterns:

- **Pattern A** (1977-1979): `<h5><i>Title</i>, YYYY<br>medium<br>dimensions</h5>`
- **Pattern B** (all other pages): `<h5>TITLE</h5>` + `<h6>YEAR</h6>` as separate widgets

**Results so far:**
- 213 artworks, 13 exhibitions — all pass schema validation
- 77% have year, but only 7% have medium/dimensions (only the 1977-1979 page embeds that data in the HTML)
- 0% have description or collection info

**Full WP page inventory (51 pages):**
- 11 In the Studio pages (all scraped)
- 8 Beyond the Studio pages (all scraped)
- 13 Art in a Broader Context / exhibition pages (all scraped)
- 7 Bookshelf pages (writings — NOT yet scraped)
- 1 Notes page, 1 About, 1 Contact, 1 Home, etc.

## Schema Alignment with Schema.org VisualArtwork

Current schema maps well to Schema.org but should add a few fields for completeness:

| Our field       | Schema.org equivalent       | Status    |
|-----------------|-----------------------------|-----------|
| `title`         | `name`                      | done      |
| `year`          | `dateCreated`               | done      |
| `medium`        | `artMedium`                 | done      |
| `dimensions`    | `width` / `height` / `depth`| done      |
| `type`          | `artform`                   | done      |
| `images`        | `image`                     | done      |
| (missing)       | `artworkSurface`            | **add**   |
| (missing)       | `artEdition`                | **add**   |
| (missing)       | `provenance`                | **add**   |
| `collection`    | (custom)                    | done      |
| `description`   | `description`               | done      |

## Plan — 4 Steps

### Step 1: Enhance the JSON schema

Add fields to `artwork.schema.json`:
- `artworkSurface` — canvas, paper, linen, board, etc. (nullable string)
- `artEdition` — edition info for prints/multiples (nullable string)
- `provenance` — array of `{ owner, acquired, notes }` objects
- `writings` — array of IDs linking to a new `writing.schema.json`
- Split `medium` into `artMedium` (materials) + keep `medium` as full freeform string

Add a new `writing.schema.json` for Bookshelf entries:
- `id`, `title`, `slug`, `year`, `publication`, `url`, `type` (essay/review/interview/anthology), `sourceUrl`

Update `catalog.schema.json` to include `writings` array.

### Step 2: Scrape Bookshelf pages

7 Bookshelf pages to scrape for writing/publication records:
- `/bookshelf/` (index)
- `/bookshelf_afterall/` (Afterall journal)
- `/bookshelf_artforum/` (Artforum)
- `/bookshelf_eastofborneo/` (East of Borneo)
- `/bookshelf_writingsabouttl/` (writings about TL by others)
- `/bookshelf-anthologies/` (anthologies)
- `/bookshelf-reallife/` (REALLIFE Magazine)
- `/elementor-1796/` (interviews)
- `/elementor-395/` (Considering Other Artists)

These are text-heavy pages with titles, publication names, and years. Parse them
into structured `writing` records.

### Step 3: Enrich artwork data via WP media API + filenames

The WP media library (`/wp-json/wp/v2/media`) contains metadata:
- EXIF data (photographer credit, copyright)
- Captions on some images
- **Filenames encode dimensions**: e.g. `Thomas-Lawson-Flying-brick-2019-18-x-24-in-45.72-x-60.96-cm`

Strategy:
1. Fetch all media items (paginated, ~200+ items)
2. Parse filenames for embedded dimensions (regex: `(\d+)-x-(\d+)-in`)
3. Match media items to artworks by image URL
4. Fill in dimensions from filename where not already set from Elementor captions

This could raise medium/dimensions coverage from 7% to ~30-40%.

### Step 4: Manual enrichment layer

Create a `data/overrides.json` file for manual corrections and additions that
can't be scraped:
- Medium/materials for works where the site doesn't list them
- Collection/owner info
- Exhibition history links
- Description text
- Series assignments beyond auto-detection

The scraper merges `overrides.json` on top of scraped data, so manual edits
persist across re-scrapes.

## Files to create/modify

| File | Action |
|------|--------|
| `schema/artwork.schema.json` | Update — add `artworkSurface`, `artEdition`, `provenance` |
| `schema/writing.schema.json` | **New** — structured writing/publication record |
| `schema/catalog.schema.json` | Update — add `writings` array |
| `scrape.mjs` | Update — add bookshelf scraping, media API enrichment, overrides merge |
| `validate.mjs` | Update — validate writings, show enrichment stats |
| `data/overrides.json` | **New** — manual corrections layer (starts empty) |
