# The object registry

`registry.json` separates what the slideshow conflates: **works** (physical
objects — title, year, medium, dimensions, status, exhibitions) from
**photos** (documents of a work at a place and time, mirrored from
`manifest-updated.json`).

The 2026-09-07 generation seeded 50 works from 58 photos by visual
clustering. Where several photos show one painting, the work carries a
`cluster` note with a confidence; two photos are marked `context` (studio
interiors, not work documents); IMG_8067 holds two works side by side and
appears under both.

## Filling it in

Edit `works[]` directly in `registry.json` — or scroll `registry.html`,
which renders every work with its photos and shows which fields are still
blank. Per work:

- `title`, `year`, `medium`, `dimensions` (H × W in)
- `status`: `studio` · `exhibited` · `sold` · `collection` · `auction` · `gifted`
- `exhibitions`: `{venue, city, dates}` entries
- correct any wrong cluster by moving filenames between `photos` arrays

## Why

Ten finished, individually documented physical works is the Pollock-Krasner
portfolio requirement (`grants/pollock-krasner-2026/CAREER-AUDIT.md`), and a
work with a record is what a gallery, juror, or collector can be handed.
The slideshow keeps reading `manifest-updated.json` until the registry is
filled enough to wire in — captions and alt text will come from here.
