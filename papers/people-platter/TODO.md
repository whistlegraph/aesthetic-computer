# People Platter — TODO

A sibling platter to [jeffrey-platter](../jeffrey-platter/) and [whistlegraph-platter](../whistlegraph-platter/). Tracks photographic / textual references for **other people** so jeffrey-image generation can produce group scenes (mentors, contributors, pals, public figures alongside jeffrey) instead of always-solo frames.

**Primary scope:** public figures jeffrey is publicly connected to (mentors, collaborators, peers cited in the dossier papers). Secondary: AC contributors with public presence. Tertiary: private IRL pals (camera roll only, not for public publishing — vault-only, like jas.life).

This file is the scoping doc. No manifest or sync script yet — promote to a real platter once the first ~10 entries land.

---

## Phase 0 — scope & rules

- [ ] Decide a subject is platterizable only if there is at least one public reference photo we can legally point at (Wikipedia, Wikimedia, official site, instagram public account, conference photo). Camera-roll-only subjects go in the private/vault tier and never get committed.
- [ ] Decide naming: `entries/<slug>.md` per person, mirroring the per-paper directory style. Slug = lowercase, dash-separated.
- [ ] Decide the per-person frontmatter schema: `name`, `tier` (public-figure | contributor | pal | private), `relationship` (mentor / cofounder / collaborator / peer / friend), `consent` (public | ask-first | private), `sources` (urls + local paths), `face_match_status`, `last_synced`.
- [ ] Decide whether to reuse the jeffrey face-match → describe → CDN-thumb pipeline (`portraits/jeffrey/bin/`) for everyone, or only for the public-figure tier. Disk + RAM budget on the 8GB machine matters here — no parallel chromium runs.

## Phase 1 — seed list (public figures jeffrey is publicly connected to)

Fill these in as a first pass. Each entry: name, why-they're-here, where to find canonical photos.

- [ ] **Casey Reas** — AIR host, ucla-arts thread. Sources: ucla.edu faculty page, reas.com, wikimedia.
- [ ] **Lauren Lee McCarthy** — sosoft cofounder. Sources: lauren-mccarthy.com, ucla.edu faculty page.
- [ ] **@mollysoda** — Selfie palette collaborator (`disks/selfie.mjs`). Sources: public IG, press photos.
- [ ] **Goodiepal** — referenced in `arxiv-goodiepal/`. Sources: existing dossier figures.
- [ ] **Holden** — referenced in `arxiv-holden/`. Sources: existing dossier figures.
- [ ] sweep `papers/arxiv-*/` dossiers for "People" sections — pull every named person already cited as a public figure, dedupe, score by jeffrey-proximity. Likely candidates: Pioneer Works, Mellon, Internet Archive, Recurse, Eyebeam, SFPC, Rhizome.
- [ ] sweep `papers/cv/cv.tex` for named collaborators / advisors.

## Phase 2 — AC contributors with public presence

- [ ] grep AC repo for handles in `disks/*.mjs` headers ("designed with @x", "with @y").
- [ ] cross-reference handles against `system/netlify/functions/handles.mjs` to confirm they're real AC users.
- [ ] for each, capture: AC handle, real name (if public), public photo URL, what they contributed.

## Phase 3 — private pals (camera roll, vault-only)

Strictly out of the public AC repo. Storage path: vault, alongside jas.life material. Never commit images or names from this tier into `papers/`.

- [ ] decide vault subdirectory layout: `vault/people/<slug>/`.
- [ ] iOS camera roll: write a one-shot extractor that uses `osxphotos` or AppleScript Image Capture to dump tagged faces + a short description per face, **never** uploading anywhere.
- [ ] Fia entry first, as the canonical private-tier example (warmth-default voice, see memory).

## Phase 4 — group imagery generator

- [ ] extend `portraits/jeffrey/bin/generate-neo.py` (or sibling) to accept a cast list: `[jeffrey, casey-reas, mccarthy]` and stitch a multi-subject prompt with face-locked likenesses for each subject.
- [ ] respect: 8 GB RAM cap (≤3 parallel image gens), real-outfits-only rule, no costumes, citrus-green Neos only when the scene calls for hardware.
- [ ] add a "consent gate" — public-figure tier can render in any context; ask-first tier requires a per-render approval log; private tier never renders into anything that lands in `papers/` or `system/public/`.

---

## Open questions (for jas, before phase 1)

1. Is the public-figure tier the only one we publish (e.g. into the papers PDFs / press kits), or do AC contributors with consent also publish?
2. Does the platter live at `papers/people-platter/` or elsewhere? (Defaulting here for now to mirror jeffrey-platter.)
3. Do we want a hosted dashboard like `papers.aesthetic.computer/platter/people/`, or keep this index-only until the first dossier needs it?
4. For private-tier entries: do we even keep an index in the public repo (just `slug` + `tier: private`), or does the existence of those subjects also live only in the vault?
