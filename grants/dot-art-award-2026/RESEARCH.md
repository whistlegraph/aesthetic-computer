# .ART Award 2026 — Research

Sourced: `grants/prospects-research-2026-06-01.md` (flagged as highest
open-eligibility cash on the board). Promoted 2026-07-19.

## The call

- **URL:** https://www.award.art/
- **Deadline:** 2026-11-01 · winners announced 2026-12-03
- **Fee:** none · no geographic restrictions · 18+ · collectives OK ·
  previously exhibited work OK · cross-disciplinary welcome
- **Prizes (pool $50k+):**
  - Grand Prize: **$15,000 cash** (one artist)
  - One-month residencies: Chateau du Fresne (France) + Anfitrion.art (Spain)
  - Editorial coverage in international art media
  - A premium .ART domain (valued $10,000)

## Jury (as listed 2026-07)

Jerry Saltz (Pulitzer-winning critic), **Regina Harsanyi (Museum of the
Moving Image — variable media / software preservation)**, Dean Phelus
(American Alliance of Museums), Akanksha Ballaney (Artsy/Artnet), plus
six additional curators/art professionals. Harsanyi is the vote our
entry speaks to most directly: a still-running, URL-addressable
software artwork with documented lineage and preservation practice.

## Submission format

The entry **is a .art domain URL**, pointing at one of:

1. a dedicated website on the .art domain;
2. a .art redirecting to a site on another extension;
3. a .art redirecting to social.

Plus three content components:

- artist biography (≥100 words)
- high-quality artwork documentation with technical specs
- process narrative: text ≥100 words OR video 30–60s

**Multiple entries allowed — each on its own .art domain.**

## Our entry: No Paint, 2016–present, at nopaint.art

Spine per `plans/NOPAINT_CONSTRUCT_MIGRATION.md`: *No Paint,
2016–present* — a long collaboration between a person, a proposing
machine, and a community of brush and stamp makers. The Construct
recovery (source `.c3p` recovered + hash-verified 2026-07-17) is
evidence of lineage; the native AC remake is the current artwork.

Domain state (verified 2026-07-19): `nopaint.art` is live (Vercel,
76.76.21.21) serving the 2021 Construct build ("No Paint: Summer
2021"). Planned routing from the migration plan, step 5:

- `nopaint.art` — the new native work + short explanation
- `nopaint.art/classic` — preserved 2021 Construct build
- `nopaint.art/story` — 2016 → 2021 → Aesthetic Computer → present
- `aesthetic.computer/nopaint` — the same live native piece inside AC

Refresh progress as of 2026-07-19:

- ✅ `.c3p` source recovered from Dropbox, verified, fully inventoried
  (45 event sheets, 82 layouts, 205 object types, 1,197 images,
  128 sounds; brush/filter vocabulary + weighted picker extracted)
- ✅ Migration step 1 landed (commit `e75489f08`): native proposal loop
  in `disks/nopaint.mjs` with explicit states + `lib/nopaint-proposals.mjs`
  weighted picker + test suite
- ⬜ Classic build export via Construct r449 LTS (step 0.4–0.5)
- ⬜ Authored brush ports (step 2), social-artwork manifest (step 3),
  playlist/prompt language (step 4), nopaint.art routing (step 5)

## Second entry (optional)

Rules permit multiple entries. **cancelok** (nopaint-style iterative
software; `nopad.art` was free as of 2026-07-14) could be a second
entry *if* it matures by Nov 1. Decision can wait until October —
No Paint carries the flag.

## Open questions

- [ ] Open the actual entry form; capture exact fields + caps.
- [ ] Confirm which categories/art-form tags exist on the form.
- [ ] Confirm whether the entry judges the domain *as it stands at
      deadline* (assume yes — plan the nopaint.art refresh milestones
      backward from Nov 1).
- [ ] Whose name on the entry: @jeffrey solo, or with the 2021
      collaborator credits as on the existing site (Alex Freundlich,
      Ella Fleck, Camille Klein & Friends)? Collectives are allowed.
