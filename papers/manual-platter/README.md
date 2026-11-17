# Manual — platter

Design references for **manuals**: bestiaries, rulebooks and pocket field
manuals, read for how a page teaches. A sub-platter within the
[papers platter](../SCORE.md), opened 2026-11-16 beside the
[AC '26 Magician's Manual](../arxiv-magicians-manual/).

The request was old Dungeons & Dragons guides, monster manuals and Traveller.
Those books are in copyright and still sold, so they sit on a cite-only shelf.
What gets fetched is what they descend from and what restates them openly.

## Posture

**Index and citation only.** No source text or page image is reproduced in this
repo.

- Public-domain and openly licensed PDFs are fetched into `sources/` by
  [`fetch-sources.mjs`](fetch-sources.mjs) from the Internet Archive and
  Wizards of the Coast. **`sources/` is gitignored.**
- The TSR and GDW originals are cited, never fetched. Unlicensed scans of them
  are easy to find on the Internet Archive; the script does not touch them.
- Full bibliography with fetch URLs, rights lines and verification status:
  [`sources.json`](sources.json). Reading notes: [`digest.md`](digest.md).

## Shelves

| Shelf | What it holds |
|---|---|
| **ancestor** | Public-domain books the game manual descends from: a demon dictionary (1863), a bestiary (1658), Wells's *Little Wars* (1913), War Office war-game rules (1896), an OSS pocket manual (1944). |
| **open** | Modern rulebooks under open licences or given away by their authors: the D&D SRD 5.1 (CC BY 4.0), Basic Fantasy core rules and field guide, OSRIC. |
| **cite-only** | The in-copyright originals: the 1977 *Monster Manual*, the 1979 *Dungeon Masters Guide*, the 1981 *Basic Rulebook*, *Traveller* Books 1 to 3, and the Cepheus Engine SRD (open, but behind a storefront login). |

## Fetch

```
node papers/manual-platter/fetch-sources.mjs
```

Fetches nine PDFs (about 470 MB), converts each to text with `pdftotext`, and
cuts one contact sheet of twelve evenly spaced pages per book into
`sources/sheets/`. The sheets are for looking at layouts side by side; they
stay local with the PDFs.
