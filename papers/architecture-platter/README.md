# Architecture — platter

A reading platter for **foundational architectural treatises**: the proportion,
order, and modular-composition systems that architecture wrote down before
software existed. A sub-platter within the [papers platter](../SCORE.md),
parallel to [rhythm-platter](../rhythm-platter/) and
[corporate-graphics-platter](../corporate-graphics-platter/).

Two openers, chosen as a matched pair:

- **Palladio, *The Four Books of Architecture*** (Venice 1570; Isaac Ware's
  London 1738 English folio, Getty Research Institute scan on the Internet
  Archive). The canonical proportional system: the five orders, villa plans,
  civic works, Roman temples — 212 plates of ratios made visible.
- **Durand, *Précis of the Lectures on Architecture*** (École polytechnique
  1802–1805; Getty Research Institute Texts & Documents translation, 2000,
  intro. Antoine Picon, trans. David Britt). The counter-move: Durand throws
  out Vitruvian proportion for a modular grid, combinatorial assembly of
  standard elements, and economy as the governing criterion — architecture
  restated as a generative procedure. The pair brackets the shift from
  *proportion as authority* to *composition as algorithm*.

## Posture

**Index and citation only.** No source text is reproduced in this repo:

- PDFs are fetched into `sources/` by
  [`fetch-sources.mjs`](fetch-sources.mjs) from their public hosts — the
  Internet Archive for Palladio, the Getty Virtual Library for Durand.
  **`sources/` is gitignored** — the fetch is reproducible, the mirror is not
  committed.
- The Getty Durand translation is in copyright, distributed free by Getty.
  Cite it; never redistribute it.
- Full bibliography with fetch URLs and verification status:
  [`sources.json`](sources.json).

## Shelves

| Shelf | What it holds |
|---|---|
| **treatise** | The primary treatises themselves, in canonical scanned or publisher editions. |

More shelves (commentary, computational readings, pattern languages) can be
added as the platter grows.

## Fetch

```
node papers/architecture-platter/fetch-sources.mjs
```

Fetches both PDFs (~44 MB total) and converts each to text with `pdftotext`
(both carry text layers — the Internet Archive scan includes OCR).
