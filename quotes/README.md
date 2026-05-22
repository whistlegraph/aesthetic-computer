# Quotes

Vendor quotes issued under **Aesthetic Inc.** Source of truth for what was billed, to whom, and for what scope. Each quote lives in its own directory named `<quote-id>-<slug>/`; the source and built PDF are both named after the quote ID (`<quote-id>.tex` / `<quote-id>.pdf`).

## Quote ID

Format: `AE-<YYYY>-<NNN>` — `AE` for Aesthetic (Inc.), four-digit year, three-digit sequence resetting each year. Set once per quote via `\newcommand{\quoteid}{...}` at the top of the `.tex` source (and used as the filename itself).

## Ledger

| Quote ID | Date | Recipient | Scope | Amount | Status |
|----------|------|-----------|-------|--------|--------|
| [AE-2026-001](AE-2026-001-ucla-social-software/AE-2026-001.pdf) | 2026-04-22 | UCLA Design Media Arts — Social Software (Jinwoo Park; program co-directed by Casey Reas and Lauren Lee McCarthy) | Technical documentation of Social Software runtime/language/tools, analysis of software in production across UCLA DMA, survey of open-source and proprietary stacks; hosted at [sosoft.arts.ucla.edu](https://sosoft.arts.ucla.edu/) | $1,200.00 | Issued |
| [AE-2026-002](AE-2026-002-resilience-institute/AE-2026-002.pdf) | 2026-05-21 | The Resilience Institute — Resilient Minds (Rebecca Bonneville, NP), c/o Fía Benitez | Four-area web app built as code — homepage + four pillars (psychiatry practice, mentoring & arts, classes, shop), Wix migration, two revision rounds, brand & design (logo + style guide); optional six-month support contract priced separately | $8,900.00 | Draft |

## Building

Each quote builds independently with tectonic:

```bash
cd quotes/AE-2026-001-ucla-social-software
tectonic -X compile AE-2026-001.tex
```

Relative paths in the source:
- Fonts: `../../system/public/type/webfonts/` (YWFT Processing)
- Logo: `../../papers/arxiv-ac/figures/pals.pdf`

Some quotes ship a `build.fish` instead — multi-page layouts with
`remember-picture` overlays need two xelatex passes, and a quote may carry
a grayscale variant. `AE-2026-002` is built this way:

```bash
cd quotes/AE-2026-002-resilience-institute
./build.fish        # → AE-2026-002.pdf (color) + AE-2026-002-grayscale.pdf
```

The grayscale variant is the same source compiled with `\def\GRAYSCALE{}`,
which desaturates the palette and images and drops decorative fills.

## Starting a new quote

1. Pick the next sequence number (look at the ledger, increment).
2. `cp -R AE-YYYY-NNN-<prev-slug>/ AE-YYYY-NNN+1-<new-slug>/` (or start from a blank fork of an existing one).
3. Rename the `.tex` inside to match the new quote ID.
4. Update `\newcommand{\quoteid}{...}` and the `\permalink` / `\permalinkdisplay` macros to reference the new filename.
5. Update recipient, subject, scope, amount.
6. Build, commit, add a row to the ledger above.
