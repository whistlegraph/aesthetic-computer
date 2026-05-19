# Cover-prompt sources — What's New CalArts!?

The cover is a **single brutalist megastructure violently shearing apart** —
one chunk breaking free along a jagged fault — rendered with torn-xerox punk
energy on pure white. Per dossier doctrine (papers/SCORE.md), every visual
element traces to a real reference, and the institution is **never named** in
the image; identity is carried by form, silhouette, and era.

## Visual elements → sources

| Element | Source |
|---|---|
| **Long low monolithic concrete megastructure** | The CalArts main building (the "A-building"), Valencia, CA — one brutalist megastructure by **Ladd & Kelsey**, opened **1971**. The literal, recognizable-across-a-room icon of CalArts: one building, all disciplines under one roof — "kernel = proximity." Source: this dossier's `data/sources.md`; prior paper `arxiv-calarts/calarts.tex`. |
| **The violent shear / a chunk breaking free** | The dossier's argument made visceral (not a hairline). "The University in Ruins, 2017–2026" — the $15M structural deficit, the spring-2025 voluntary separations, the July-2025 layoffs, and the booed 2026 commencement. The institution splits along the fault its administration cut into it. Source: `data/sources.md` (financial-crisis + 2026-commencement sections). |
| **Torn / over-photocopied punk-flyer treatment** | The dossier's register. It opens with a boo and argues the boo *is* the crit — confrontation as the institution's oldest process (1972, 2014, 2026). Punk-flyer mark-making is the visual rhyme for the callout: raw, defaced, anti-institutional, made to be stapled to a pole. |
| **Pure white ground, dissolving far ends** | House vignette style, `--white` variant of `gen-cover.mjs` (PAPER = pure white per the requested background). Single floating form, faded periphery, no environmental context. |
| **Darkest-palette-only (near-black, oxblood, brick red, slate)** | Restricting the house palette to its heaviest end carries the punk weight without leaving the AC dossier-cover series. |

## What was deliberately excluded

- **Microphone / podium / "BOO" text** — the boo is the dossier's opening,
  but cover doctrine is ONE singular subject; the fracture carries the crisis.
- **Disney / mouse iconography** — would name a different institution.
- **People, crowds, the graduating class** — no figures; the slab is the subject.
- **Scrub / hills / sky / any readable name** — no context, no signage.

## Sources

- This dossier: `data/sources.md`, `references.bib`
- Prior paper: `papers/arxiv-calarts/calarts.tex` (Ladd & Kelsey megastructure, Disney 1961, Valencia 1971)
- Singerman, *Art Subjects* (1999) — CalArts founding/architecture context
- Generated via `papers/bin/gen-cover.mjs arxiv-calarts-news --force --white`
  (gpt-image-2; colored-pencil/gouache-on-white STYLE_PREFIX auto-prepended)
