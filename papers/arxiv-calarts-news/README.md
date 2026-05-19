# What's New CalArts!? — a dossier

**Subtitle:** why the president got booed off the stage — a letter, with sources
**Register:** dossier, working draft, **local-only** (not deployed/pushed)

A news-bulletin dossier. It opens with a letter to two CalArts MFAs (Fía and
Patrick) who sent a video — the 2026 CalArts president booed off the
commencement stage — and then ventures, with sourced history and real
art-education / institution theory, to figure out *why*. No computer
metaphor; no self-citations. The argument: the crit is the school's actual
curriculum, the boo is that crit turned on the administration, and the
dossier traces it through Womanhouse (1972), the 2014 Title IX walkout, and
the 2017–2026 budget/labor crisis. Theory pulled from the platter
(Singerman, de Duve, Elkins, hooks, Freire, Spivak, Bourdieu, Readings,
Bousquet, Harney & Moten, Fraser, Alberro, Davis, Ross, Fisher).

## Files

| File | Role |
|---|---|
| `calarts-news.tex` | Canonical arXiv twocolumn source (for the platter / a future arXiv build). |
| `calarts-news-cards.tex` | **Hand-authored** cards build; title illustration is the OpenAI dossier cover. The deliverable. |
| `calarts-news-cards.pdf` | Built locally only (4×6" card, viewable on the MacBook Neo). |
| `references.bib` | Sourced references; theory refs carried from the prior paper. |
| `data/sources.md` | Per-claim citation audit trail (+ flagged gaps). |
| `figures/cover-prompt.txt` | gpt-image-2 subject prompt (house STYLE_PREFIX auto-prepended by `gen-cover.mjs`). |
| `figures/cover-sources.md` | Per-element audit trail for the cover (dossier cover doctrine). |
| `figures/cover.png` | OpenAI colored-pencil cover (Ladd & Kelsey megastructure + hairline fracture). |
| `figures/pals.pdf` | AC pals mark (copied from `arxiv-ac/figures`). |

## Why the cards .tex is hand-authored

`cards-convert.mjs` regenerates `*-cards.tex` from the arXiv source and emits a
**fixed** title card (pals logo only) — it would not include the OpenAI cover
on the title card. So this paper is intentionally **not** in `cards-convert.mjs`'s
`PAPER_MAP`. It *is* registered in `cli.mjs` + `papermill.mjs` `PAPER_MAP` and
`SCORE.md`, and `cli.mjs` auto-detects the existing `*-cards.tex` for builds.
If you ever regenerate via cards-convert, you will lose the title art — edit
`calarts-news-cards.tex` directly instead.

## Build (local only)

```bash
cd papers/arxiv-calarts-news
xelatex -interaction=nonstopmode calarts-news-cards.tex
bibtex calarts-news-cards
xelatex -interaction=nonstopmode calarts-news-cards.tex
xelatex -interaction=nonstopmode calarts-news-cards.tex
```

Do **not** deploy, push, or run `sync-platter.mjs` for this — it is a
working draft addressed to two people by first name.

## Gaps

- Exact 2026 commencement date unconfirmed from a text source (stated "May 2026").
- "CalArts Mafia" animation diaspora stated qualitatively, not enumerated.
- 2014 case facts attributed to outlets (Al Jazeera / Hyperallergic / Artforum);
  no independent records pulled.
