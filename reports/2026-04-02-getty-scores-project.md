# Getty's *The Scores Project* for AC's Research Platter

*Digest written through `papers/SCORE.md`*
*2026-04-02*

## Sources

- Project site: https://www.getty.edu/publications/scores/
- Publication repository: https://github.com/thegetty/scores
- Open-access PDF: https://www.getty.edu/publications/scores/_assets/downloads/the-scores-project.pdf

## What This Is

*The Scores Project: Essays on Experimental Notation in Music, Art, Poetry, and Dance, 1950-1975* is a Getty Research Institute digital publication edited by Michael Gallope, Natilee Harren, and John Hicks, first published May 6, 2025.

Its subject is the postwar score as an intermedia technology: a page, object, or instruction set that can move between music, dance, poetry, and visual art. The book centers works by George Brecht, Sylvano Bussotti, John Cage, Morton Feldman, Allan Kaprow, Alison Knowles, Jackson Mac Low, Benjamin Patterson, Yvonne Rainer, Mieko Shiomi, David Tudor, and La Monte Young, and it includes a complete edition of *An Anthology of Chance Operations*.

The online edition is the real machine: more than 2,000 archival objects, zoomable images, audio/video, and scholarly commentaries bound together in Quire. The print/PDF book carries the introduction and commentaries, but the website is where the scores behave as an archive you can traverse.

## Why It Belongs on Our Platter

This is directly useful to AC because it treats a score as a living interface instead of a dead document. That is the same wager behind `SCORE.md`, Whistlegraph, KidLisp cards, and the `/papers` platter itself.

The project also gives us a concrete publishing model: a research archive first, then a book carved from that archive, then a repository that preserves the book as editable source.

## Score Mechanics Worth Stealing

### 1. Score as Executable Social Form

The Getty project frames experimental notation as a thing that can trigger performance, interpretation, and cross-disciplinary collaboration. That maps cleanly onto AC's piece model: a Whistlegraph score, a KidLisp card, or a tiny `.mjs` file is not only representation but a prompt for action.

### 2. Platter Before Paper

The publication behaves like our own platter pipeline:

```
archival objects + score facsimiles + media + commentary
  -> thematic score pages
    -> print/PDF book chapters
      -> a source repo with revision history
```

That is the same pressure system described in `papers/SCORE.md`: gather raw material first, let a thread emerge, then compress it into a paper or card.

### 3. Object Pages as Research Cards

Each score entry pairs a primary object with context and commentary. That is close to AC's card format, where one sheet should carry enough source, argument, and invitation to travel. If we expand `/papers` beyond PDFs, this project is a strong precedent for "cards as scholarly object pages."

### 4. Repository as Publication Memory

Getty keeps the publication source in GitHub, with `main` representing the published edition and a `revisions` branch for prospective changes. That makes the book behave more like software without pretending every typo fix should ship instantly. AC papers could use the same idea when one paper needs a stable public edition but ongoing source-level correction.

### 5. Rights Boundaries Made Explicit

The Getty repo README and PDF both state a sharp split: text is CC BY-NC 4.0, but many images are excluded and live in a private `scores-images` submodule. That boundary matters for our platter. We can mirror/link the book text and code, but we should treat image assets as referenced context, not something to ingest wholesale unless rights are clear.

## Concrete AC Uses

- **Whistlegraph '26**: cite this project as a serious art-historical frame for score-as-performance and score-as-viral form.
- **Reading the Score**: compare Getty's archival score scholarship with AC's own `SCORE.md` as an operational score for agents.
- **Radical Computer Art**: use Fluxus/intermedia score histories here as one lineage for Goodiepalian instruction aesthetics.
- **Cards pipeline**: borrow the "object + commentary" pairing when designing richer research cards or platter pages.
- **Repo workflow**: consider a stable publication branch plus a corrections branch for `/papers` source once revisions become frequent.

## What Was Added to the Platter

- Local text extract: `system/public/assets/papers/readings/text/Gallope-Harren-Hicks-The-Scores-Project-2025.txt`
- Repo link: https://github.com/thegetty/scores

The text extract was generated from Getty's open-access PDF with `pdftotext -layout`. It keeps the book's textual layer available for search and reading while preserving the license warning that images are not covered by the CC BY-NC grant.
