# Research Direction · Aesthetic Computer

**Last updated**: 2026-03-28
**Author**: @jeffrey — [ORCID 0009-0007-4460-4913](https://orcid.org/0009-0007-4460-4913)

---

## Current Goals

1. **Get all 6 papers to submission quality** — integrate platter corpus citations, polish arguments, tighten prose
2. **Submit to open deadlines**: ACM C&C Demos (Apr 16), ICCC Short Papers (Apr 24)
3. **Build the research moat** — use the 40+ readings library and 94-project software history as scholarly foundation
4. **Establish AC's place in computing history** — repo archaeology paper traces the lineage; software history catalogs 94 predecessor projects

---

## Per-Paper Revision Plans

### 0a. PLORKing the Planet (arXiv — `arxiv-plork/plork.tex`)
**Status**: Working draft, ~8 pages, WORKING DRAFT watermark
**Current citations**: ~25 (PLOrk, L2Ork, SLOrk, Attali, Turino, Illich, OLPC, e-waste, surplus pricing)
**Core argument**: PLOrk proved laptop orchestras are musically legitimate but the model is trapped in universities at $1,500+/seat. AC Native OS + surplus hardware ($50/seat) removes the economic barrier entirely — 240M Windows 10 EOL machines = raw material for a planetary laptop orchestra.
**What's needed**:
- [ ] Add figures: PLOrk vs AC OS cost scaling chart, surplus hardware photo grid, ensemble network diagram
- [ ] Get exact PLOrk equipment cost figures from Trueman 2006/2007 papers (currently estimated)
- [ ] Add real AC OS ensemble performance test results (latency, voice count, network throughput)
- [x] Cite Ge Wang's ChucK papers more specifically for SLOrk technical details — prose strengthened 2026-03-28
- [ ] Add section on software-defined spatialization (distributed speakers as emergent hemispherical speaker)
- [x] Research and cite post-COVID laptop orchestra status (many dissolved?) — prose added in Related Work 2026-03-28
- [ ] Consider NIME 2027 submission (laptop orchestras + new instruments = core NIME topic)

### 0b. AC Native OS '26 (arXiv — `arxiv-os/os.tex`)
**Status**: Working draft, ~6 pages, WORKING DRAFT watermark
**Current citations**: ~16 (OLPC, Kittler, McLuhan, Illich, Papert, Ukeles, Raspberry Pi, Sonic Pi, Ted Nelson)
**Core argument**: Flashing surplus commodity laptops with a bare-metal creative OS offers a post-OLPC, post-Apple model of personal computing — deeper personalization at lower cost with zero infrastructure
**What's needed**:
- [ ] Add figures: boot splash screenshot, architecture diagram, cost comparison chart
- [x] Cite Eben Moglen / FreedomBox — community-owned infrastructure lineage — BibTeX + prose added 2026-03-28
- [x] Cite e-waste statistics (UN Global E-waste Monitor 2024) for the surplus hardware argument — BibTeX + prose added 2026-03-28
- [ ] Add real performance benchmarks (boot time breakdown, frame timing, memory usage)
- [ ] Reference FedAC kiosk variant as the Fedora-based predecessor
- [x] Consider citing ChromeOS / CloudReady as the incumbent surplus-laptop OS — BibTeX + prose added 2026-03-28
- [ ] Add section on security model (no network services, no writable rootfs, EFI-only persistence)
- [ ] Pull hardware compatibility test results from different surplus laptop models
- [x] Cite Ted Nelson *Computer Lib / Dream Machines* — personal computing philosophy lineage (BibTeX added 2026-03-18)

### 1. Aesthetic Computer '26 (arXiv — `arxiv-ac/ac.tex`)
**Status**: Working draft, 5 pages, WORKING DRAFT watermark ✓
**Current citations**: ~14 (Processing, p5.js, Scratch, Glitch, Kittler, McLuhan, Ted Nelson, 10 PRINT) — all \cite{} calls wired into prose
**What's missing from the platter**:
- [x] Cite Kittler "There is No Software" — BibTeX added 2026-03-18
- [x] Cite McLuhan "Understanding Media" — BibTeX added 2026-03-18
- [x] Cite Ted Nelson *Computer Lib / Dream Machines* — BibTeX added 2026-03-18
- [x] Cite Montfort et al. *10 PRINT* — procedural generation / randomness in computation — BibTeX added 2026-03-18
- [x] Cite Ukeles "Manifesto for Maintenance Art" — AC's maintenance-as-practice philosophy (ants, upkeep) — BibTeX + prose added 2026-03-28
- [x] Cite Roos & McLean "Strudel" (ICLC 2023) — BibTeX + prose added 2026-03-18
- [x] Cite Staunæs on Stiegler "Concept of Idiotext" — BibTeX + prose added 2026-03-18
- [ ] Add adoption metrics section (user counts, piece counts, session data from MongoDB)
- [ ] Reference the 94-project software history to strengthen "Background" section
- [ ] Consider adding a figure showing the repo evolution timeline (from archaeology paper)

### 2. KidLisp '26 (arXiv — `arxiv-kidlisp/kidlisp.tex`)
**Status**: Working draft, 6 pages, WORKING DRAFT watermark ✓
**Current citations**: ~16 (Lisp history, creative coding, DSLs, Little Schemer, McCarthy, van Engelen, Joy of Clojure) — all \cite{} calls wired into prose
**What's missing from the platter**:
- [x] Cite *The Little Schemer* — BibTeX added 2026-03-18
- [x] Cite van Engelen "Lisp in 99 lines of C" — BibTeX added 2026-03-18
- [x] Cite McCarthy 1960 (original Lisp paper) — BibTeX added 2026-03-18
- [x] Cite Fogus & Houser *Joy of Clojure* — functional paradigm context — BibTeX added 2026-03-18
- [x] Cite Roos & McLean "Strudel" — BibTeX + prose added 2026-03-18
- [ ] Add 118-function table or appendix (the spec exists in `kidlisp/README.md`)
- [ ] Reference Dropbox `kidlisp-syntax-colors/` design assets
- [ ] Add real-world usage stats from `kidlisp` MongoDB collection

### 3. Pieces Not Programs '26 (arXiv — `arxiv-pieces/pieces.tex`)
**Status**: Working draft, 4 pages, WORKING DRAFT watermark ✓
**Current citations**: ~19 (+ Ted Nelson, 10 PRINT, Shklovsky, Adorno, Langer) — all \cite{} calls wired into prose
**What's missing from the platter**:
- [x] Cite Langer *Feeling and Form* — the "piece" as aesthetic unit — BibTeX added 2026-03-18
- [x] Cite Shklovsky "Art as Technique" — defamiliarization, making the familiar strange — BibTeX added 2026-03-18
- [x] Cite Adorno "Punctuation Marks" — formal systems criticism — BibTeX added 2026-03-18
- [x] Cite Ted Nelson *Computer Lib / Dream Machines* — BibTeX added 2026-03-18
- [x] Cite Montfort et al. *10 PRINT* — BibTeX added 2026-03-18
- [x] Cite Ingold "Textility of Making" — BibTeX + prose added 2026-03-18
- [x] Cite Staunæs on Stiegler — BibTeX + prose added 2026-03-18
- [ ] Reference the 94-project software history: show how "piece" concept evolved across Jeffrey's tools
- [ ] Add examples of pieces that illustrate the argument (from `disks/` directory)

### 4. notepat '26 (arXiv — `arxiv-notepat/notepat.tex`)
**Status**: Working draft, 4 pages, WORKING DRAFT watermark ✓
**Current citations**: ~6
**What's missing from the platter**:
- [x] Cite McLuhan "Medium is the Message" — keyboard as musical interface, repurposing everyday input — BibTeX + prose added 2026-03-28
- [ ] Reference `notepat.com` Netlify routing and custom domain setup
- [ ] Add usage/adoption data if available
- [x] Reference predecessor tools: nopaint (6 repos!), whistlegraph performance tools — prose added 2026-03-28
- [ ] Consider citing Eglash on culturally situated design tools

### 5. Aesthetic Computer JOSS (`joss-ac/paper.md`)
**Status**: Working draft, 2 pages, WORKING DRAFT watermark ✓
**JOSS focus**: Software quality, documentation, community impact, contribution guidelines
**Revision plan**:
- [ ] Update contribution guidelines reference (does CONTRIBUTING.md exist?)
- [ ] Add community impact metrics
- [ ] Verify all API documentation links are current
- [ ] Cross-reference with arXiv paper to avoid duplication

### 6. KidLisp JOSS (`joss-kidlisp/paper.md`)
**Status**: Working draft, 3 pages, WORKING DRAFT watermark ✓
**JOSS focus**: Software quality, test suite, API documentation
**Revision plan**:
- [ ] Reference the test suite (`spec/` directory, `npm run test:kidlisp`)
- [ ] Add test coverage metrics
- [ ] Update API documentation references
- [ ] Link to `kidlisp/README.md` as canonical language reference

---

## Upcoming Deadlines

| Deadline | Venue | Submission Type | Paper(s) to Submit |
|----------|-------|----------------|-------------------|
| **Apr 16, 2026** | ACM C&C 2026 (London) | Demos | AC demo + poster |
| **Apr 24, 2026** | ICCC 2026 (Coimbra) | Short Papers | KidLisp as computational creativity |
| TBD | IEEE ICIR 2026 (Pisa) | Late-Breaking / Demos | AC or KidLisp |

---

## Open Research Threads

### Repository Archaeology
- Working draft published at `papers.aesthetic.computer/ac-repo-archaeology`
- Traces AC through 4 successive repos (system-ac → disks-ac → 2022.aesthetic.computer → aesthetic-computer)
- Documents 94 predecessor projects with GitHub source links
- **Next**: Could become a formal paper for a software engineering venue

### Software History Lineage
- 94 projects cataloged in `system/public/assets/papers/readings/text/jas-software-history.txt`
- Platter at `papers.aesthetic.computer/platter` — searchable research hub
- Three lineage threads: drawing tools → AC pieces, performance tools → multiplayer, dev infrastructure → hot-reload

### Citation Integration (The Moat)
- 58 readings in `system/public/assets/papers/readings/text/`
- **32 of 58 now cited** across paper bibliographies (up from 0 as of 2026-03-10)
- **Batch added 2026-03-18**: Little Schemer, van Engelen, Joy of Clojure, McCarthy 1960, Ted Nelson, 10 PRINT, Kittler, McLuhan, Shklovsky, Adorno, Langer, Fisher across 8 bib files
- **Prose citations added 2026-03-18**: `\cite{}` calls integrated into 6 paper .tex files (ac, kidlisp, pieces, sustainability, os, complex)
- **Final Tier 1 batch 2026-03-18**: Ingold, Strudel (Roos & McLean), Staunæs/Stiegler added to bib files AND cited in prose
- **All 6 papers build clean** (xelatex + bibtex, 3 passes, no undefined citations)
- **Tier 1 complete.** All originally identified Tier 1 priorities now have BibTeX entries and prose citations.
- Remaining uncited readings worth considering (Tier 2): Arnheim, Klee (already in `arxiv-whistlegraph`), Manovich
- **Batch 2026-03-28**: Ukeles (AC paper), McLuhan (notepat), Moglen/FreedomBox + e-waste stats + ChromeOS Flex (OS paper), post-COVID status + ChucK/SLOrk detail (PLORKing), Jeffrey economic specifics (Who Pays), predecessor tools (notepat)
- **Next**: Write actual paper prose around citations (currently integrated as single sentences; some could become fuller paragraphs)

### Ars Electronica 2026
- Submission at `papers/ars-electronica-2026/`
- `demoplay.md`, `description.md`, architecture report
- Festival: Sep 9–13, Linz, Austria

### Conference Demo Preparation
- For ACM C&C and ICCC: need a live demo setup
- Consider: notepat live performance, KidLisp collaborative drawing, AC piece gallery walkthrough
- Hardware: phone + laptop + projector (URL-addressable = easy demo)

---

## Platter Corpus Summary

The research platter (`papers.aesthetic.computer/platter`) contains:
- **6 papers** (4 arXiv LaTeX + 2 JOSS Markdown)
- **40+ readings** (PDFs with text extractions)
- **85 reports** (internal markdown)
- **144 plans** (internal markdown)
- **8 studies** (internal markdown)
- **70+ reference items**
- **94-project software history**
- **Repository archaeology** (4-repo evolution)
- **API & data source documentation**

This corpus is the scholarly foundation. Every paper revision should draw from it.
