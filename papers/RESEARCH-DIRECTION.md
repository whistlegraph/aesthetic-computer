# Research Direction · Aesthetic Computer

**Last updated**: 2026-03-10
**Author**: Jeffrey Alan Scudder — [ORCID 0009-0007-4460-4913](https://orcid.org/0009-0007-4460-4913)

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
- [ ] Cite Ge Wang's ChucK papers more specifically for SLOrk technical details
- [ ] Add section on software-defined spatialization (distributed speakers as emergent hemispherical speaker)
- [ ] Research and cite post-COVID laptop orchestra status (many dissolved?)
- [ ] Consider NIME 2027 submission (laptop orchestras + new instruments = core NIME topic)

### 0b. AC Native OS '26 (arXiv — `arxiv-os/os.tex`)
**Status**: Working draft, ~6 pages, WORKING DRAFT watermark
**Current citations**: ~15 (OLPC, Kittler, McLuhan, Illich, Papert, Ukeles, Raspberry Pi, Sonic Pi)
**Core argument**: Flashing surplus commodity laptops with a bare-metal creative OS offers a post-OLPC, post-Apple model of personal computing — deeper personalization at lower cost with zero infrastructure
**What's needed**:
- [ ] Add figures: boot splash screenshot, architecture diagram, cost comparison chart
- [ ] Cite Eben Moglen / FreedomBox — community-owned infrastructure lineage
- [ ] Cite e-waste statistics (UN Global E-waste Monitor 2024) for the surplus hardware argument
- [ ] Add real performance benchmarks (boot time breakdown, frame timing, memory usage)
- [ ] Reference FedAC kiosk variant as the Fedora-based predecessor
- [ ] Consider citing ChromeOS / CloudReady as the incumbent surplus-laptop OS
- [ ] Add section on security model (no network services, no writable rootfs, EFI-only persistence)
- [ ] Pull hardware compatibility test results from different surplus laptop models

### 1. Aesthetic Computer '26 (arXiv — `arxiv-ac/ac.tex`)
**Status**: Working draft, 5 pages, WORKING DRAFT watermark ✓
**Current citations**: ~10 (mostly technical — Processing, p5.js, Scratch, Glitch)
**What's missing from the platter**:
- [ ] Cite Kittler "There is No Software" — AC as software that foregrounds its own materiality
- [ ] Cite McLuhan "Medium is the Message" — URL-as-instrument, the medium shapes creative output
- [ ] Cite Ukeles "Manifesto for Maintenance Art" — AC's maintenance-as-practice philosophy (ants, upkeep)
- [ ] Cite Roos & McLean "Strudel" (ICLC 2023) — live coding in the browser, shared lineage
- [ ] Cite Staunæs on Stiegler "Concept of Idiotext" — user-generated pieces as idiotexts
- [ ] Add adoption metrics section (user counts, piece counts, session data from MongoDB)
- [ ] Reference the 94-project software history to strengthen "Background" section
- [ ] Consider adding a figure showing the repo evolution timeline (from archaeology paper)

### 2. KidLisp '26 (arXiv — `arxiv-kidlisp/kidlisp.tex`)
**Status**: Working draft, 6 pages, WORKING DRAFT watermark ✓
**Current citations**: ~12 (Lisp history, creative coding, DSLs)
**What's missing from the platter**:
- [ ] Cite *The Little Schemer* — pedagogical Lisp tradition KidLisp builds on
- [ ] Cite van Engelen "Lisp in 99 lines of C" — minimal Lisp implementation lineage
- [ ] Cite Roos & McLean "Strudel" — another browser-based creative language
- [ ] Add 118-function table or appendix (the spec exists in `kidlisp/README.md`)
- [ ] Reference Dropbox `kidlisp-syntax-colors/` design assets
- [ ] Add real-world usage stats from `kidlisp` MongoDB collection
- [ ] Consider citing McCarthy 1960 (original Lisp paper) and Sussman & Steele (Scheme)

### 3. Pieces Not Programs '26 (arXiv — `arxiv-pieces/pieces.tex`)
**Status**: Working draft, 4 pages, WORKING DRAFT watermark ✓
**Current citations**: ~8
**What's missing from the platter**:
- [ ] Cite Langer *Problems of Art* — the "piece" as aesthetic unit, art vs. artifact
- [ ] Cite Ingold "Textility of Making" — making as process, not product
- [ ] Cite Staunæs on Stiegler — pieces as idiotexts
- [ ] Reference the 94-project software history: show how "piece" concept evolved across Jeffrey's tools
- [ ] Add examples of pieces that illustrate the argument (from `disks/` directory)
- [ ] Strengthen philosophical framing with readings from Shklovsky, Arnheim, Klee

### 4. notepat '26 (arXiv — `arxiv-notepat/notepat.tex`)
**Status**: Working draft, 4 pages, WORKING DRAFT watermark ✓
**Current citations**: ~6
**What's missing from the platter**:
- [ ] Cite McLuhan "Medium is the Message" — keyboard as musical interface, repurposing everyday input
- [ ] Reference `notepat.com` Netlify routing and custom domain setup
- [ ] Add usage/adoption data if available
- [ ] Reference predecessor tools: nopaint (6 repos!), whistlegraph performance tools
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
- 40+ readings library PDFs with text extractions in `system/public/assets/papers/readings/text/`
- 0 of 40 currently cited in any paper — this is the biggest gap
- Tier 1 priorities: Kittler, McLuhan, Langer, Little Schemer, Ingold, Ukeles, Strudel, van Engelen, Stiegler
- **Next**: Draft BibTeX entries for all Tier 1, find key quotes, integrate into papers

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
