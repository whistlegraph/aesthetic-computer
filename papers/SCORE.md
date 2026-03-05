# Papers · Aesthetic Computer

## Papers

| Paper | Format | PDF | Source |
|-------|--------|-----|--------|
| Aesthetic Computer '26 | arXiv (LaTeX, 5pp) | `arxiv-ac/ac.pdf` | `arxiv-ac/ac.tex` |
| Aesthetic Computer '26 | JOSS (Markdown, 2pp) | `joss-ac/paper.pdf` | `joss-ac/paper.md` |
| KidLisp '26 | arXiv (LaTeX, 6pp) | `arxiv-kidlisp/kidlisp.pdf` | `arxiv-kidlisp/kidlisp.tex` |
| KidLisp '26 | JOSS (Markdown, 3pp) | `joss-kidlisp/paper.pdf` | `joss-kidlisp/paper.md` |

## Building

```bash
# arXiv papers (xelatex + bibtex, 3 passes)
cd papers/arxiv-ac && xelatex ac.tex && bibtex ac && xelatex ac.tex && xelatex ac.tex
cd papers/arxiv-kidlisp && xelatex kidlisp.tex && bibtex kidlisp && xelatex kidlisp.tex && xelatex kidlisp.tex

# JOSS papers (pandoc)
cd papers/joss-ac && pandoc paper.md --citeproc --pdf-engine=xelatex -o paper.pdf
cd papers/joss-kidlisp && pandoc paper.md --citeproc --pdf-engine=xelatex -o paper.pdf
```

## Formats

- **arXiv**: Full academic papers with two-column layout, AC custom fonts (YWFT Processing), syntax-highlighted code examples, development history tables, adoption metrics. Intended for arXiv preprint submission.
- **JOSS**: Condensed papers for Journal of Open Source Software submission. Markdown with LaTeX header-includes for syntax highlighting. JOSS reviews focus on software quality, documentation, and community impact.

## Subdomain

Published at `papers.aesthetic.computer` and `papers.prompt.ac`.

## Targets

Upcoming conferences and journals to submit to.

### Still Open — Deadlines Ahead

| Venue | Type | Deadline | Funding | Conference Date | Status |
|-------|------|----------|---------|-----------------|--------|
| [ACM C&C 2026](https://cc.acm.org/2026/) | **Demos** | **Apr 16, 2026** | TBD | Jul 13–16, London | **GO** |
| [ICCC 2026](https://computationalcreativity.net/iccc26/) | Short Papers | **Apr 24, 2026** | TBD (email organizers) | Jun 29–Jul 3, Coimbra, Portugal | **GO** |
| [IEEE ICIR 2026](https://icir.ieee.org/) | Late-Breaking / Demos | TBD (check site) | Check w/ organizers | Jun 25–26, Pisa, Italy | **Plan** |
| [Prix Ars Electronica 2026](https://ars.electronica.art/prix/en/) | Interactive Art + | **Mar 9, 2026** 2PM CET | €10,000 prize; free entry | Sep 9–13, Linz, Austria | **GO — 5 days** |
| [S+T+ARTS Prize 2026](https://ars.electronica.art/starts-prize/en/) | Science/Tech/Arts | **Mar 9, 2026** 2PM CET | €40,000 total prize | Sep 9–13, Linz | **GO — 5 days** |

### Deadlines Passed (Track for Next Year)

| Venue | Type | Deadline | Funding | Conference Date | Status |
|-------|------|----------|---------|-----------------|--------|
| [NIME 2026](https://nime2026.org/) | Papers, Music, Workshops | ~~Feb 12~~ / ~~Mar 5~~ passed | Hybrid option (present remotely) | Jun 23–26, London | Missed |
| [xCoAx 2026](https://xcoax.org/) | Papers, Artworks, Performances | ~~Feb 15~~ passed | None announced (€350 reg) | Jul 8–10, Torino | Missed |
| [ACM CHI 2026](https://chi2026.acm.org/) | Posters / Demos | ~~Jan 22~~ passed | [Gary Marsden Travel Awards](https://sigchi.org/resources/gary-marsden-travel-awards/) | Barcelona | Missed |
| [ACM C&C 2026](https://cc.acm.org/2026/) | Papers / Artworks | ~~Feb 5~~ passed | TBD | Jul 13–16, London | Missed |
| [ISEA 2026](https://isea-2026.isea-international.org/) | Papers, Art, Demos | TBD | Hybrid/online for artists | Apr 10–19, Dubai | Research |
| [SIGGRAPH 2026](https://s2026.siggraph.org/) | Art Gallery / Art Papers | Closed | [Underrepresented Communities Grant](https://www.siggraph.org/) | Jul 19–23, Los Angeles | Missed |

### IEEE ICIR 2026 — Intelligent Reality

**What:** IEEE 5th International Conference on Intelligent Reality — convergence of physical/digital worlds, AI, XR, Digital Twins, IoT, Robotics, Wearables.

**Fit for AC:** Aesthetic Computer sits at the intersection of creative computing, extended reality, and AI-augmented authoring. Relevant topic areas:
- Emerging Technologies for Intelligent Reality (AI, ML)
- Extended Reality (XR) — AC's spatial/embodied pieces
- Hardware, Software, and Devices for Intelligent Reality
- Simulation and Digital Twins
- Computer Vision and Graphics

**Submission options:**
1. **Full Paper (4–6 pages)** — AC as a platform for Intelligent Reality authoring: runtime architecture, KidLisp as an AI-friendly creative language, real-time multi-user sessions, embodied input (pen/hand/gamepad)
2. **Short Paper (2–4 pages)** — Focused contribution, e.g. KidLisp + LLM co-authoring for generative art, or the session-server architecture for collaborative XR
3. **Demo / Performance** — Live AC demo (later deadline TBD)

**Key dates:**
- Round 1 submission: ~~Feb 14, 2026~~ (passed)
- Late-Breaking Papers / Demos / Performances: later deadlines — check [icir.ieee.org](https://icir.ieee.org/)
- Camera-ready: Apr 19, 2026 (R1)
- Early bird registration: Apr 15, 2026
- Conference: Jun 25–26, 2026

**Submission portal:** [EDAS](https://edas.info/N34775)

**Next steps:**
- [ ] Check late-breaking / demo / performance deadlines on ICIR site
- [ ] Decide paper angle (full paper vs. short paper vs. demo)
- [ ] Draft outline
- [ ] Adapt existing arXiv AC paper content to IEEE format (4–6 pages)
- [ ] Email organizers re: travel support

### Prix Ars Electronica 2026 — Interactive Art +

**What:** The world's most prestigious media art competition since 1987. Category "Interactive Art +" focuses on interactive works with technological/scientific innovation, new forms of human-tech communication, and sociopolitical relevance.

**Fit for AC:** AC is an interactive art platform. KidLisp as creative language, pen/hand/gamepad embodied input, URL-as-instrument interaction model, real-time multi-user sessions. Perfect fit for Interactive Art +.

**Prize:** Golden Nica + **€10,000**. Free to enter. Submission is online only.

**Deadline:** **March 9, 2026, 2 PM CET** (extended)

**Submit at:** [ars.electronica.art/prix/en/opencall](https://ars.electronica.art/prix/en/opencall/)

**Also:** S+T+ARTS Prize 2026 (science/technology/arts intersection) — deadline ~~Mar 9, 2026~~ — €40,000 total prize. [Entry details](https://ars.electronica.art/starts-prize/en/open-call/entry-details/)

**Festival:** Sep 9–13, 2026, Linz, Austria

### ACM C&C 2026 — Demos Track (OPEN)

**What:** ACM Creativity & Cognition. Theme: "Creativity for Change." London, Jul 13–16. Central Saint Martins, University of the Arts London.

**Fit for AC:** Creative computing platform, AI-augmented authoring, KidLisp as end-user creative programming. Demos track is ideal — show AC live.

**Deadline:** **April 16, 2026** (Demos). Notification: May 7. Camera-ready: May 21.

**Next steps:**
- [ ] Prepare demo submission by Apr 16
- [ ] Check if Gary Marsden Travel Awards apply to C&C (it's a SIGCHI conference)

### ICCC 2026 — Computational Creativity

**What:** 17th International Conference on Computational Creativity. Coimbra, Portugal, Jun 29–Jul 3.

**Fit for AC:** KidLisp + LLM co-authoring is literally computational creativity. The AI-human creative loop, generative art through a Lisp dialect, the AC runtime as a creativity support environment. Strong fit.

**Short Papers deadline:** **April 24, 2026**. Notification: May 11. Camera-ready: May 27.

**Full Papers:** ~~Mar 8~~ passed.

**Next steps:**
- [ ] Draft ICCC short paper: KidLisp as a computational creativity environment
- [ ] Email iccc26@computationalcreativity.net re: travel support

### IEEE ICIR 2026 — Late-Breaking / Demos

See details above. Check for late-breaking deadlines on [icir.ieee.org](https://icir.ieee.org/).

### Venues to Track for 2027

- **NIME** — AC as musical instrument interface. Hybrid option. Track 2027 dates.
- **xCoAx** — "Aesthetics & X" — perfect alignment. Torino was €350 reg.
- **ACM CHI** — Gary Marsden Travel Awards cover everything. Demos track ideal.
- **SIGGRAPH** — Art Gallery / Emerging Tech. Underrepresented Communities Grant.

## Funding Sources (General)

| Source | Type | Amount | Notes |
|--------|------|--------|-------|
| [Gary Marsden Travel Awards](https://sigchi.org/resources/gary-marsden-travel-awards/) | Travel grant | Flights, lodging, meals, registration | For SIGCHI conferences (CHI, C&C, etc.). Early-career / financial need |
| [SIGGRAPH Underrepresented Communities Grant](https://www.siggraph.org/) | Registration + mentorship | Registration waiver | For SIGGRAPH attendance |
| [Creative Capital Award](https://creative-capital.org/) | Project grant | Up to $50,000 | For original artistic works. Tech/multidisciplinary welcome |
| [Foundation for Contemporary Arts](https://www.foundationforcontemporaryarts.org/) | Emergency Grant | Varies | For artists presenting at specific events |
| [Processing Foundation](https://processingfoundation.org/) | Fellowships | Varies | Creative coding community. AC is in this lineage |
| NIME Diversity Grants | Travel/registration | Varies | Check with nime2026.org |
| IEEE Conference Travel Grants | Travel | Partial | Available at many IEEE conferences for students/researchers |
| [Prix Ars Electronica](https://ars.electronica.art/prix/en/) | Prize | €10,000 (Golden Nica) | Interactive Art + category. Free entry. Annual. |
| [S+T+ARTS Prize](https://ars.electronica.art/starts-prize/en/) | Prize | €40,000 total | EU Commission prize for science/technology/arts. Annual. |

## Author

Jeffrey Alan Scudder — ORCID: [0009-0007-4460-4913](https://orcid.org/0009-0007-4460-4913)
