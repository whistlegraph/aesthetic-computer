# Papers · Aesthetic Computer

## Papers

Sorted by most recently edited/added.

| Paper | Format | PDF | Source |
|-------|--------|-----|--------|
| From setup() to boot(): The Piece API as Successor to the Processing Sketch | arXiv (LaTeX, 7pp) | `arxiv-api/api.pdf` | `arxiv-api/api.tex` |
| Network Audit: Who Uses Aesthetic Computer and What Do They Make? | arXiv (LaTeX, 4pp) | `arxiv-network-audit/network-audit.pdf` | `arxiv-network-audit/network-audit.tex` |
| KidLisp Language Reference: 118 Built-ins in 12 Categories | arXiv (LaTeX, 4pp) | `arxiv-kidlisp-reference/kidlisp-reference.pdf` | `arxiv-kidlisp-reference/kidlisp-reference.tex` |
| Whistlegraph: Drawing, Singing, and the Graphic Score as Viral Form | arXiv (LaTeX, 4pp) | `arxiv-whistlegraph/whistlegraph.pdf` | `arxiv-whistlegraph/whistlegraph.tex` |
| Dead Ends: Failed Experiments and Abandoned Approaches | arXiv (LaTeX, 4pp) | `arxiv-dead-ends/dead-ends.pdf` | `arxiv-dead-ends/dead-ends.tex` |
| Who Pays for Creative Tools? | arXiv (LaTeX, 5pp) | `arxiv-sustainability/sustainability.pdf` | `arxiv-sustainability/sustainability.tex` |
| Radical Computer Art: Goodiepalian Approaches | arXiv (LaTeX, 4pp) | `arxiv-goodiepal/goodiepal.pdf` | `arxiv-goodiepal/goodiepal.tex` |
| Repository Archaeology: Tracing AC Through Its Git History | arXiv (LaTeX, 3pp) | `arxiv-archaeology/archaeology.pdf` | `arxiv-archaeology/archaeology.tex` |
| Diversity and Inclusion in AC Paper Citations | arXiv (LaTeX, 4pp) | `arxiv-diversity/diversity.pdf` | `arxiv-diversity/diversity.tex` |
| KidLisp: A Minimal Lisp for Generative Art | arXiv (LaTeX, 6pp) | `arxiv-kidlisp/kidlisp.pdf` | `arxiv-kidlisp/kidlisp.tex` |
| notepat.com: From Keyboard Toy to System Front Door | arXiv (LaTeX, 5pp) | `arxiv-notepat/notepat.pdf` | `arxiv-notepat/notepat.tex` |
| AC Native OS '26 | arXiv (LaTeX, 5pp) | `arxiv-os/os.pdf` | `arxiv-os/os.tex` |
| Aesthetic Computer '26 | arXiv (LaTeX, 5pp) | `arxiv-ac/ac.pdf` | `arxiv-ac/ac.tex` |
| Pieces Not Programs: The Piece as a Unit of Creative Cognition | arXiv (LaTeX, 4pp) | `arxiv-pieces/pieces.pdf` | `arxiv-pieces/pieces.tex` |
| Aesthetic Computer '26 | JOSS (Markdown) | `joss-ac/paper.pdf` | `joss-ac/paper.md` |
| KidLisp '26 | JOSS (Markdown) | `joss-kidlisp/paper.pdf` | `joss-kidlisp/paper.md` |

## Building

```bash
# Prerequisites: xelatex + bibtex must be installed in the dev container.
# The Dockerfile includes texlive-xetex and related packages for this.

# arXiv papers (xelatex + bibtex, 3 passes)
cd papers/arxiv-ac && xelatex ac.tex && bibtex ac && xelatex ac.tex && xelatex ac.tex
cd papers/arxiv-kidlisp && xelatex kidlisp.tex && bibtex kidlisp && xelatex kidlisp.tex && xelatex kidlisp.tex
cd papers/arxiv-os && xelatex os.tex && bibtex os && xelatex os.tex && xelatex os.tex
cd papers/arxiv-api && xelatex api.tex && bibtex api && xelatex api.tex && xelatex api.tex

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

### Dated Checklist (as of Mar 11, 2026)

- [ ] **Mar 11, 2026**: Confirm ICIR late-breaking/demo dates on `icir.ieee.org` and choose route (full, short, or demo).
- [ ] **Mar 12, 2026**: Finalize ACM C&C demo concept and required assets list (video, screenshots, abstract, setup notes).
- [x] **Mar 13, 2026**: Draft AC Native OS '26 paper (`arxiv-os/os.tex`) — bare-metal creative OS, surplus hardware thesis, device-level personalization.
- [ ] **Mar 13, 2026**: Draft Tier 1 BibTeX entries (Kittler, McLuhan, Langer, Little Schemer, Ingold, Ukeles, Strudel, van Engelen, Stiegler).
- [ ] **Mar 14, 2026**: Pull adoption metrics (users, pieces, sessions, KidLisp usage) and define one shared stats block for all papers.
- [ ] **Mar 17, 2026**: Revise `arxiv-ac/ac.tex` with new citations + adoption metrics + software-history framing.
- [ ] **Mar 19, 2026**: Revise `arxiv-kidlisp/kidlisp.tex` with citation additions + function-table/appendix plan + usage stats.
- [ ] **Mar 21, 2026**: Revise `arxiv-pieces/pieces.tex` and `arxiv-notepat/notepat.tex` with philosophical citations + lineage examples.
- [ ] **Mar 24, 2026**: Revise `joss-ac/paper.md` and `joss-kidlisp/paper.md` for docs/test coverage/API-link accuracy.
- [ ] **Apr 1, 2026**: Complete ACM C&C demo draft package and run an internal review pass.
- [ ] **Apr 8, 2026**: Freeze ACM C&C demo materials and submit travel-support query if applicable.
- [ ] **Apr 16, 2026**: Submit ACM C&C demos track.
- [ ] **Apr 17, 2026**: Start ICCC short-paper draft from KidLisp paper + repo evidence.
- [ ] **Apr 22, 2026**: Final ICCC polish and send travel-support email to `iccc26@computationalcreativity.net`.
- [ ] **Apr 24, 2026**: Submit ICCC short paper.

### Additional Opportunities Found (checked Mar 11, 2026)

| Venue | Type | Deadline | Conference Date | Status |
|-------|------|----------|-----------------|--------|
| [ACM UIST 2026](https://uist.acm.org/2026/cfp/) | Papers | Abstract: **Mar 24, 2026**; Paper: **Mar 31, 2026** | Oct 11–14, 2026 (Busan) | **New / Urgent** |
| [IEEE VIS 2026 (Full Papers)](https://ieeevis.org/year/2026/info/call-participation/call-for-participation/) | Papers | Abstract: **Mar 21, 2026**; Paper: **Mar 31, 2026** | Oct 2026 | **New / Urgent** |
| [IEEE VIS 2026 (Short Papers)](https://ieeevis.org/year/2026/info/call-participation/shortpapers/) | Short Papers | **Apr 30, 2026** | Oct 2026 | **New / Strong Fit** |
| [ACM Multimedia 2026 (Technical)](https://2026.acmmm.org/site/cfp-guidelines.html) | Full Papers | Abstract: **Mar 25, 2026**; Paper: **Apr 1, 2026** | Nov 10–14, 2026 (Rio de Janeiro) | **New / Strong Fit** |
| [ACM Multimedia 2026 (Brave New Ideas)](https://2026.acmmm.org/site/call-bni.html) | Vision/BNI Papers | **Apr 1, 2026** | Nov 10–14, 2026 (Rio de Janeiro) | **New / Strong Fit** |
| [ACM Multimedia 2026 (Interactive Art)](https://2026.acmmm.org/site/call-interactive-art.html) | 2-page Interactive Art Paper | **Apr 23, 2026** | Nov 10–14, 2026 (Rio de Janeiro) | **New / Strong Fit** |
| [JOSS](https://joss.theoj.org/) | Software Paper (journal) | Rolling (no fixed deadline) | Rolling | **Anytime** |
| [NordiCHI 2026](https://sites.uwasa.fi/nordichi2026/nordichi-2026/submit/) | HCI Papers/Critiques/Arts/Industry | Abstract + metadata: **Apr 16, 2026**; Submission: **Apr 23, 2026** | Oct 3–7, 2026 (Vaasa) | **New / HCI** |
| [HCI International 2026 (Late Breaking Work)](https://2026.hci.international/submissions.html) | LBW Paper Proposal (800 words) | Proposal: **Mar 30, 2026**; Final: **Jun 12, 2026** | Jul 26–31, 2026 (Montreal) | **New / HCI** |
| [European Lisp Symposium 2026](https://www.european-lisp-symposium.org/2026/index.html) | Lisp Papers / Experience Reports | **Mar 22, 2026** (extended) | May 11–12, 2026 (Kraków + online) | **New / Lisp / Urgent** |
| [FUNARCH 2026 @ ICFP](https://icfp26.sigplan.org/home/funarch-2026) | Functional/Lisp-adjacent Workshop Papers | Submission: **Jun 1, 2026** (estimate on CFP) | Aug 2026 (with ICFP 2026) | **New / Lisp-adjacent** |

### Funding + Cost Tracker (checked Mar 11, 2026)

Only opportunities with filter result `Yes` or `Partial`, sorted by nearest deadline.

| Opportunity | Filter Result | Nearest Deadline | Free to Submit? | Funding / Paid Upside | Cost Notes |
|-------|------|----------|---------|--------|--------|
| [IEEE VIS 2026 Papers](https://ieeevis.org/year/2026/info/call-participation/call-for-participation/) | **Partial** | **Mar 21, 2026** (abstract) / Mar 31 paper | **Likely yes** (no submission fee stated in CFP) | [Doctoral Colloquium funding](https://ieeevis.org/year/2026/info/call-participation/doctoral-colloquium/) may cover registration + partial travel (student-limited) | No APC requirement stated in CFP |
| [ACM UIST 2026 Papers](https://uist.acm.org/2026/cfp/) | **Partial** | **Mar 24, 2026** (abstract) / Mar 31 paper | **Likely yes** (no submission fee stated in CFP) | [GMTA](https://sigchi.org/resources/gary-marsden-travel-awards/) for SIGCHI events | UIST CFP notes ACM 2026 APC model ($250 ACM/SIG member, $350 non-member if APC applies) |
| [Creative Capital Open Call 2027](https://creative-capital.org/creative-capital-award/award-application/) *(non-paper grant path)* | **Yes** | **Apr 3, 2026** (3 PM ET) | **Yes** (handbook states free and open-call application) | Paid upside: grants up to $50,000 + $10,000 state prize | Grant program, not publication/APC model |
| [ACM C&C 2026 Demos](https://cc.acm.org/2026/demos/) | **Partial** | **Apr 16, 2026** | **Likely yes** (no submission fee stated in CFP) | [GMTA](https://sigchi.org/resources/gary-marsden-travel-awards/) can cover registration/travel for eligible SIGCHI applicants | Conference registration required if accepted; ACM Open model may apply to ACM publication types |
| [NordiCHI 2026 Research Papers](https://sites.uwasa.fi/nordichi2026/nordichi-2026/submit/submission-guidelines/) | **Partial** | **Apr 16, 2026** (abstract) / Apr 23 submission | **Likely yes** (no submission fee stated in CFP) | [GMTA](https://sigchi.org/resources/gary-marsden-travel-awards/) may apply (SIGCHI-linked conference) | Research papers / critiques are APC-eligible under ACM Open |

### GMTA Checklist (Independent Researcher)

- [ ] Confirm target conference is SIGCHI-sponsored (or SIGCHI-linked) and submission status is acceptance-ready.
- [ ] Draft a 120-200 word funding-need statement: independent researcher, limited institutional support, why travel support changes outcome.
- [ ] Draft a 120-200 word impact statement: what your demo/paper contributes to HCI/creative computing and who benefits.
- [ ] Build a lean budget (registration, airfare, lodging, local transit, visa if needed) with lowest credible quotes/screenshots.
- [ ] Prepare acceptance evidence (acceptance email or official decision page) and submission metadata.
- [ ] Gather links that establish legitimacy: ORCID, GitHub repo, papers index, demo/video page.
- [ ] Request advance reimbursement if cash-flow is a blocker (GMTA explicitly allows advances by request).
- [ ] Submit before GMTA cycle cutoff and set a follow-up reminder 7 days before travel booking deadlines.
- [ ] Reuse the same narrative + budget packet for UIST, C&C, and NordiCHI variants to reduce prep time.

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
