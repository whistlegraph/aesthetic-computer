# ORCID Integration for Aesthetic Computer

**ORCID iD:** [0009-0007-4460-4913](https://orcid.org/0009-0007-4460-4913)
**Date:** 2026-03-03

## What is ORCID?

ORCID (Open Researcher and Contributor ID) is a persistent digital identifier that distinguishes you from every other creator/researcher. It connects your identity to your works across platforms, publications, datasets, and software.

Your ORCID iD: `https://orcid.org/0009-0007-4460-4913`

## What You Can Do with ORCID + This Monorepo

### 1. Add a `CITATION.cff` File (Immediate, High Value)

A `CITATION.cff` in the repo root makes Aesthetic Computer formally citable. GitHub renders a "Cite this repository" button automatically when it detects this file.

See `/CITATION.cff` (already added to repo root). GitHub will now render a "Cite this repository" button automatically. Anyone citing AC in papers, exhibitions, or grant applications gets a machine-readable citation with your ORCID linked.

### 2. GitHub + Zenodo Integration (Citable DOIs)

Connect the pipeline: **GitHub releases -> Zenodo -> DOI -> ORCID profile**

- Link your GitHub account to [Zenodo](https://zenodo.org)
- Link your ORCID to Zenodo (under Profile > Linked Accounts)
- Enable the `aesthetic-computer` repo in Zenodo's GitHub settings
- Each GitHub release automatically gets a DOI minted by Zenodo
- Zenodo pushes the citation to your ORCID profile via DataCite

This means every tagged release of AC becomes a citable, archived scholarly object with a permanent DOI. Useful for grant applications, exhibition credits, and academic references.

### 3. Populate Your ORCID Profile (Developer Tools API)

With the developer tools you registered, you can use the **Public API** to:

- **Read** your ORCID record programmatically
- **Authenticate** users via "Sign in with ORCID" (OAuth 2.0)
- **Search** the ORCID registry

To **write** to your record (add works, affiliations, etc.), you need the **Member API** (requires institutional membership) or can do it manually / via Zenodo auto-sync.

### 4. Embed ORCID in Project Metadata

Places to surface your ORCID across the AC ecosystem:

| Location | How |
|---|---|
| `CITATION.cff` | `orcid:` field under authors |
| `package.json` | Add to author/contributors metadata |
| `codemeta.json` | Schema.org-compatible software metadata with `@id` ORCID URL |
| `SCORE.md` / README | Link in contributor credits |
| KidLisp pieces | Metadata headers for published pieces could include creator ORCID |

### 5. `codemeta.json` (Software Metadata Standard)

See `/codemeta.json` (already added to repo root). This Schema.org-compatible metadata file makes AC discoverable by software registries and academic indexers.

### 6. ORCID in Grant & Funding Applications

If you apply for grants (NEA, Creative Capital, Rhizome, etc.), your ORCID iD:
- Links directly to all your registered works
- Provides a stable identity across funding platforms
- Some funders now require or prefer ORCID iDs
- Auto-populates SciENcv profiles (for US federal grants)

### 7. Piece Publishing & Attribution

Future possibility for AC's social layer: when users publish pieces, their ORCID could serve as a verified creator identity, distinct from the `@handle` system but linkable to it. This creates a bridge between AC's social network and the broader academic/creative citation ecosystem.

## AC Components Ripe for Individual Citation / DOI

The monorepo contains several distinct subsystems that could each receive their own `CITATION.cff`, Zenodo DOI, or ORCID-linked work entry. These are ranked by standalone potential and scholarly/creative interest.

### Tier 1: High standalone value, ready now

| Component | What it is | Why it's citable |
|---|---|---|
| **KidLisp** | Minimal Lisp dialect (118 built-in functions) for generative art | A novel language with its own evaluator, comprehensive docs, and test suite. Publishable as a standalone creative coding language. Could get its own paper, DOI, and npm package. |
| **Disk API / Piece Runtime** | Core framework: `boot.mjs`, `bios.mjs`, `disk.mjs` (~572KB), 50+ support libs | The creative computing runtime itself — immediate-mode graphics, input, audio, UI, networking. Comparable to Processing or p5.js in scope. A framework-level contribution worth citing independently. |
| **MCP Server** (`@aesthetic.computer/mcp`) | Model Context Protocol server for AI-assisted piece creation | Already published to npm. Represents a novel intersection of creative computing + LLM tooling. Citable as a tool/protocol contribution. |
| **VSCode Extension** | KidLisp syntax highlighting, 11+ themes, piece commands | Already published to VSCode Marketplace. Citable as a developer tool. |

### Tier 2: Strong standalone potential, needs minor extraction

| Component | What it is | Why it's citable |
|---|---|---|
| **Session Server** | Real-time multiplayer backend (Geckos.io, Jamsocket, Redis) | Own `package.json`, own infrastructure. Reusable pattern for ephemeral multiplayer creative apps. Could be written up as an architecture contribution. |
| **Oven** | Video processing & screenshot service (Puppeteer, Sharp, FFmpeg) | Own `package.json`, own DigitalOcean deployment. Media pipeline for creative computing platforms. |
| **AC-Electron** | Cross-platform desktop app (Electron + xterm + Three.js) | Own `package.json`, GitHub releases with auto-update. Represents the "instrument as desktop app" concept. |
| **DP1-Feed** | OpenAPI 3.1.0 server for blockchain art playlists (Feral File) | Already has a separate GitHub repo. MPL-2.0 licensed. Citable as a digital art exhibition protocol. |

### Tier 3: Interesting for specialized audiences

| Component | What it is | Why it's citable |
|---|---|---|
| **Grab** | Cloudflare Worker for serverless browser rendering | Reusable edge-rendering pattern. |
| **Silo** | MongoDB/Redis data dashboard | Data management tooling. |
| **AT Tools** | ATProto/Bluesky integration experiments | Fediverse publishing from creative platforms. |
| **AestheticAnts** (`ants/`) | Automated repo maintenance system | Novel approach to AI-assisted codebase maintenance. |

### How to give each component its own DOI

For Tier 1 components especially:

1. **Create a tagged release** for each component (e.g., `kidlisp-v1.0.0`)
2. **Add a component-level `CITATION.cff`** in each subdirectory with your ORCID
3. **Register each as a separate Zenodo record** — Zenodo supports linking related records (e.g., "is part of" the AC umbrella DOI)
4. **List each as a separate "work" on your ORCID profile** — distinguishes the language from the runtime from the tool from the platform

This is how large research software projects (like Jupyter, SciPy) handle sub-component citation — the umbrella project gets one DOI, and significant sub-components get their own.

### The KidLisp case is especially strong

KidLisp has everything a publishable language needs:
- 118 documented built-in functions across 12 categories
- Own evaluator (`kidlisp.mjs`)
- Comprehensive README/spec (`kidlisp/README.md`)
- Test suite (`spec/`)
- Analysis tools (`kidlisp/tools/`)
- 16,174+ user-created programs (a dataset in its own right)
- A novel design philosophy (minimal Lisp for generative art, designed for non-programmers)

A short paper at something like FARM (Functional Art, Music, Modeling & Design) or SPLASH/Onward! would be a natural fit, and the ORCID + DOI infrastructure makes it properly citable from day one.

## Zenodo Depositions (Draft)

Both are draft depositions — upload a release archive and publish to mint the DOI.

| Record | Zenodo ID | Pre-reserved DOI | Status |
|---|---|---|---|
| **Aesthetic Computer** | 18854192 | `10.5281/zenodo.18854192` | Draft |
| **KidLisp** | 18854196 | `10.5281/zenodo.18854196` | Draft (linked as "isPartOf" AC) |

Manage at: `https://zenodo.org/deposit/18854192` and `https://zenodo.org/deposit/18854196`

## Status

- [x] `CITATION.cff` added to repo root
- [x] `codemeta.json` added to repo root
- [x] Add ORCID link to `SCORE.md` contributor section
- [x] Zenodo API tested and working (PAT in vault)
- [x] Draft depositions created (AC + KidLisp) with pre-reserved DOIs
- [x] `kidlisp/CITATION.cff` created
- [ ] Upload release archives to Zenodo drafts and publish
- [ ] Link GitHub account to Zenodo for auto-DOI on future releases
- [ ] Explore ORCID-based creator attribution in AC's piece publishing system
- [ ] Use ORCID API to pull/display your works on aesthetic.computer

## Grants & Funding Opportunities

### Tier 1: Strong fit, apply now or soon

| Opportunity | Amount | Deadline | AC Angle |
|---|---|---|---|
| [Creative Capital Award](https://creative-capital.org/creative-capital-award/) | Up to $50,000 | **Apr 2, 2026** (for 2027 cycle) | Technology + art platform. AC as an artistic instrument/social network. |
| [NEA Grants for Arts Projects](https://www.arts.gov/grants/grants-for-arts-projects) | $10,000–$100,000 | **Jul 9, 2026** | "Technology-centered creative practices" — explicitly includes AI-exploring work. AC fits perfectly. |
| [NLnet NGI Zero Commons Fund](https://nlnet.nl/commonsfund/) | €5,000–€50,000+ | **Apr 1, 2026** | Open-source infrastructure. KidLisp as a public-good creative language, AC as open digital commons. |
| [Sovereign Tech Fund](https://www.sovereign.tech/programs/fund) | Varies (substantial) | Rolling | Open-source digital base technology. AC's runtime, KidLisp evaluator, session protocol. |
| [Eyebeam Open Call 2026](https://eyebeam.org/open-call-2026/) | Residency + production support | Check site | 3-month NYC residency (Apr–Jun 2026). Open-source ethos aligned. Requires NYC residency. |
| [LACMA Art + Tech Lab](https://www.lacma.org/art/lab/grants) | Up to $50,000 | **Apr 22, 2026** | Art+technology project. Global eligibility. 2-year duration. Very strong fit. |
| [Spencer Foundation Small Grants](https://www.spencer.org/grant_types/small-research-grant) | Up to $50,000 | **Apr 15, 2026** | Education research — KidLisp as creative computing pedagogy. |

### Tier 2: Good fit, upcoming or annual

| Opportunity | Amount | Deadline | AC Angle |
|---|---|---|---|
| [Rhizome Microgrants](https://rhizome.org/editorial/2025/sep/15/open-call-microgrants-2025/) | $500–$1,500 | Annual (usually fall) | "Alternatives for digital culture" — AC as a community-owned creative platform. |
| [Processing Foundation Fellowship](https://processingfoundation.org/fellowships) | $3,000 | Annual (usually Jan) | Creative coding community tool. KidLisp or AC education integration. |
| [GitHub Secure Open Source Fund](https://github.com/open-source/github-secure-open-source-fund) | $10,000 | Annual cycle | Security hardening of AC's open-source infrastructure. |
| [Sloan Foundation — Better Software for Science](https://sloan.org/programs/digital-technology/better-software-for-science) | Varies (up to $750K for OSPOs) | LOI-based | KidLisp as research software. AC as creative computing research infrastructure. Needs institutional affiliation. |
| Creative Capital State of the Art Prize | $10,000 | Same as CC Award | Unrestricted artist grant — broader eligibility. |

### Tier 3: Festivals & prizes (visibility + small money)

| Opportunity | Amount | Deadline | AC Angle |
|---|---|---|---|
| [Prix Ars Electronica — Interactive Art +](https://ars.electronica.art/prix/en/categories/interactive-art/) | Golden Nica + €10,000 | **Mar 4, 2026** (just passed — next year) | AC as interactive art platform. Submit a piece or the platform itself. Festival: Sep 9–13, Linz. |
| [SIGGRAPH 2026 Art Gallery](https://s2026.siggraph.org/program/art-gallery/) | Exhibition | Check deadlines | AC pieces as computational art. Los Angeles, Jul 19–23. |
| [ISEA 2026 Dubai](https://isea-2026.isea-international.org/) | Exhibition/presentation | Dec 20, 2025 (passed) | "Constellating Place, Data and Identity" — Apr 10–19, Dubai. Next year. |

### Federal arts funding caveat

NEA and NEH have been disrupted — the administration proposed eliminating both agencies, hundreds of grants were canceled, and a federal judge ruled the cancellations "likely unconstitutional." Congress passed level funding ($207M each) but operations remain unstable. Approach federal grants with backup plans.

### How ORCID helps with grants

- **NEA & federal grants**: ORCID auto-populates SciENcv profiles
- **Creative Capital**: ORCID links your works portfolio for reviewers
- **NLnet/Sovereign Tech**: Shows established open-source track record via linked DOIs
- **All applications**: A single URL (orcid.org/0009-0007-4460-4913) that proves authorship across AC, KidLisp, publications, exhibitions

## Academic Publication & Conference Venues

### Papers about KidLisp / AC

| Venue | Type | Fit | Typical Deadline |
|---|---|---|---|
| [FARM (Functional Art, Music, Modelling & Design)](https://functional-art.org/) | ACM workshop (co-located with ICFP) | **Best fit.** KidLisp as functional creative language, with 16K+ user programs as evaluation data. | ~June |
| [NIME 2026](https://nime2026.org/) | Conference on new interfaces for musical expression | AC as a musical instrument interface. Notepat, live coding with KidLisp. Theme: "Communities." | Check site |
| SPLASH/Onward! | ACM conference on programming languages | KidLisp language design, AC runtime architecture. | ~April |
| CHI (alt.chi or interactivity) | ACM conference on human-computer interaction | AC's UX design, social creative computing. | ~Sept (for next year) |
| [SIGGRAPH Technical Workshops](https://s2026.siggraph.org/program/technical-workshops/) | Workshop proposals | Creative computing runtime design, GPU primitives in AC. | Check site |
| [ELS 2026](https://www.european-lisp-symposium.org/2026/) | European Lisp Symposium (Krakow + online) | **Ideal venue for KidLisp.** Lisp dialect community. | May 11-12, check CFP |
| [JOSS](https://joss.theoj.org) | Journal of Open Source Software | **Free** peer review, CrossRef DOI, Google Scholar indexed. ~1 page paper. Best ROI. | Rolling |
| [ICLC](https://iclc.toplap.org/) | Int'l Conference on Live Coding | AC as a live coding environment, KidLisp performances. | TBD |

### Art exhibitions & festivals

| Venue | Type | AC Angle |
|---|---|---|
| Ars Electronica Festival (Linz, Sep 2026) | Exhibition + symposium | AC as interactive art platform |
| Transmediale (Berlin, annual) | Festival for art and digital culture | AC as alternative social network |
| Eyebeam (NYC) | Residency + exhibition | Open-source creative tool development |
| Rhizome (online/NYC) | Net art preservation + exhibition | AC pieces as born-digital art |
| NEW INC (New Museum, NYC) | Incubator | AC as creative tech startup |

### Publication strategy

1. **Short paper at FARM 2026** (~June deadline): "KidLisp: A Minimal Lisp for Generative Art" — language design, 118 functions, 16K user programs as evaluation
2. **NIME 2026 demo**: Notepat as a new musical interface, live-coded KidLisp performances
3. **Full paper at SPLASH/Onward!**: "Aesthetic Computer: A Runtime for Memorizable Creative Computing" — architecture, social network, piece lifecycle
4. **Exhibition at Ars Electronica / SIGGRAPH Art Gallery**: Submit AC itself or a curated set of pieces

Each publication mints a new DOI via the conference proceedings, all linked back to your ORCID.

## Free Research Network & Promotion Channels

### Preprint / Open Access Publication (free, gets DOI)

| Venue | Cost | What to publish | Notes |
|---|---|---|---|
| [arXiv (cs.PL or cs.HC)](https://arxiv.org/) | Free | KidLisp language paper, AC runtime paper | World's largest preprint server. Instant visibility. cs.PL (programming languages) or cs.HC (human-computer interaction). Needs an endorser for first submission — ask any CS academic. |
| [JOSS (Journal of Open Source Software)](https://joss.theoj.org) | **Free** (diamond open access) | AC or KidLisp as research software | Peer-reviewed, gets a CrossRef DOI, indexed in Google Scholar. Review happens on GitHub. If your docs + tests are solid (they are), the paper is ~1 page describing the software. **Best ROI for effort.** |
| [TechRxiv](https://www.techrxiv.org/) | Free | Technical papers | IEEE-affiliated preprint server. Good for engineering-focused writeups. |
| [Zenodo](https://zenodo.org) | Free | Software releases, datasets, whitepapers | Already set up. Upload your `sosoft/proposal.pdf` as a whitepaper right now — it gets a DOI. |

### Lisp / Language Design Communities (free exposure)

| Community | URL | How to engage |
|---|---|---|
| [Scheme Community](https://community.scheme.org/) | community.scheme.org | Post KidLisp as a "Scheme-adjacent" creative dialect. The Scheme Workshop (co-located with ICFP) accepts demos. |
| [Racket Discourse](https://lists.racket-lang.org) | lists.racket-lang.org | Racket people love language design stories. KidLisp's "118 functions for generative art" is a great post. |
| [Common-Lisp.net](https://common-lisp.net/) | common-lisp.net | Mailing lists, project listings. |
| [lisp.community](https://lisp.community/) | lisp.community | Aggregator of Lisp communities across dialects. |
| IRC: `#lisp`, `#scheme`, `#racket` on Libera Chat | libera.chat | Real-time. Share KidLisp, get feedback, find collaborators. |
| [r/lisp](https://reddit.com/r/lisp), [r/scheme](https://reddit.com/r/scheme) | reddit.com | Good for "Show" posts with screenshots/demos. |

### Creative Coding Communities (free exposure)

| Community | URL | How to engage |
|---|---|---|
| [OpenProcessing](https://openprocessing.org/) | openprocessing.org | 100K+ creative coders. Cross-post KidLisp pieces or AC demos. |
| [awesome-creative-coding](https://github.com/terkelg/awesome-creative-coding) | GitHub | Submit a PR to get AC listed. High-visibility curated list. |
| [Gray Area](https://grayarea.org/) | grayarea.org | SF-based. Creative Code Intensive courses. Workshop/talk opportunities. |
| [Creative Coding Discord](https://discord.gg/creativecoding) | Discord | Active community. Share work, get feedback. |
| [lines.org (Monome community)](https://llllllll.co/) | llllllll.co | Instrument-minded creative coders. AC's "instrument metaphor" fits perfectly here. |

### Hacker News / Tech Communities (free, high-visibility)

| Channel | Strategy |
|---|---|
| **Show HN** | You already had [Notepat on HN](https://news.ycombinator.com/item?id=41526754). Do a "Show HN: KidLisp — a 118-function Lisp for generative art" post linking to kidlisp.com. |
| **Show HN: Aesthetic Computer** | Broader platform launch post. Link to aesthetic.computer, emphasize the "memorizable paths" instrument design. |
| [Lobste.rs](https://lobste.rs/) | Invite-only but focused technical audience. Great for language design posts. |
| [Tildes](https://tildes.net/) | Smaller, higher-quality discussions. Creative computing fits ~creative group. |

### Academic Networks (free, builds citation graph)

| Platform | URL | What to do |
|---|---|---|
| [Google Scholar](https://scholar.google.com/) | scholar.google.com | Create a profile linked to your ORCID. Any arXiv/JOSS/Zenodo publications auto-index here. |
| [Semantic Scholar](https://www.semanticscholar.org/) | semanticscholar.org | AI-powered academic search. Auto-indexes arXiv and DOI papers. |
| [DBLP](https://dblp.org/) | dblp.org | CS bibliography. Auto-indexes conference/journal papers. |
| [ResearchGate](https://www.researchgate.net/) | researchgate.net | Upload preprints, connect with researchers. |
| [ORCID Works](https://orcid.org/) | orcid.org | Every DOI (Zenodo, JOSS, arXiv, conference) auto-populates your profile via DataCite. |

### Whitepaper Strategy (using your existing LaTeX setup)

Your `sosoft/proposal.tex` is already a polished academic-quality document with AC's full architecture, live data, and piece catalog. Here's how to turn it into citable research:

1. **Upload `proposal.pdf` to Zenodo now** — it gets a DOI immediately. Title it "Aesthetic Computer: A Mobile-First Runtime for Creative Computing." Link your ORCID. This is your first "work" on your ORCID profile.

2. **Write a JOSS paper** — Fork your LaTeX template into a `papers/` directory. JOSS papers are short (~1-2 pages): software description, statement of need, key features, community. KidLisp or AC both qualify. Free peer review, free DOI, indexed in Google Scholar.

3. **arXiv preprint for KidLisp** — A 4-6 page paper: "KidLisp: A Minimal Lisp for Generative Art." Include language design rationale, the 118 functions, evaluation (16K user programs as corpus data). Submit to cs.PL. This is your calling card in the PL research community.

4. **FARM 2026 submission** (~June deadline) — Adapt the arXiv paper for the workshop format. FARM loves creative language design + live performance. You could demo KidLisp live at the evening performance.

### The Network Effect

```
Zenodo DOI ──→ ORCID profile ──→ Google Scholar
     │                                    │
     ▼                                    ▼
arXiv preprint ──→ Semantic Scholar ──→ citation graph
     │                                    │
     ▼                                    ▼
JOSS publication ──→ DBLP ──→ discoverable by researchers
     │
     ▼
Conference paper (FARM/NIME/CHI) ──→ ACM Digital Library
```

Each publication creates a node. ORCID ties them together. Researchers finding one paper find all your work. The citation graph compounds over time — every new paper that cites KidLisp also surfaces AC, and vice versa.

### Immediate free actions (today)

1. Upload `sosoft/proposal.pdf` to Zenodo draft (already created, ID: 18854192)
2. Post KidLisp to awesome-creative-coding GitHub list (PR)
3. Create Google Scholar profile linked to ORCID
4. Post on r/lisp and Scheme community about KidLisp
5. Start JOSS paper in `papers/` directory using your LaTeX template

## Resources

- [CITATION.cff Format Spec](https://citation-file-format.github.io/)
- [GitHub: About CITATION files](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-citation-files)
- [Zenodo: CITATION.cff integration](https://help.zenodo.org/docs/github/describe-software/citation-file/)
- [GitHub-Zenodo-ORCID integration guide](https://inbo.github.io/checklist/articles/zenodo.html)
- [ORCID API Tutorial](https://orcid.github.io/orcid-api-tutorial/)
- [ORCID Developer Tools FAQ](https://info.orcid.org/documentation/integration-and-api-faq/)
- [CodeMeta Generator](https://codemeta.github.io/codemeta-generator/)
- [Zenodo: Linking Accounts](https://help.zenodo.org/docs/profile/linking-accounts/)
- [Making Your Code Citable (TU Hamburg guide)](https://www.tub.tuhh.de/en/2024/03/03/making-your-code-citable/)
