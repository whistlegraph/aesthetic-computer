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

## Status

- [x] `CITATION.cff` added to repo root
- [x] `codemeta.json` added to repo root
- [ ] Add ORCID link to `SCORE.md` contributor section
- [ ] Set up Zenodo-GitHub integration for automatic DOIs
- [ ] Create component-level `CITATION.cff` files (KidLisp first)
- [ ] Explore ORCID-based creator attribution in AC's piece publishing system
- [ ] Use ORCID API to pull/display your works on aesthetic.computer

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
