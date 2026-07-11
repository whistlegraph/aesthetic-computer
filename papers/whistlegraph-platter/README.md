# Whistlegraph Platter

An index of whistlegraph materials across the repo, the web, and dropbox. A sub-platter within the [papers platter](../SCORE.md) — source material that feeds the arxiv paper, the are.na channel, the Annual Vol. 8 pitch, and anything else that comes next.

> **Whistlegraph** — a drawing form invented in 2019 where every line is a sung syllable. You draw it while you sing it. The finished drawing is a score. The score teaches you how to play it.

This is **index-only**: pointers to where things live. No file migration, no binary duplication. Extend sections as material is cataloged.

---

## 1. In this repo

### Paper
- [papers/arxiv-whistlegraph/whistlegraph.tex](../arxiv-whistlegraph/whistlegraph.tex) — main arxiv paper, 4pp, "Whistlegraph: Drawing, Singing, and the Graphic Score as Viral Form"
- [papers/arxiv-whistlegraph/whistlegraph-cards.tex](../arxiv-whistlegraph/whistlegraph-cards.tex) — single-sheet card version
- Translations: [da](../arxiv-whistlegraph/whistlegraph-da.tex), [es](../arxiv-whistlegraph/whistlegraph-es.tex), [ja](../arxiv-whistlegraph/whistlegraph-ja.tex), [zh](../arxiv-whistlegraph/whistlegraph-zh.tex)
- Built PDFs: [en](../arxiv-whistlegraph/whistlegraph.pdf), [da](../arxiv-whistlegraph/whistlegraph-da.pdf), [es](../arxiv-whistlegraph/whistlegraph-es.pdf), [zh](../arxiv-whistlegraph/whistlegraph-zh.pdf)
- Published PDFs (papers.aesthetic.computer): [en](../../system/public/papers.aesthetic.computer/whistlegraph-26-arxiv.pdf), [cards](../../system/public/papers.aesthetic.computer/whistlegraph-26-arxiv-cards.pdf), [da](../../system/public/papers.aesthetic.computer/whistlegraph-26-arxiv-da.pdf), [es](../../system/public/papers.aesthetic.computer/whistlegraph-26-arxiv-es.pdf), [zh](../../system/public/papers.aesthetic.computer/whistlegraph-26-arxiv-zh.pdf)
- Figures — 10 score PNGs in [papers/arxiv-whistlegraph/figures/](../arxiv-whistlegraph/figures/), each a symlink into `system/public/assets/whistlegraph/<piece>/` (DO Spaces; pulled locally 2026-05-02 via scoped `aws s3 sync`).
- [papers/arxiv-whistlegraph/pals.pdf](../arxiv-whistlegraph/pals.pdf) — referenced inclusion
- [papers/arxiv-whistlegraph/references.bib](../arxiv-whistlegraph/references.bib) — bibliography

### Practice surface (aesthetic.computer)
- [system/public/aesthetic.computer/disks/whistlegraph.mjs](../../system/public/aesthetic.computer/disks/whistlegraph.mjs) — the piece: draw-while-singing, record, learn, perform
- [system/public/aesthetic.computer/disks/whistlegraph/whistlegraph-cards.js](../../system/public/aesthetic.computer/disks/whistlegraph/whistlegraph-cards.js) — card-based score browser
- Live URL: https://aesthetic.computer/whistlegraph

### Synced assets — per-piece score↔video pairs
`system/public/assets/whistlegraph/<piece>/` (1.0 GB, 47 files, synced 2026-05-02 from `s3://assets-aesthetic-computer/whistlegraph/`; assets dated 2023-10-21).

**10 pieces with consistent structure** — each has `<piece>-score.png` + `<piece>-tt-compilation.mp4` + `<piece>-web.mp4` + `<piece>.webp`:
- butterfly-cosplayer · i-dont-need-an-iphone · lately-when-i-fly · loner · mommy-wow · people-pleaser · puzzle · slinky-dog · time-to-grow · whats-inside-your-heart

**1 outlier piece** — `music-2-whistlegraph-2/`: `intro.mp4`, `poster.webp`, `spines.webp`, `.webp`. No score PNG, no tt-compilation. Different shape; may be a music-track piece rather than a score-based whistlegraph.

**Per-piece file roles**:
- `<piece>-score.png` (~400 KB – 1.8 MB) — clean graphic score, paper figure source
- `<piece>-tt-compilation.mp4` (~28 – 96 MB) — TikTok compilation: **multiple takes of the same piece in one file**. Primary signal for variation analysis (tempo / stroke-order / vocal phrasing across recordings of the same score).
- `<piece>-web.mp4` (~18 – 46 MB) — single web-friendly recording
- `<piece>.webp` — poster for card-deck UI

**Card-deck UI assets** (top level): `next-arrow.svg`, `play-circle.svg`, `play-triangle.svg`

> The 10 piece dirs ARE the score↔video pairs already organized by directory name. No further pairing work needed for these — the analyzer can iterate `<piece>/` and have score + N-take video alongside. The tt-compilation files are the unfair advantage: each one is a built-in dataset of how the same score gets re-performed.

### Standalone site
- [system/public/whistlegraph.org/index.html](../../system/public/whistlegraph.org/index.html) — **whistlegraph.org** — the live archive/index. 268 recovered compositions (sortable: featured / videos / sung / year / title / author) + a **Research** tab for drawing experiments; a **Lectures, shows & press** section (talks, livestreams, exhibitions, workshops, press) with verified links; the Sex Magazine zine with cover; and an example code that randomizes to a featured piece each load. Data in [graphs.json](../../system/public/whistlegraph.org/graphs.json). Detail pages carry per-graph `versions[]` (e.g. Underpainting's two performances). Live: https://whistlegraph.org
- [sites/whistlegraph.com/index.html](../../sites/whistlegraph.com/index.html) — whistlegraph.com landing page

### Are.na Annual Vol. 8 submission (2026-04-20)
- [gigs/are-na-annual-vol-8/](../../gigs/are-na-annual-vol-8/) — pitch package, channel seed scripts, block validation
- [system/public/are.na-annual/index.html](../../system/public/are.na-annual/index.html) — published pitch page
- Live: https://aesthetic.computer/are.na-annual

---

## 2. External

### Whistlegraph-owned domains and accounts
- **TikTok** — https://www.tiktok.com/@whistlegraph · ~2.7M followers (peak, 2020–2023)
- **Instagram (post-2023, Jeffrey solo)** — https://www.instagram.com/whistlegraph/
- **Instagram (trio era, 2020–2023)** — https://www.instagram.com/whistlegraph.trio/
- **YouTube** — https://www.youtube.com/channel/UCZ_5AuCebRbm9t9_Y7SrckQ (incl. *Whistlegraph LIVE: Cat Charmer*, *WHISTLEGRAPH TIKTOKS 2019* compilation)
- **X / Twitter** — https://x.com/whistlegraph
- **GitHub** — https://github.com/whistlegraph (the org; @jeffrey is prompt.ac handle)
- **Trio site** — https://trio.whistlegraph.com/about
- **Shop** — https://shop.whistlegraph.com (zine, editions)
- **One-pager** — https://whistlegraph.mmm.page
- **Fan wiki** — https://whistlegraph.fandom.com
- **Are.na channel** — https://www.are.na/aesthetic-computer/self-teaching-scores · 68 annotated blocks

### Institutional / commissions
- **Rhizome × New Museum** — *The Longest Whistlegraph Ever (so far)*, commissioned 2021, debuted at the New Museum on **2022-05-14** as part of *First Look*. Dedicated site: https://sites.rhizome.org/the-longest-whistlegraph-ever-so-far/ · Editorial: https://rhizome.org/editorial/2022/sep/13/first-look-the-longest-whistlegraph-ever-so-far/ · Workshops call: https://cdn.rhizome.org/editorial/2022/mar/02/apply-to-whistlegraph-workshops/ · Event page: https://rhizome.org/events/the-longest-whistlegraph/
- **Feral File** — artist page https://feralfile.com/artists/Whistlegraph · *Ten Whistlegraphs* exhibition, May 2022 · also featured in **FF1** retrospective https://retrospective.feralfile.com/ff1
- **Schneider Museum of Art** (Southern Oregon University, Ashland) — https://sma.sou.edu/whistlegraph/ · *Virtual Creative Industries Discussion*: https://sma.sou.edu/virtual-creative-industries-discussion-whistlegraph/ · *FREE Family Days with Whistlegraph*: https://sma.sou.edu/free-family-days-with-whistlegraph/
- **KADIST** — Jeffrey Alan Scudder artist page: https://kadist.org/people/jeffrey-alan-scudder/ · *TBD: confirm specific whistlegraph work(s) in KADIST's collection*
- **SMK** (Statens Museum for Kunst, Copenhagen) — *TBD: confirm. General search didn't surface a direct whistlegraph holding; may need to cross-check with Jeffrey's CV*

### Related Jeffrey Alan Scudder pages
- **Artist archive** — https://jas.life
- **Wild.xyz** — https://wild.xyz/jeffrey-scudder/

### Press, prior writing, context
- **The Whistlegraph Zine** (2023-06-06) — 60 pages, ed. Asher Penn for *Sex Magazine*, **750 copies**. Long-form interviews with Jeffrey / Alex / Camille + two fan entries.
  - Shop (Whistlegraph): https://shop.whistlegraph.com/products/the-whistlegraph-zine
  - Shop (Sex Magazine): https://sexmag.shop/products/the-whistlegraph-zine
- **Jacob Ciocci** — *The Butterfly Effect / Rules Set You Free* — essay in the Whistlegraph Zine. Places the form in the rule-based community-art / Paper Rad lineage. Ciocci also contributed an intro.
- **Perry** — intro in the Whistlegraph Zine (*TBD: full name / attribution*)
- **Cmarthoughts** — *The Longest Whistlegraph Ever — Body Language*: https://cmarthoughts.com/whistlegraph-body-language/
- *TBD: additional reviews, interviews, press mentions (check FADER, Artnews, other mags)*

### Frequent collaborators beyond the trio
Per Schneider Museum page: Ella Fleck, Niki Stebbins, Matt Doyle, Anastasia Lewis, Adam Schwarz, Ash Nerve, plus an online community of artists and fans.

---

## 3. Dropbox archive — `/Whistlegraph/`

**Total: 12,903 files · 421.66 GB · 49 top-level subfolders** (manifest pulled 2026-05-03 via Dropbox HTTP API).

**Not locally synced** — accessed via the API using the OAuth token in [`aesthetic-computer-vault/dropbox/auth.json`](../../aesthetic-computer-vault/dropbox/) (dbxcli format). Manifest tooling: [`tools/dropbox-manifest.mjs`](tools/dropbox-manifest.mjs) → outputs [`dropbox-manifest.md`](dropbox-manifest.md) and [`dropbox-manifest.json`](dropbox-manifest.json).

### Performance & recording
- `Whistlegraph/TikTok/` — **233 files, 10.94 GB** · 73 subdirs incl. `Collabs/bo en/` (Bo En collaborations, late Jan–Feb 2021 .mov files)
- `Whistlegraph/YouTube/` — **278 files, 43.48 GB** · gigabyte-class long-form (`Improv.MOV` 1.32 GB, `relaxing-horror-nightmare-show-s1e1.mov` 1.52 GB, `IMG_7329.mov` 3.38 GB)
- `Whistlegraph/Instagram/` — 14 files, 27.2 MB (thin — IG archive is small)
- `Whistlegraph/Live/` — **151 files, 11.45 GB** · streaming assets (overlays, scenes JSON), live performance captures
- `Whistlegraph/Music/` — **1569 files, 23.84 GB** · incl. `Music/Goodiepal Recording Session/prut mixene/` (whistle montage WAVs ~200 MB each)
- `Whistlegraph/Audio Drafts/` — 1 file, 375 KB (`camille run 22.7.12.m4a`)
- `Whistlegraph/Recorder/` — 3 files, 519 KB (just UI assets — NOT actual recordings)
- `Whistlegraph/Misc. Videos/` — empty in the manifest pull (folder visible, contents not enumerated; investigate)

### Scores & drawing source (primary analysis material)
- `Whistlegraph/Scores/` — **39 files, 51.4 MB** · Score Rev1 PDF subdirectory contains `i-dont-need-an-iphone.pdf`, `its-time-to-grow.pdf`, `lately-when-i-fly.pdf`, `loner.pdf` — **same piece names as the paper figures and the per-piece DO Spaces dirs.** Rosetta confirmed: each piece has a DO Spaces tt-compilation MP4 + a Dropbox Score PDF.
- `Whistlegraph/iPad Procreate Backups/` — **33 files, 6.38 GB** · `.procreate` bundles with **native per-stroke playback timing**. Highest-leverage analysis input.
- `Whistlegraph/Pixaki Updates/` — 87 files, 280 KB · `.pixaki` projects (frame-by-frame; tiny).
- `Whistlegraph/Manuscripts/` — 22 files, 86.3 MB · per-piece manuscript pages (incl. `slinky dog manuscripts/`)
- `Whistlegraph/Research/Whistlegraphs for GAN/` — **78 files, 217 MB** · already-curated JPEG dataset prepped for ML training. *Pre-existing prior work — re-use.*
- `Whistlegraph/Large iPad Procreate Update/` — 5 files, 824 MB · large procreate exports (`Spiral_Smile.procreate` 115 MB).

### Exhibitions & press
- `Whistlegraph/Exhibitions/` — **4426 files, 253.93 GB** (largest single subfolder) · Rhizome *Longest Whistlegraph Ever* microsite media, manuscript selects
- `Whistlegraph/The Longest Whistlegraph Ever (so far) Media Archive.zip` — single 19.24 GB zip at root
- `Whistlegraph/Whistlegraph Full Cover Ad Shoot/` — 218 files, 11.64 GB
- `Whistlegraph/Sex Magazine/` — 2522 files, 10.97 GB · zine production files (FINAL INDD, fonts, layout links)
- `Whistlegraph/Sex Magazine Launch/` — 101 files, 226 MB
- `Whistlegraph/NFTs/` — 14 files, 205 MB
- `Whistlegraph/Press/`, `Whistlegraph/Commercials/`, `Whistlegraph/Lectures/` (3.59 GB), `Whistlegraph/Scholarly Articles:Academia/`
- `Whistlegraph/Write a Whistlegraph Workshop/` — 43 files, 868 MB · social-media promo `.mov` files (Triangle, Factory, Puzzle)
- `Whistlegraph/Stamps/`, `Whistlegraph/Shop/`

### Community
- `Whistlegraph/Media from Fans/` — 43 files, 635 MB · incl. *The Wonderful World Of Whistlegraphs I: A Documentary.mp4* (166 MB)
- `Whistlegraph/Media from Friends/` — 301 files, 13.05 GB · large per-friend subdirs (e.g. *From Katrín Helga Ólafsdóttir Whistlegraph Copenhagen Sept 3 2021*)
- `Whistlegraph/Comments Screenshots/` — 1 file, 231 KB

### Trio internal
- `Whistlegraph/Whistlegraph Minutes/` — 4 files, 470 MB · zoom recordings (March 25 trio meeting w/ Ella, Camille, Alex, Jeffrey)
- `Whistlegraph/Hand Health/` — 8 files, 5.86 GB · large `.m4v` therapy videos
- `Whistlegraph/Moon Paintings/` — 16 files, 2.5 GB · collaborative paintings w/ Alex F. + CK (massive PSDs)
- `Whistlegraph/No Paint/` — **2202 files, 140 MB** · the No Paint app source/assets (jeffrey + alex's earlier project)

### Business (preserve, exclude from analyzer corpus)
- `Whistlegraph/LLC/` — 6 files, 1.3 MB · Articles, EIN, Operating Agreement, W9, f8832
- `Whistlegraph/Invoices, Bills & Contracts/` — 19 files, 17.7 MB
- `Whistlegraph/Signed Documents/`, `Whistlegraph/Taxes/`

### Loose root-level files
`Bumbling Bumperfly.mp4` (29.9 MB), `whistlegraph-tiktok.mov` (7.1 MB), `playbuttons.png` (14.2 MB), `playbuttonassets.ai`, `Whistlegraph Account Screenshot (Dark).jpg`, `Capture.PNG`, `Whistlegraph.png`, `Rhizome - Workshops Blog Post.zip` (8.2 MB).

### File-type signal (analyzer-relevant)
| Type | Count | Notes |
|---|---:|---|
| `.mp4` + `.mov` + `.webm` + `.m4v` | 1551 | performance video corpus |
| `.wav` + `.aif` + `.m4a` + `.mp3` + `.aac` | 1331 | audio takes/stems |
| `.png` + `.jpg` + `.jpeg` + `.heic` + `.webp` + `.gif` | 6225 | stills, score scans, manuscripts |
| `.procreate` | 40 | **per-stroke timing native** |
| `.pixaki` projects | (in Pixaki Updates) | frame-by-frame source |
| `.als` (Ableton) + `.aep` (After Effects) + `.prproj` (Premiere) + `.psd` + `.indd` | 268 | composition/edit projects |
| `.pdf` | 120 | scores, manuscripts, contracts |
| `.srt` | 26 | subtitles (alignment ground truth) |

---

## 4. Collaborators (2019–2023 trio)

- **Jeffrey Alan Scudder** — creator, aesthetic.computer, @jeffrey
- **Alex Freundlich**
- **Camille Klein**

- **Form invented** by Jeffrey in **2019**. **Trio formed in 2020** (pandemic, cabin in Ashland, Oregon). **Separated November 2023.** December 2023, Jeffrey took solo access to all of the group's social media. The form continues under aesthetic.computer.

---

## 5. Timeline (rough spine)

- **2019** — Jeffrey invents the form. Early TikTok posts.
- **2020** — Trio forms (Jeffrey + Alex Freundlich + Camille Klein) during the pandemic in a cabin in Ashland, Oregon.
- **2020–2023** — TikTok growth to ~2.7M followers. No paid promotion. Viral hits with 4-stroke / 4-sung-line compositions.
- **2021** — Rhizome commissions *The Longest Whistlegraph Ever (so far)*.
- **2022-05** — *Ten Whistlegraphs* at Feral File; *The Longest Whistlegraph Ever (so far)* debuts at New Museum as part of Rhizome's *First Look*.
- **2023-06-06** — *The Whistlegraph Zine* published (Sex Magazine / Asher Penn), 60pp, 750 copies. Ciocci essay.
- **2023-11** — Trio separates. Jeffrey continues solo.
- **2024–2025** — Form continues under aesthetic.computer: `/whistlegraph` piece, self-documenting-score as AC's founding principle.
- **2026-04-20** — Are.na Annual Vol. 8 pitch submitted (essay: *Whistlegraph and the Self-Teaching Score*; book Dec 2026).

---

## 6. Local off-repo caches

Material on @jeffrey's laptop outside the repo, Dropbox, and Spaces. Useful for analysis without round-trips.

### iCloud Drive (`~/Library/Mobile Documents/com~apple~CloudDocs/`)
- `Downloads/whistlegraph-2023.1.30.02.30.20.mp4` — performance capture (Jan 30 2023)
- `Downloads/whistlegraph-2023.1.30.02.40.41.mp4` — performance capture (Jan 30 2023)
- `Downloads/whistlegraph-2023.2.04.15.35.23.mp4` — performance capture (Feb 4 2023)
- `Downloads/whistlegraph-2023.2.12.13.55.10.mp4` — performance capture (Feb 12 2023)
- `Audio Files/Noise_Shoegaze Whistlegraph.aif` — composition draft
- `Audio Files/Noise_Shoegaze Whistlegraph v3.aif` — composition draft (rev3)
- `Audio Files/Whistlegraph Practice.aif` — practice take
- `Adam's Beats/4 2 whistlegraph 80 g .wav` — collaboration with Adam Schwarz (80 BPM, key g)
- `2021-03-21-Whistlegraph Retainer Agreement.pdf` — retainer (LLC adjacent)

---

## 7. Analysis targets — what to pull, align, and structure

Goal: feed a *structural* understanding of the whistlegraph form (stroke ↔ syllable ↔ time mapping). What we have so far is unaligned. To get to a learnable grammar, the corpus needs **score-file ↔ video pairs**.

### Highest-leverage pulls (sized against the manifest)

Sized "analysis core" subset is **~17.7 GB** — Dropbox-only:

| Priority | Path | Size | Why |
|---|---|---:|---|
| 1 | `Whistlegraph/Scores/` | 51 MB | Rev1 PDFs match per-piece names from the paper figures + DO Spaces dirs. Clean vector source. |
| 2 | `Whistlegraph/iPad Procreate Backups/` | 6.4 GB | 33 `.procreate` bundles with per-stroke timing replay. Skips audio-to-stroke alignment entirely. |
| 3 | `Whistlegraph/Research/Whistlegraphs for GAN/` | 217 MB | Already curated by jeffrey for ML. Re-use, don't re-curate. |
| 4 | `Whistlegraph/TikTok/` | 10.94 GB | 233 files, primary performance corpus. Score-as-video. Includes `Collabs/bo en/` Bo En collaboration takes. |
| 5 | `Whistlegraph/Manuscripts/` | 86 MB | Per-piece manuscript scans (slinky dog visible). |
| 6 | `Whistlegraph/Pixaki Updates/` | 280 KB | Cheap; frame-by-frame source. |
| 7 | `Whistlegraph/Audio Drafts/` | 375 KB | Single Camille take. |

Already in hand (DO Spaces, locally synced 2026-05-02): 10 piece dirs with score PNG + tt-compilation MP4 + web MP4 + webp = 1.0 GB. **No further pairing work needed for those 10.**

### External pulls (web, not Dropbox)
- **TikTok profile crawl** (`yt-dlp` against `@whistlegraph` URL or CDP against the creator dashboard) — supplements the Dropbox `TikTok/` archive with anything missing or post-archive. The Dropbox copy is likely the canonical source given the upload patterns.
- **YouTube channel pull** (`yt-dlp` against channel URL) — Dropbox `YouTube/` has 43 GB of source files; the public channel has the encoded uploads. Pull whichever is easier per clip.
- **Are.na `self-teaching-scores`** — 68 annotated blocks · already public · scrape via Are.na API for descriptions.

### Heavy / situational pulls (defer until analyzer is working)
- `Whistlegraph/YouTube/` (43.48 GB) — long-form, lower density per byte
- `Whistlegraph/Music/` (23.84 GB) — Goodiepal sessions, collaborative composition context
- `Whistlegraph/Live/` (11.45 GB) — streaming overlay assets, less score-density
- `Whistlegraph/Exhibitions/` (253.93 GB) — Rhizome microsite media; institutional context, not score corpus
- `Whistlegraph/Media from Fans/` (635 MB) + `Media from Friends/` (13 GB) — UGC; analyzable as "responses to" the form, separate corpus

### Pairing strategy (the "hedge")
- **Score ↔ video pairing** is what makes the corpus analyzable. Each named piece (e.g. `butterfly-cosplayer`, `loner`, `puzzle`) has a paper figure, a per-piece DO Spaces dir, *and* a TikTok upload. Cross-reference filename / title / hashtag to build the join key.
- **Procreate replays** are the unfair advantage when present — per-stroke timestamps without alignment work.
- **Audio-to-stroke alignment** for un-replayed pieces: vocal-onset detection (librosa/whisper) + frame-diff on the video; align local maxima within a beat-grid tolerance.

### Are.na as ground truth
The [`self-teaching-scores`](https://www.are.na/aesthetic-computer/self-teaching-scores) channel has 68 annotated blocks — human commentary per piece. Block descriptions are a free training signal for the "describe" step.

### Out of scope for the analysis corpus
- LLC / Taxes / Invoices / Signed Documents / Hand Health folders — preserve in place, exclude from analyzer.

---

## How to extend this index

1. When new material appears (a recording gets indexed, a press mention shows up, a new piece ships) — add a line here with a link or path.
2. Use `TBD:` prefix for known-missing items so they're greppable.
3. Don't check in large binaries; keep this as pointers.
4. If the dropbox catalog grows large, split it into a sibling `dropbox-manifest.md` (still text, still pointers).
