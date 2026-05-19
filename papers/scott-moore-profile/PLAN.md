---
name: scott-moore-profile plan
description: confidential person-scoped research profile on Scott Moore (Gitcoin / Public Works) — open-source + creative-tech infrastructure funding, read through one node
type: project
---

# Profile · scott-moore-profile

**Title**: *Scott Moore — A Profile*
**Canonical artifact**: `scott-moore-profile.tex` → `scott-moore-profile.pdf`
(full-page US-letter, single column, formal academic register).
Copy delivered to `~/Desktop/scott-moore-profile.pdf`.
`scott-moore-cards.tex` is a superseded earlier draft (cards format,
lowercase voice) — kept for reference only; do not treat as current.

**Status**: confidential internal research. **Not** for publication: no
SCORE.md row, no `sync-platter.mjs`, no deploy. `CONFIDENTIAL` cover banner +
watermark on every page.

**Register** (per @jeffrey, 2026-05-18): formal / neutral academic,
proper capitalization and punctuation, narrative prose. No all-lowercase
cards voice. No "this is a Profile not a dossier" meta-framing. No folk
refrain / song. Numbered `[n]` citations → Sources section.

**Occasion**: private (a friend-of-a-friend money question). Deliberately
**excluded from the document** — an unverified claim about a named living
person has no place in it. Recorded here only as motive.

## Verified this pass (primary / multi-source)

- **Identity triangulated 3 ways**: NEW INC person page, Gray Area community
  entry, and SEC Form D all name the same Scott Moore (Public Works founder).
- **NEW INC (New Museum)**: listed as **Mentor**, *Office Hours Mentors*,
  **Year Twelve**; page updated Sept 2025 (current). Connects subject into
  the existing `arxiv-new-inc` dossier corpus.
- **Gray Area**: **board member** / active advisor (San Francisco creative-
  coding nonprofit).
- **Public Works LP** — SEC EDGAR CIK 0001998151. Cayman Islands LP, formed
  2023; single Form D filed 2023-10-25 (accn 0001998151-23-000001). Pooled
  investment fund / private-equity type; Reg D 506(b) + 3(c)(1). Directors:
  **Scott Moore, Danielle Faul, Jacobus Pietersen**. Offering amount and
  remaining = "Indefinite"; amount sold = \$0; first sale "yet to occur."
  Never amended.
- Public Works mission (Gray Area + NEW INC verbatim): "open source ecosystem
  fund focused on supporting **artists and creative technologists**";
  portfolio incl. Fuser, Gitcoin, Metalabel, Spline.

## Tooling notes (reusable)

- JS-rendered pages WebFetch returns empty (e.g. newmuseum.org) →
  headless Chrome works: `"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new --virtual-time-budget=12000 --dump-dom <url>`.
- SEC EDGAR blocks generic fetchers (403). `curl` with a declared
  `User-Agent: <name> <email>` works against `efts.sec.gov` (full-text JSON),
  `www.sec.gov/cgi-bin/browse-edgar` (company browse, `output=atom`), and
  `data.sec.gov/submissions/CIK##########.json`.
- `ac-paper-cards.sty` needs `Droid Sans Fallback`; installed
  `~/Library/Fonts/DroidSansFallbackFull.ttf` (Apache-2.0) — fixes all cards
  builds, not just this one.

## Rev 4 additions (2026-05-18)

- **Portfolio reverse-map**: trackers (crypto-fundraising.info, DropsTab)
  name "Public Works" in exactly one round — **Rise Labs seed, ≈\$3.2M,
  Sept 2024**, syndicate incl. **Vitalik Buterin** (closes the lineage loop).
  All other deals are *Scott-Moore-individual* angel activity, not separable
  by those sources from the fund — kept indicative only.
- **publicworks.fm** is a single mission page (no portfolio/team/disclosure);
  Wayback holds only `/` and `/mutualism`, earliest Feb 2023.
- **CIMA**: `cima.ky/search-entities-cima` results table is gated behind a
  client-side call; not retrievable via curl OR headless Chrome. Recorded as
  **not retrieved, not a negative finding** — open manual check.
- **Transcripts** (saved in `transcripts/`): Blockchain Socialist is the
  substantive one. Moore states the model = invest + surplus redistributed
  to community + "exit to community"; **no fund size / LP / check size on
  record**. Opacity now confirmed across SEC + web + his own interviews =
  structural, not a search gap.

## Rev 5 — admin apparatus resolved (2026-05-18)

- **Registered office / corporate services = Campbells** (Campbells
  Corporate Services Ltd; "Floor 4, Willow House, Cricket Square" is their
  address — confirmed via Campbells contact page + ICIJ Offshore Leaks).
- **Both non-principal directors are Hash independent directors** (Cayman
  crypto-governance firm): **Danielle Faul** (CA(SA); Hash since 2022; BACI
  treasurer; Women in Blockchain Cayman) and **"Cobus" Pietersen** (Hash;
  ex-Walkers/MUFG/UBS). Faul sits on the Coinbase Multi-Strategy feeder *as a
  Hash independent director*, NOT as Coinbase staff — earlier rev-4 wording
  corrected.
- **Fund administrator proper (NAV/TA) = not in any public filing.** Lives
  only in the CIMA register, which is **reCAPTCHA-gated** (human-only by
  design; not bypassed — recorded as a manual check). This is the precise
  reason, supersedes the rev-4 "tooling failure" wording.

## Final state (2026-05-18)

- **Deliverable to Celine** = `~/Desktop/scott-moore-profile.pdf` = the
  12-card readable brief (`scott-moore-cards.tex`). Plain English, cited,
  shareable framing ("Private brief — prepared for Celine; not for
  publication"); WORKING DRAFT tiling cleared via \ClearShipoutPicture.
- Full-page detailed version (`scott-moore-profile.tex`, rev 6, 6pp) kept
  in-repo only as the long record.
- **CIMA confirmed** (human captcha, by ): Public Works LP is an
  ACTIVE registered **Private Fund**, ref **2051840**, effective
  **22-Jun-2023** — predates the US Form D by ~4 months; auditor lives in
  the non-public annual return (Cayman law), not the public register.
- Not published: no SCORE row, no platter, no deploy.
