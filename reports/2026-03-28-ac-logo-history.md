# AC Logo History

Working report prepared from repo history and local Claude session artifacts on 2026-03-28.

## Scope

This report traces the available evidence around the Aesthetic.Computer "Pals" logo. It does not claim a complete provenance chain for the exact current vector asset in `system/public/purple-pals.svg`. The goal is to separate what is clearly supported by evidence from what is still only plausible.

## Already Done

- The Goodiepal paper no longer describes the Pals mark as "a pair of eyes." It now describes the logo as "two hand-drawn figures holding hands" in both:
  - `papers/arxiv-goodiepal/goodiepal.tex`
  - `papers/arxiv-goodiepal/goodiepal-cards.tex`
- A previous Claude session drafted an inline logo-history writeup, but it was never saved as a repo file:
  - `/home/me/.claude/projects/-workspaces-aesthetic-computer/802d983d-d234-410c-b7b5-e4dad02f5335.jsonl`

## Confirmed Evidence

### Email and calendar artifacts

- 2022-09-14: email subject `my invoice` from Niki Stebbins.
- 2022-09-19: email subject `Niki's invoice #1`.
- 2023-02-01: email subject `Niki's Invoice #2`.
- 2023-07-19: calendar hit `logos w/ niki`.
- 2023-08-01: calendar hit `niki logo update`.
- 2023-09-09: email subject `!c3 cr34m l0go$` from Niki Stebbins with 18 JPG attachments and body text `ice cream logos 8)`.
- 2025-05-11 to 2025-05-13: Casey Rubber Stamps thread around `Aesthetic Computer Pals Logo.pdf`.

These artifacts live in the March 28 Claude session and its saved tool results:

- `/home/me/.claude/projects/-workspaces-aesthetic-computer/802d983d-d234-410c-b7b5-e4dad02f5335.jsonl`
- `/home/me/.claude/projects/-workspaces-aesthetic-computer/802d983d-d234-410c-b7b5-e4dad02f5335/tool-results/`

### Repo history

- `5bcefc5a9` - `better defaults in \`wgr\` for niki's logo efforts`
- `a0009f5d0` - `add new vscode extension logo from niki`
- `0aba1a378` - `add dynamic logo generator / enable real print orders in production / clear query strings after loading a disk`
- `f1a4f272c` - `` `logo` -> `pals.aesthetic` ``
- `12e348035` - `` `pals.aesthetic.computer` url fix; printing label typo; faux-progress on upload ``
- `bf29eadf8` - `host purple-pals graphic for docs`
- `9dc1704ac` - `refactor: rename score to "Score for Aesthetic.Computer & Pals"`
- `2d7d1c58e` - `feat: add demoplay conductor, pals logo to papers, auto theme, Ars Electronica prep`

### Current repo context

- Current score title:
  - `SCORE.md`
- Current static public asset:
  - `system/public/purple-pals.svg`
- Current redirect for `pals.aesthetic.computer`:
  - `system/netlify.toml`
- Current logo service implementation:
  - `system/backend/logo.mjs`
- Backlog note connecting the ice-cream idea to the logo service:
  - `TODO.txt`

## Strongest Supported Timeline

### 2022-09 to 2023-02

Niki Stebbins is definitely doing paid work for AC by this period. The evidence is the invoice thread: `my invoice`, `Niki's invoice #1`, and `Niki's Invoice #2`.

### 2023-02-01

Commit `5bcefc5a9` is the earliest strong repo signal directly linking Niki to logo work: `better defaults in \`wgr\` for niki's logo efforts`.

### 2023-07-19 to 2023-08-01

The mail/calendar artifacts show explicit logo meetings:

- `logos w/ niki`
- `niki logo update`

Commit `a0009f5d0` on the repo side gives the clearest shipped-asset evidence from this period: `add new vscode extension logo from niki`.

### 2023-09-05 to 2023-09-13

AC adds a dynamic logo endpoint and then renames the public route to `pals.aesthetic.computer` through:

- `0aba1a378`
- `f1a4f272c`
- `12e348035`

Important caveat: this appears to be a rotating or generated logo/painting service, not proof of the exact later static `purple-pals.svg` asset.

### 2023-09-09

The `!c3 cr34m l0go$` email shows Niki exploring ice-cream-based logo directions. That idea still echoes in `TODO.txt`:

- `Add ice cream pals to \`pals.aesthetic.computer.\``

### 2024-01-31

Commit `bf29eadf8` is the cleanest first appearance of the current static vector asset in the repo:

- `host purple-pals graphic for docs`

This is the strongest date for "the current in-repo canonical vector exists," but not yet proof that Niki authored this exact final SVG.

### 2025-05-11 to 2025-05-13

The logo is clearly mature enough for physical production. The Casey Rubber Stamps thread references `Aesthetic Computer Pals Logo.pdf` and requests a production-ready file.

### 2026-02-16 onward

The repo and papers start explicitly centering "Pals" as platform identity:

- `9dc1704ac` renames the score to `Score for Aesthetic.Computer & Pals`
- `2d7d1c58e` and later paper commits bring the Pals logo into the paper stack
- 2026-03-28: the paper wording is corrected to describe the mark as two hand-drawn figures holding hands

## What Is Not Yet Proven

- That Niki created the exact current `system/public/purple-pals.svg` without later modification.
- That the 2023 dynamic `pals.aesthetic.computer` logo service and the later static `purple-pals.svg` are the same artifact lineage.
- That the ice-cream explorations directly became the final static Pals mark.

## Best Current Reading

The safest claim is:

Niki Stebbins was materially involved in AC logo development by 2022-2023, participated in explicit logo meetings in July and August 2023, and is directly credited in repo history for at least one shipped logo asset (`add new vscode extension logo from niki`). The current static Pals vector appears in the repo on 2024-01-31, but the exact authorship chain for that final SVG is not yet fully proven by the evidence gathered so far.

## Next Sources To Read

If this needs to become a publication-grade provenance report, the next sources should be:

- the early `stamp 4 u` mail thread
- any original Figma comments or export history tied to the logo work
- the unsaved inline draft in the March 28 Claude session
- git blame and asset diffs around `system/public/purple-pals.svg`

Until then, this report should be treated as a careful working history rather than a final attribution statement.
