# SIGGRAPH Asia 2026 Technical Papers — Conversion Notes

Target: **Technical Papers (full)** track, dual-track conference paper.

## Deadlines (AoE assumed unless stated otherwise — verify)

- **Tue 2026-05-05** — Submission form / abstract deadline
- **Tue 2026-05-12** — Full paper deadline
- **Wed 2026-05-13** — Upload deadline

Submit at https://ssl.linklings.net/conferences/siggraphasia/

## Source

`latency-source.tex` is a verbatim copy of `papers/arxiv-latency/latency.tex`
(2026-04 working draft, "Where the Microseconds Go: Input and Audio Latency
in AC Native OS"). The original is 6pp in custom AC two-column LaTeX.

## What needs to change for SIGGRAPH Asia

### 1. Template
- Switch document class to `\documentclass[acmtog,anonymous,review]{acmart}`
- Drop the custom `ac-paper-layout.sty`, AC fonts (`ywft-processing-*`),
  AC color palette, `draftwatermark`, and the custom `\titleformat`
  blocks — `acmart` provides all of this.
- Keep `listings`, `booktabs`, `tabularx`, `natbib` (acmart includes a
  compatible bibliography stack — verify before duplicating).
- Move from Latin Modern + AC display fonts to acmart's default
  Linux Libertine.

### 2. Page budget
- Limit: **7 pages excluding references and figures-only pages**;
  at most **2 figures-only pages**.
- Source is currently ~6pp arxiv two-column. acmart double-column at
  acmtog spacing is denser than the AC layout — expect ~5–6pp once
  reflowed. There is room to add a related-work section and one more
  figure without going over.

### 3. Anonymization (double-blind, mandatory)
The source is NOT anonymous. Strip / rewrite:
- Title block: `@jeffrey`, ORCID, `https://aesthetic.computer`,
  `pals.pdf` logo
- Abstract: replace "I" with passive / "we"; remove "A letter for Parag"
  subtitle (deanonymizing dedication)
- Body: remove or genericize `\acos{}`, `\ac{}`, "Aesthetic Computer",
  `notepat`, `fedac/native`, GitHub URLs, commit hashes that resolve
  to a public repo. Refer to "the runtime" / "the target OS" / "the
  test piece" instead.
- Acknowledgements: omit at submission, restore for camera-ready.
- Self-citations: cite as third party ("Scudder 2026 reports ...")
  and check that `\cite{}` keys do not leak handles.

### 4. Reframing for the graphics community
SIGGRAPH Asia tech papers expects a graphics or interactive-systems
contribution. The latency paper is HCI-systems leaning. Add or
strengthen:
- A subsection on **frame-pacing and display latency** (Wayland section
  already gestures at this — expand).
- An evaluation that ties input/audio latency to **interactive
  graphics** outcomes (responsiveness in a real-time visual feedback
  loop, not just sound).
- Related work: cite at minimum Jota et al. on touch latency,
  Ng et al. "Designing for Low-Latency Direct-Touch Input,"
  and Casiez et al. on input-to-display latency.

### 5. Supplementary materials
- Up to 500 MB. Allowed: code, data, video (5 min max), comparisons.
- Suggested: a short video of `notepat` running on HDA-direct vs SOF
  hardware showing the audible difference; the latency-test harness
  output; the relevant `fedac/native` source files.

## Build

Once converted, build with the standard SIGGRAPH route:

```bash
cd papers/siggraph-asia-2026-tech
xelatex paper.tex && bibtex paper && xelatex paper.tex && xelatex paper.tex
```

The oven papermill polls `papers/`, so once `paper.tex` exists and
`SCORE.md` lists it, the auto-build pipeline will produce `paper.pdf`.

## Decision point

A 7pp double-blind acmart paper from a 6pp arxiv draft is roughly
**3–5 days of focused writing**. With the May 5 form / May 12 paper
deadlines, the realistic call is:

- Decide by **Mon 2026-05-04** whether to submit.
- If yes: dedicate May 5–11 to the rewrite.
- If no: archive this directory or move it to SIGGRAPH 2027 prep.
