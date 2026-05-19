# SIGGRAPH Asia 2026 Technical Papers — Conversion Notes

Target: **Technical Papers (full)** track, dual-track conference paper.

## 2026-05-07 working call

This should be treated as the soonest-deadline paper **only if the May 5
submission form was already filed**. If the form was not filed, stop spending
new writing energy here and pivot to ICCC Early Career (May 15), ArtsIT (June
1), or SIGGRAPH Asia Art Papers (June 8).

If the form exists, the rewrite should get simpler, not bigger. The paper is
not "everything about AC Native OS." It is one claim:

> Low-latency creative systems get fast when buffer turnarounds become visible
> enough to remove.

Everything else should serve that claim. Keep the browser-to-native jump, the
Wayland/DRM display path, the ALSA/SOF hardware split, and the CoreAudio/SDL
A/B result. Cut or compress long commit archaeology unless it proves one of
those four turns.

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

### 0. Simplify the idea

The current source is vivid but overloaded. For the SIGGRAPH version, use this
spine:

1. **Problem:** interactive graphics and musical pieces feel wrong when input,
   display, and audio are buffered by separate layers.
2. **Method:** instrument one real creative runtime and follow a single
   event from keypress to visible/audio response.
3. **Result:** the largest wins came from removing buffer turnarounds:
   browser to native, Wayland/Cage to DRM-direct, SDL stream to CoreAudio
   direct, and HDA-direct instead of SOF-constrained audio.
4. **Lesson:** the hard part is not shaving microseconds. The hard part is
   finding which layer secretly owns the next frame or audio period.

This lets the paper stay technical without dragging the reader through every
commit. The commit history becomes evidence, not the plot.

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
- A subsection on **frame-pacing and display latency**. Do not make it a
  Wayland explainer. Make it about who owns the next presented frame.
- An evaluation that ties input/audio latency to **interactive graphics**
  outcomes: keypress to visible response, keypress to audible response, and
  whether the two stay locked together under load.
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

As of **Thu 2026-05-07**, the form deadline has passed. The realistic call is:

- If the May 5 form was **not** filed: mark this track missed for 2026 and
  reuse the simplified argument for June venues.
- If the May 5 form **was** filed: spend May 7-11 only on the reduced spine
  above, then upload by May 13.
- Do not add a new grand theory. This submission wins, if it wins, as a clear
  systems paper about buffer ownership in creative runtimes.
