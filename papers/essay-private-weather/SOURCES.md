# Sources and provenance

## Primary paper

- Emily A. Ertle, Michael Levin, and Matthias Scheutz. “Free Lunch? Low-Cost Intelligence Through Pattern-Guided Exploration.” *2025 IEEE International Conference on Development and Learning (ICDL)*, pp. 1–8.
  - Publication record: https://hrilab.tufts.edu/publications/ertleetal25icdl/
  - Canonical PDF: https://hrilab.tufts.edu/publications/ertleetal25icdl.pdf
  - OpenReview record: https://openreview.net/forum?id=xVefsBbG2O
  - Pulled PDF SHA-256: `86f7002fffca975d2c64fc740c62284d7d8796e47b006552c3cc5dbe01fa6630`
  - Accessible text: https://assets.aesthetic.computer/papers/readings/text/Ertle-Levin-Scheutz-Free-Lunch-Pattern-Guided-Exploration-2025.txt

The essay preserves an important limitation in the results: structured fractals and artworks outperform shuffled and uniform-noise sources for the simple mapping agents, but substituting uniform noise for the fractal does not significantly change the mixed-input DQN results. The essay therefore treats an internal pattern as a source of context or behavioral variation, not evidence of latent artistic meaning.

## Aesthetic Computer sources

- `system/public/aesthetic.computer/disks/klpad.lisp` — persistent, independently decaying state coupled to sound and image.
- `system/public/aesthetic.computer/disks/klbutton.lisp` — frame-persistent counter, UTC-locked beat, and touch input.
- `kidlisp-wasm/latent-garden.lisp` — compact oscillator-driven visual behavior.
- `papers/nopaint-3-full-shape/nopaint-3-full-shape.tex` — bounded Jastow search, versioned candidate packets, human acceptance, cached fallback, and the rule that remote compute is optional.
- `papers/nopaint-3-full-shape/references.bib` — Karl Sims (1991) and Hideyuki Takagi (2001) precedents for evolutionary graphics and interactive evolutionary computation.
- `reports/jas-nzxt-fleet-use-report-2026-07-22.tex` — Jastow hardware and operating constraints.
- `marketing/podcast/the-machine-that-came-back.md` — current fleet role: a bounded CUDA worker that returns accepted artifacts and does not become public infrastructure.

## Machine state consulted

The fleet record was checked on 2026-08-05. It identified Jastow as an offline Linux GPU worker with CUDA, Docker, and render capabilities, last seen on 2026-07-25. The essay does not depend on the box being online and makes offline operation an explicit design constraint.

## No Paint 3 design decision

The user clarified on 2026-08-05 that a No Paint 3 painting is an Aesthetic Computer piece, and that No/Paint judges code and pixels together. The essay treats the durable painting as an ordered layer score. Each accepted layer keeps executable source or an explicit raster-layer adapter alongside its rendered pixel result; No discards both candidate forms, while Paint commits both atomically.

## Authorship and lane

- Lane: `/papers` `essay-*`
- Author: `@jeffrey`
- Date: August 2026
- Voice and layout references: `papers/VOICE.md`, `papers/ac-paper-essay.sty`, and `papers/essay-may-26/may-26.tex`
