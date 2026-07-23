# Fuser Internal Paper Style

Use this profile for Fuser-facing papers in the shared paper stack. It extends
`Figure-Table-QA-Check`; it does not replace that acceptance gate.

## Identity source

- Use the production Fuser lockup from
  `/Users/jas/Developer/fuser/apps/app/public/fuser-thumbnail-logo.svg`.
- Use the icon path from that same SVG when a mark-only treatment is needed.
  `captutor/bin/captutor-wallpaper.swift` is the reference implementation: it
  extracts the production path verbatim and never invents a replacement mark.
- Never ask an image model to typeset or reconstruct the Fuser wordmark. For a
  generated cover, pass the production logo as an image input for visual
  conditioning, then layer the exact SVG or a deterministic rasterization into
  the document.

## Cover

- The lead illustration is Figure 1 and visibly leads page 1.
- Prefer one strong cover field over a small decorative image below a title.
- Keep a calm region for title typography and an exact Fuser lockup.
- Use the Captutor stage as a tonal reference: near-black or off-white ground,
  sparse modular marks, restrained motion/connection language, and no generic
  cloud, robot, or fantasy-server imagery.
- Generated covers must be checked both as standalone images and in the final
  page crop. A good source image can still fail as a cover.

## Color and type

- Base: near-black `#171717`; paper white `#FAFAFA`.
- Brand energy: violet around `#662DD6`, electric blue around `#4259FF`, and
  cyan/mint around `#00ADBB` / `#009970`.
- Reserve magenta/red for pressure, failure, or risk; reserve amber for warning.
- Use semantic color in tables. The same color means the same category or
  verdict throughout one paper.
- Headers and section labels may use a clean sans face. Long-form body copy must
  remain quiet and highly readable. No data-bearing text below footnote size.

## Tables and figures

- Default to thin violet outlines, a dark violet header, light semantic row
  tints, aligned values, and concise cell copy.
- Avoid decorative fills, avoidable wrapping, cramped columns, and ambiguous
  legends.
- Architecture diagrams should choose a direction that survives the final
  column width. If a wide float would separate a diagram from its heading or
  reorder the argument, redesign it as an in-flow vertical figure.
- Telemetry, code, query, and reproduction cards count as figures and receive
  the same font-size and visual-inference checks.

## Acceptance

Run `paper_figure_table_qa_check` after the final build. Inspect the all-page
overview, then every page containing a figure, table, diagram, or embedded card
at full resolution. The check must explicitly verify:

- exact-logo fidelity and cover crop;
- title/logo contrast and minimum reading size;
- figure and table order relative to the prose;
- all table wrapping, cell rules, semantic colors, and numeric alignment;
- diagram flow, arrow direction, label collision, and caption order;
- code/query-card type at normal PDF reading size.

