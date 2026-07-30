# Figure-Table-QA-Check

`Figure-Table-QA-Check` is the visual acceptance gate for every paper in the
Aesthetic Computer paper stack. A successful TeX build, a clean log, and a
whole-document thumbnail are necessary but do not constitute a pass.

## What counts

Review every visual surface:

- generated illustrations, photographs, screenshots, plots, and diagrams;
- tables, matrices, scorecards, and comparison grids;
- code, query, telemetry, and reproduction cards;
- framed callouts or other embedded raster/vector panels.

An embedding counts as a figure for this process even when the source does not
use a LaTeX `figure` environment.

## Required loop

1. Build the paper with `paper_build`.
2. Run `paper_figure_table_qa_check` without `page` for an all-page overview.
3. Use OpenAI visual inference to inspect the overview for hierarchy, placement,
   float order, density, column balance, negative space, and unfinished pages.
4. Run the check with `page: N` for every page containing an inventoried figure,
   table, or embedded card, plus every overview page that appears imbalanced or
   unusually empty. Inspect each at full resolution.
5. Reject and revise every visual failure. Rebuild and repeat the overview and
   detail checks until the entire paper passes.
6. Open the accepted PDF with `paper_open`.

The QA tool deliberately reports `VISUAL INFERENCE REQUIRED`; it never
self-certifies a pass.

## Acceptance rubric

### Intent and placement

- A requested lead illustration actually leads the paper.
- Visuals appear near the claims they support, in narrative order.
- Captions and figure/table numbers match references in the prose.
- Floats do not strand headings, reorder evidence, or create an unfinished page.

### Page and column balance

- On a multi-column page, columns end at comparable visual depth unless a lead
  visual, section break, or deliberate closing gesture explains the imbalance.
- Treat a depth difference greater than roughly 10% of the live page height as
  a failure on an ordinary prose page. A label such as “natural ending” is not
  an exception; the exception must be visible in the composition.
- Negative space is intentional composition, not residue from a float queue,
  forced page break, orphaned heading, or nonbreaking table.
- Reject half-empty interior pages, sparse float-only pages, and final pages
  whose content could be consolidated legibly onto the preceding page.
- Full-width tables stay near the section that introduces them and do not split
  a numbered procedure or move evidence after the conclusion it supports.
- Inspect the first and last page, every section transition, and both columns of
  every page; a clean figure/table crop does not excuse a broken page around it.

### Reading size and typography

- All text is legible at normal PDF reading size without exceptional zoom.
- Body, captions, legends, axes, diagram labels, code, and table values have a
  deliberate hierarchy.
- Data-bearing text must not be smaller than the paper's footnote size.
- `\scriptsize` and `\tiny` are not acceptable for figures, tables, or cards.

### Tables

- Use intentional column widths and eliminate avoidable line wrapping.
- Use clean outlined cells and restrained semantic color coding.
- Align numeric values and comparable units consistently.
- Keep headers, rules, padding, and caption placement consistent.
- Reject clipping, broken rules, crowded cells, ambiguous colors, and decorative
  fills that do not encode meaning.

### Figures, diagrams, and embedded cards

- Flow direction and hierarchy are immediately unambiguous.
- Repeated cards, logos, or nodes use equal widths, heights, row baselines, and
  gutters. Inspect the negative space between items, not only the items.
- Peer boxes retain a visible gutter at normal reading size. Borders must not
  touch or nearly touch unless a labeled container intentionally groups them.
- Card atlases and repeated-node diagrams form a complete rectangular cadence.
  Reject a missing-card hole, an uneven final row, arbitrary centering, or a
  large interior gap; add a meaningful item, change the grid, or redesign.
- Parallel branches and peer nodes are visually symmetric unless asymmetry
  encodes a stated difference. Decorative asymmetry never counts as evidence.
- Labels do not collide with nodes, arrows, art, borders, or watermarks.
- Contrast survives print and normal-size screen reading.
- Code/query cards preserve readable type, indentation, and line length.
- Generated imagery is inspected for both content fidelity and page composition.

## House style

Paper tables should default to readable full-width layouts when a single column
would force ugly wrapping. Prefer a dark semantic header, lightly tinted row or
status cells, thin colored outlines, aligned values, and concise cell copy. Color
must explain categories, time windows, status, risk, or recommendation—not merely
decorate the grid.

For Fuser-facing internal documents, also apply
[`FUSER-PAPER-STYLE.md`](FUSER-PAPER-STYLE.md). Its exact-logo, cover-image,
palette, diagram-flow, and brand-fidelity checks are part of visual acceptance,
not optional decoration.
