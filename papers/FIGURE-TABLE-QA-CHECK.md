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
   float order, density, and unfinished pages.
4. Run the check with `page: N` for every page containing an inventoried figure,
   table, or embedded card. Inspect each at full resolution.
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
