# Aesthetic Eye

`aesthetic-eye` is the render-first design gate for papers and their diagrams. A
successful TeX build is not visual approval. The final PDF must receive a
paper-wide brand verdict, and every diagram must receive the literal verdict
`design: pass` or `design: fail` in an `aesthetic-eye.json` beside the paper.

## Brand rule

Every visible project-name wordmark is `Aesthetic.Computer`. The period is
mandatory and uses AC pink, `#B44887`; a visible `Aesthetic Computer` fails the
gate. This rule applies to titles, bylines, prose, captions, tables, diagrams,
and references.

Use one macro throughout LaTeX source, including bibliography fields:

```tex
\definecolor{acpink}{RGB}{180,72,135}
\newcommand{\acdot}{{\color{acpink}.}}
\newcommand{\ac}{Aesthetic{\acdot}Computer}
```

The checker extracts PDF text and fails automatically when the period is
missing. Because text extraction cannot verify color, `prepare` also renders
`pages-contact.png`; visual inference must confirm the dot color in the
manifest's `brand` block.

## The six checks

Every diagram receives all six checks. `design: pass` is valid only when all six
pass.

- `tangents` — labels, arrows, rules, nodes, and captions do not almost-touch,
  collide, or create accidental continuities. Edge labels have deliberate air.
- `type` — text is legible at the PDF's normal reading size, contrast is
  sufficient, labels do not rely on color alone, and wording is concise.
- `balance` — the visual center agrees with the available frame; rows, branches,
  and annotations do not pull the figure accidentally left, right, up, or down.
- `spaceUse` — density is intentional. Content neither rattles inside a wasteful
  field nor crowds the available width merely because space exists.
- `hierarchy` — the reading order, primary path, secondary path, and explanatory
  note are visually distinct without decorative noise.
- `edgeRouting` — connections have unambiguous direction, adequate separation,
  consistent routing grammar, and no avoidable crossings or misleading arrows.

These are visual judgments, not geometry-only lint. The agent must open and
inspect the prepared crops. A manifest copied from an older render fails because
it records the SHA-256 of the reviewed PDF.

## Manifest

Place `aesthetic-eye.json` beside the paper source:

```json
{
  "schema": 1,
  "paper": "Example Paper",
  "pdf": "example.pdf",
  "expectedDiagrams": 1,
  "visualInference": true,
  "pdfSha256": "sha256-of-reviewed-pdf",
  "reviewedAt": "2026-07-20T23:00:00Z",
  "reviewer": { "kind": "visual-inference", "agent": "Codex" },
  "brand": {
    "canonicalName": "Aesthetic.Computer",
    "dotColor": "#B44887",
    "design": "pass",
    "checks": {
      "period": "pass",
      "dotColor": "pass"
    }
  },
  "diagrams": [
    {
      "id": "system-map",
      "page": 2,
      "crop": [0.08, 0.12, 0.84, 0.28],
      "design": "pass",
      "checks": {
        "tangents": "pass",
        "type": "pass",
        "balance": "pass",
        "spaceUse": "pass",
        "hierarchy": "pass",
        "edgeRouting": "pass"
      },
      "notes": "Centered two-row flow; labels remain clear at normal zoom."
    }
  ]
}
```

`crop` is `[x, y, width, height]` in normalized page coordinates, measured from
the upper-left. `expectedDiagrams` is the explicit inventory: every diagram in
the paper must appear once, including title illustrations that explain the
system rather than merely decorate it.

## Pass

```bash
node papers/aesthetic-eye.mjs prepare papers/arxiv-example
# Open .aesthetic-eye/pages-contact.png, diagrams-contact.png, and every crop.
# Record the visual-inference verdicts and the printed pdfSha256.
node papers/aesthetic-eye.mjs check papers/arxiv-example
```

The check fails when a visible brand name omits its period, the brand review is
absent, the dot-color check fails, a diagram is missing, a verdict is absent, a
diagram fails, or the PDF changed after review. `.aesthetic-eye/` is disposable
rendered evidence; the manifest is the durable review record.
