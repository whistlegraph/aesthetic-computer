# Aesthetic Eye

`aesthetic-eye` is the render-first design gate for paper diagrams. A successful
TeX build is not visual approval. Every diagram must be rendered from the final
PDF, inspected by visual inference, and assigned the literal verdict
`design: pass` or `design: fail` in an `aesthetic-eye.json` beside the paper.

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
# Open .aesthetic-eye/diagrams-contact.png and every diagram-*.png.
# Record the visual-inference verdicts and the printed pdfSha256.
node papers/aesthetic-eye.mjs check papers/arxiv-example
```

The check fails when a diagram is missing, any verdict is absent, any diagram is
`design: fail`, a passing diagram has a failing subcheck, or the PDF changed
after review. `.aesthetic-eye/` is disposable rendered evidence; the manifest is
the durable review record.
