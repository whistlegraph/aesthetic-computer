# Cover sources — Eat the Right Answer

Edit of the house title-page illustration lineage (seated @jeffrey on the wooden chair, colored pencil on white paper), generated with the `illy` MCP (OpenAI gpt-image-2, edit mode, 2026-09-19) against one reference:

- `../arxiv-open-schools/figures/cover.png` — the *Get Closed Source Out of Schools* cover (seated figure with the chained Chromebook). Supplies the figure, outfit (yellow fountain pen, bear badge), chair, and drawing style.

Visual elements and what they stand for:

- **A small open laptop on his knees whose screen is a grid of colored tiles** — the nom board (`system/public/aesthetic.computer/lib/nom.mjs`, a 5×5 grid; the drawing shows the grid without numbers or letters by design, since the paper's point is that the content is a table that can be swapped).
- **The green wide-mouthed creature on the lid** — the muncher, descended from MECC's *Number Munchers* (1986); the answer is the thing you eat.
- **The purple horned figurine on the floor** — the troggle, the patrolling wrong-move in the same games.
- **The cross-legged, leaned-in posture** — the computer-lab kid the paper keeps returning to (Section 1).

No logos, wordmarks, or text appear in the image by design.

## Review

The cover is a title-block plate (`\coverplate`), not a `figure` environment, so the Aesthetic Eye checker does not inventory it as an evidence image. It was inspected at full resolution: the seated figure is complete and uncropped, the laptop, creature, and figurine all read at title-block size, the tile grid carries no text, and no wordmark appears. The generated grid is 4×5 rather than 5×5; the caption-free plate makes no numerical claim, and the source log records the difference.
