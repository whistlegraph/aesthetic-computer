# Cover sources — Free Versus Nonfree

Edit of the house title-page illustration lineage (seated @jeffrey on the wooden chair, colored pencil on white paper), generated with the `illy` MCP (OpenAI gpt-image-2, edit mode) against one reference:

- `../arxiv-open-schools/figures/cover.png` — the *Get Closed Source Out of Schools* cover (seated figure with the chained Chromebook), itself derived from the `arxiv-pieces` cover lineage and the photo references logged there. Supplies the figure, outfit (yellow fountain pen, bear badge), chair, and drawing style.

Visual elements and what they stand for:

- **Pink dashed barrier tape across the floor** — the one line Stallman treats as ethical: free above, nonfree below (Figure 1 of the paper; `gnu.org/philosophy/categories.html`).
- **Five colored floppy disks, loose and uncovered, to the right of the line's far end** — the free tiers (public domain, copyleft, lax) in the AC palette; software anyone may pick up.
- **One grey disk in a padlocked cage, to the left** — proprietary software: readable through the bars, not usable (`gnu.org/philosophy/open-source-misses-the-point.html`, "source available").
- **The man laying the tape himself** — the paper's closing point: the placement is a choice the project has to make on purpose.

No logos, wordmarks, or text appear in the image by design.

## Review

The cover is a title-block plate (`\coverplate`), not a `figure` environment, so the Aesthetic Eye checker does not inventory it as an evidence image and the manifest declares `expectedFigures: 0`. It was still inspected at full resolution on page 1 of the built PDF: the pink barrier tape, the caged grey disk, and the five loose colored disks all read at title-block size, the seated figure is complete and uncropped inside the plate, and no text, wordmark, or logo appears in the drawing.
