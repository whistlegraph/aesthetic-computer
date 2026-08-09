# Embargoed public page — draft

Rewrite of `system/public/culture-hub-2026/index.html` for the accepted two-work
program. **Not live.** The published page still shows the May 2026 submitted
proposal, which is correct until CultureHub announces.

## What changed from the live page

- Title, masthead, and meta rewritten for *Whistlegraph presents* and the two works
- New **The Program** section — *Special Sign*, *MacNeoPolitan*, and why they pair
- Status moved from "submitted · awaiting decision" to "in residence"
- The submitted-proposal section is **kept intact** as the public record, relabeled
  as history rather than the current plan
- `header.png` is a symlink to the live copy; resolve it before promoting

## Promoting it

Only after CultureHub's formal resident announcement (expected late August /
early September 2026):

1. Delete the `⛔ EMBARGOED DRAFT` banner and the HTML comment above it.
2. Swap the hero illustration for the two-work image once `POSTER.md` is done,
   and update the figcaption and alt text.
3. Confirm the September dates with CultureHub, then remove the "provisional"
   hedges in the Program section.
4. Replace `header.png` symlink with the real asset.
5. Copy to `system/public/culture-hub-2026/index.html`, commit, and run
   `fish lith/deploy.fish` — pushing alone does not put it in production.

## Preview

```bash
open index.html
```
