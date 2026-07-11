# whistlegraph.org — open notes

Deferred items from @minanimals (Alex Freundlich) via iMessage, 2026-07-10.
Data edits + copy/loop changes from that thread are already applied to `graphs.json` / `index.html`.

## To design / decide
- **Left-column "cover" colors.** Hand-select the swatch color on the left of each
  title (most archive rows currently share `#b44887`). Alex: pull colors from each
  composition's main video thumbnail. For the featured top 10, consider pulling
  preview thumbnails / "covers" from the Feral File performance videos.
- **Underpainting versions.** Link the two versions of *Underpainting* on the
  Whistlegraph YouTube channel from the `undr` detail page. (Needs a `versions`
  array of embeddable sources — YouTube can't drop straight into the `<video>` tag,
  so this needs either mp4 mirrors or a YouTube-embed branch in `showDetail`.)
- **Archive combing.** Cross-check the full list against the old Notion + website
  archive; fold duplicates, decide what to keep vs. ditch, and pull in any linkable
  extras (lectures, livestreams, old blogs) not already in "Lectures, shows & press."
  Notion seat was repurchased 2026-07-10 to crawl this.
- **CV line-items.** *The Longest Whistlegraph Ever (So Far)* and *Underpainting*
  live mainly as bottom-of-page CV items, but Alex wants them to still carry a
  `[code]` and count toward the ultimate total even if they're out of the main scroll.
  (`long` and `undr` codes already exist — confirm they're counted the way he means.)

## Needs input from Alex
- **Kunstverein Hamburg link.** He asked to add "Kunstverein Hamburg" on the Feral
  File line (Elsewhere) but the URL didn't come through in iMessage ("have the link
  be:" arrived empty). Placeholder currently points at the existing Kunstverein
  Hamburg recital video (YouTube `YXUUCkqv2LY`) — swap in the intended link.
- **"More Than Enough" duplicate.** `wtm` was renamed to *More Than Enough* `[more]`
  by Alex Freundlich per his note. A separate `enuf` *More Than Enough* (by
  Whistlegraph) still exists. Confirm whether these are the same composition and
  should fold, or stay distinct.
