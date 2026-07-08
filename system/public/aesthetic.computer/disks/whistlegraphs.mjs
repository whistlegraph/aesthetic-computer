// Whistlegraphs — the canonical code → piece index for the artform.
//
// Each whistlegraph is given a short, memorizable code. The code is the
// address of the work: type it at the aesthetic.computer prompt (`imab`) to
// perform it, open `whistlegraph.org/<code>` to see its score + video, or
// pass it to the `wg` card player (`wg imab`).
//
// This map is the single source of truth. It is consumed by:
//   - disks/prompt.mjs  → top-level prompt routing (`imab` → `wg~imab`)
//   - disks/wg.mjs       → the card player's internal shortcut lookup
//
// Keep codes lowercase, 3–4 chars, and pronounceable. Add a line when a new
// whistlegraph is indexed.

export const whistlegraphs = {
  imab: "butterfly-cosplayer",
  grow: "time-to-grow",
  idni: "i-dont-need-an-iphone",
  l8ly: "lately-when-i-fly",
  lonr: "loner",
  w0w: "mommy-wow",
  ppl: "people-pleaser",
  sdog: "slinky-dog",
  puzz: "puzzle",
  wiyh: "whats-inside-your-heart",
  m2w2: "music-2-whistlegraph-2",
};

export default whistlegraphs;
