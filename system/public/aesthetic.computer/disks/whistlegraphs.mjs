// Whistlegraphs — legacy code → card-player shortcuts.
//
// Each whistlegraph is given a short, memorizable code. The code is the
// address of the work: type it at the aesthetic.computer prompt (`imab`) to
// perform it, open `whistlegraph.org/<code>` to see its score + video, or
// pass it to the `wg` card player (`wg imab`).
//
// The complete, canonical command index is generated at
// whistlegraph.org/api/commands. This small map only covers the hand-authored
// cards still implemented by disks/wg.mjs and doubles as prompt.mjs's offline
// fallback while the live feed loads.
//
// Keep codes lowercase, 3–4 chars, and pronounceable. Add a line when a new
// hand-authored card is added to wg.mjs.

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
