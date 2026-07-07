// Tree on blacktop: Seedance pinned between the two validated stills, with
// SINGING (sung words, whistlegraph-style) and chalk-color swaps choreographed.
import { generateShot } from "/Users/jas/aesthetic-computer/pop/lib/fal-seedance.mjs";

const HERE = "/Users/jas/Desktop/seedance-variations/storyboard";

const PROMPT =
  "A real phone video, top-down, of a hand drawing with sidewalk chalk on " +
  "asphalt blacktop. Starting from the first frame, the hand sets down the " +
  "pink chalk, picks up a BROWN chalk and draws the tree trunk from the last " +
  "frame in thick strokes with two root flares; then swaps to a GREEN chalk " +
  "and scribbles the big round canopy loosely; then takes the PINK chalk back " +
  "and dots exactly three small blossoms, one at a time, ending resting on " +
  "the third. A warm casual human voice SINGS along the whole time, " +
  "whistlegraph-style — singing the words \"a tru-unk\" stretched over the " +
  "trunk strokes, \"and a fluff-y green top\" over the canopy scribble, and " +
  "\"one... two... three!\" with each pink blossom as it is dotted. " +
  "THE ONE RULE: the graphics align with the sounds — each sung syllable " +
  "lands exactly on the stroke being drawn, the chalk scrape on rough asphalt " +
  "is heard only while the chalk touches the ground, silent during chalk " +
  "swaps. Every mark grows only from the chalk tip; no line appears by " +
  "itself; drawn marks never shift or redraw. Natural handheld framing, " +
  "playful unpolished singing like someone singing to themselves.";

const r = await generateShot({
  image: `${HERE}/start-blacktop.png`,
  endImage: `${HERE}/end-tree.png`,
  prompt: PROMPT,
  duration: "10",
  ratio: "9:16",
  resolution: "720p",
  tier: "fast",
  audio: true,
  outPath: `${HERE}/blacktop-tree.mp4`,
  label: "blacktop-tree",
});
console.log(r.ok ? `✓ blacktop-tree: ${(r.bytes / 1e6).toFixed(1)}MB in ${r.seconds.toFixed(0)}s (seed ${r.seed})` : `✗ ${r.error}`);
